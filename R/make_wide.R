#' Convert Panel Data from Long to Wide Format
#'
#' This function reshapes panel data from long format to wide format,
#' creating separate columns for each time period for a selected set of
#' time‑varying variables. All other variables (except the entity and time
#' identifiers) are treated as time‑invariant and are verified as such.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param select A character vector specifying the names of the time‑varying
#'        variables to reshape into wide format. This argument has no default
#'        and must be provided explicitly.
#' @param index A character vector of length 2 specifying the names of the
#'        entity and time variables.
#'        If not specified and data is a `panel_data` object, the entity and time values
#'        will be extracted from the data.frame attributes.
#' @param spacer A character string to insert between variable names and time
#'        values in the wide format column names. Default = "_".
#' @param invert A logical flag indicating whether to put time values before
#'        variable names in column names. If `FALSE`, column names are
#'        `"variable_spacer_time"`; if `TRUE`, they are `"time_spacer_variable"`.
#'        Default = FALSE.
#'
#' @return A data.frame containing panel data in a wide format, with class
#'         `"panel_wide"` and the attributes `metadata` and `details`.
#'
#' @details
#' The structure of the returned data.frame depends on the input. For example,
#' suppose your long panel data contains an entity column `id`, a time column
#' `year`, and you select time‑varying variables `y` and `x`. If there are two
#' time periods (e.g., 2000 and 2001), the resulting wide data.frame will have
#' a single row per entity, with columns `id`, `y_2000`, `y_2001`, `x_2000`,
#' and `x_2001`. Any other columns in the data (e.g., `industry`, `region`)
#' are treated as time‑invariant and appear as single columns, replicated once
#' per entity, provided they are indeed constant over time for each entity.
#'
#' The reshaped columns are ordered as follows: the entity column appears first,
#' then any time‑invariant variables (in the order they appear in the input data),
#' and finally the selected time‑varying variables, each expanded by time period.
#' Time periods are ordered by their natural order (as given by unique values of
#' the time variable). If an entity is missing a particular time period, the
#' corresponding wide column will contain `NA`.
#'
#' The function checks that:
#' \itemize{
#'   \item All variables in `select` exist in `data`.
#'   \item `select` does not contain the entity or time variables.
#'   \item At least one variable is selected for reshaping.
#'   \item All variables not in `select` or the index are time‑invariant;
#'         if any are not, the function stops and lists them.
#' }
#'
#' The returned object has class `"panel_wide"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.
#'         If the input was a `panel_data` object, the original metadata elements
#'         (entity, time, and delta) are preserved.}
#'   \item{`details`}{List containing the names of reshaped (`select`) and static
#'         variables.}
#' }
#'
#' @note
#' The input long data must have exactly one row per entity–time combination;
#' if duplicates are detected, the function stops with an error listing the
#' offending pairs. Rows with missing values in the entity or time variables are
#' automatically removed (with a message) before reshaping.
#'
#' The reshaping preserves standard atomic types and factors, but complex S3
#' classes like `POSIXlt` may not survive the process. When extracting static
#' variables, the function uses `do.call(c, ...)` to preserve common classes
#' such as `Date`, `POSIXct`, and `difftime`. However, if a static variable
#' has a class that is not among the basic atomic types or factors, a warning
#' is issued, and you should consider converting such columns to simpler types.
#'
#' Internally, a temporary separator `"._TEMP_."` is used during the reshape
#' step and later replaced by the user‑provided `spacer`. The function checks
#' that neither column names nor time values contain this exact string; if they
#' do, an error is raised. Avoid using this substring in your variable names
#' and time values.
#'
#' @seealso
#' See also [make_panel()], [make_long()], [make_balanced()].
#'
#' @examples
#' data(production)
#'
#' # Basic usage: select time-varying variables
#' wide <- make_wide(production, select = c("sales", "labor"),
#'                   index = c("firm", "year"))
#'
#' # With panel_data object
#' panel <- make_panel(production, index = c("firm", "year"))
#' wide2 <- make_wide(panel, select = c("sales", "labor"))
#'
#' # Custom spacer and inverted order
#' wide3 <- make_wide(production, select = c("sales", "labor"),
#'                    index = c("firm", "year"),
#'                    spacer = ".", invert = TRUE)
#'
#' # Accessing attributes
#' attr(wide3, "metadata")
#' attr(wide3, "details")
#'
#' @importFrom utils head
#' @importFrom stats reshape
#' @export
make_wide <- function(
  data,
  select,
  index = NULL,
  spacer = "_",
  invert = FALSE
) {
  # --- Input validation ---
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1], call. = FALSE)
  }
  if (missing(select) || is.null(select)) {
    stop("'select' must be provided and cannot be NULL", call. = FALSE)
  }
  if (!is.character(select) || length(select) == 0) {
    stop("'select' must be a non-empty character vector", call. = FALSE)
  }
  if (!is.character(spacer) || length(spacer) != 1) {
    stop("'spacer' must be a single character string", call. = FALSE)
  }
  if (!is.logical(invert) || length(invert) != 1) {
    stop("'invert' must be a single logical value", call. = FALSE)
  }

  msg_printed <- FALSE
  entity_var <- NULL
  time_var <- NULL
  keep_panel_class <- FALSE
  panel_metadata <- NULL
  panel_details <- NULL

  # --- Extract index from metadata if applicable ---
  if (inherits(data, "panel_data")) {
    meta <- attr(data, "metadata")
    if (!is.null(meta) && !is.null(meta$entity) && !is.null(meta$time)) {
      if (is.null(index)) {
        entity_var <- meta$entity
        time_var <- meta$time
        keep_panel_class <- TRUE
        panel_metadata <- meta
        panel_details <- attr(data, "details")
      } else {
        # User provided index overrides
        if (length(index) != 2 || !is.character(index)) {
          stop("'index' must be a character vector of length 2", call. = FALSE)
        }
        entity_var <- index[1]
        time_var <- index[2]
      }
    } else {
      # No valid metadata, fall back to user index
      if (is.null(index)) {
        stop("For regular data.frames, 'index' must be provided", call. = FALSE)
      }
      if (length(index) != 2 || !is.character(index)) {
        stop("'index' must be a character vector of length 2", call. = FALSE)
      }
      entity_var <- index[1]
      time_var <- index[2]
    }
  } else {
    # Regular data frame
    if (is.null(index)) {
      stop("For regular data.frames, 'index' must be provided", call. = FALSE)
    }
    if (length(index) != 2 || !is.character(index)) {
      stop("'index' must be a character vector of length 2", call. = FALSE)
    }
    entity_var <- index[1]
    time_var <- index[2]
  }

  # --- Validate existence of index variables ---
  if (!entity_var %in% names(data)) {
    stop('entity variable "', entity_var, '" not found in data', call. = FALSE)
  }
  if (!time_var %in% names(data)) {
    stop('time variable "', time_var, '" not found in data', call. = FALSE)
  }
  if (time_var == entity_var) {
    stop("entity and time variables cannot be the same", call. = FALSE)
  }

  # --- Validate select variables ---
  missing_vars <- select[!select %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(
      "Variables in 'select' not found in data: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  if (entity_var %in% select) {
    stop(
      "'select' cannot contain the entity variable '",
      entity_var,
      "'",
      call. = FALSE
    )
  }
  if (time_var %in% select) {
    stop(
      "'select' cannot contain the time variable '",
      time_var,
      "'",
      call. = FALSE
    )
  }

  # --- Remove rows with NA in entity or time ---
  na_entity <- is.na(data[[entity_var]])
  na_time <- is.na(data[[time_var]])
  if (any(na_entity)) {
    message(
      sum(na_entity),
      " rows with missing values in '",
      entity_var,
      "' variable found and excluded."
    )
    msg_printed <- TRUE
  }
  if (any(na_time)) {
    message(
      sum(na_time),
      " rows with missing values in '",
      time_var,
      "' variable found and excluded."
    )
    msg_printed <- TRUE
  }
  if (any(na_entity | na_time)) {
    data <- data[!(na_entity | na_time), , drop = FALSE]
    rownames(data) <- NULL
  }

  # --- Duplicate check: always run, even for panel_data objects ---
  dup_rows <- duplicated(data[c(entity_var, time_var)]) |
    duplicated(data[c(entity_var, time_var)], fromLast = TRUE)
  if (any(dup_rows)) {
    dup_combinations <- unique(data[
      dup_rows,
      c(entity_var, time_var),
      drop = FALSE
    ])
    n_dup <- nrow(dup_combinations)
    examples <- utils::head(dup_combinations, 5)
    example_strings <- paste0(
      examples[[entity_var]],
      "-",
      examples[[time_var]]
    )
    example_str <- paste(example_strings, collapse = ", ")
    stop(
      "Duplicate entity-time combinations found: ",
      n_dup,
      " unique combinations with duplicates. ",
      "Examples: ",
      example_str,
      call. = FALSE
    )
  }

  # --- Helper function to check time-invariance ---
  # `all_na_invariant`: if TRUE, an all‑NA column is considered invariant.
  check_invariant <- function(var, data, entity_var, all_na_invariant = TRUE) {
    vals <- data[[var]]
    entities <- data[[entity_var]]
    vals_list <- split(vals, entities)
    any_multiple <- any(sapply(vals_list, function(x) {
      non_na <- x[!is.na(x)]
      if (length(non_na) == 0) {
        # no non‑NA values in this entity
        return(!all_na_invariant)
      } else {
        return(length(unique(non_na)) > 1)
      }
    }))
    return(!any_multiple)
  }

  # --- Determine static variables (all variables except entity, time, and select) ---
  all_vars <- names(data)
  static_vars <- setdiff(all_vars, c(entity_var, time_var, select))

  # --- Check that all static variables are indeed time-invariant ---
  if (length(static_vars) > 0) {
    invariant_check <- sapply(static_vars, function(v) {
      check_invariant(v, data, entity_var, all_na_invariant = TRUE)
    })
    if (!all(invariant_check)) {
      non_invariant <- static_vars[!invariant_check]
      stop(
        "The following variables are not time-invariant and were not selected for reshaping:\n",
        paste(non_invariant, collapse = ", "),
        "\nEither include them in 'select' or ensure they are constant over time.",
        call. = FALSE
      )
    }
    message(
      "Variables treated as time-invariant: ",
      paste(static_vars, collapse = ", ")
    )
    msg_printed <- TRUE
  }

  # --- Prepare static data (one row per entity) ---
  if (length(static_vars) > 0) {
    # Start with one row per entity (first occurrence)
    static_data <- data[
      !duplicated(data[[entity_var]]),
      c(entity_var, static_vars),
      drop = FALSE
    ]

    # For each static variable, extract first non-NA value per entity while preserving class
    entities_uniq <- static_data[[entity_var]]
    for (v in static_vars) {
      vals_list <- split(data[[v]], data[[entity_var]])
      first_vals <- lapply(entities_uniq, function(e) {
        vals <- vals_list[[as.character(e)]]
        non_na <- vals[!is.na(vals)]
        if (length(non_na) == 0) NA else non_na[1]
      })
      if (is.factor(data[[v]])) {
        static_data[[v]] <- factor(
          unlist(first_vals, use.names = FALSE),
          levels = levels(data[[v]]),
          ordered = is.ordered(data[[v]])
        )
      } else {
        static_data[[v]] <- do.call(c, first_vals)
        cls <- class(data[[v]])
        basic_classes <- c(
          "numeric",
          "integer",
          "character",
          "logical",
          "factor",
          "ordered",
          "Date",
          "POSIXct",
          "POSIXt",
          "difftime",
          "hms"
        )
        if (!any(cls %in% basic_classes) && !is.null(cls)) {
          warning(
            "Static variable '",
            v,
            "' has class(es) ",
            paste(cls, collapse = ", "),
            " that may not be preserved during extraction; ",
            "consider converting to a simpler type.",
            call. = FALSE
          )
        }
      }
    }
  } else {
    static_data <- NULL
  }

  # --- Data for reshaping: keep only entity, time, and selected variables ---
  data_for_reshape <- data[, c(entity_var, time_var, select), drop = FALSE]

  # --- Perform reshape ---
  temp_sep <- "._TEMP_."
  if (any(grepl(temp_sep, names(data_for_reshape)))) {
    stop(
      "Column names contain the temporary separator '",
      temp_sep,
      "'. Please rename these columns.",
      call. = FALSE
    )
  }
  if (any(grepl(temp_sep, as.character(data_for_reshape[[time_var]])))) {
    stop(
      "Time values contain the temporary separator '",
      temp_sep,
      "'. Please change the time values.",
      call. = FALSE
    )
  }

  wide <- stats::reshape(
    data_for_reshape,
    direction = "wide",
    idvar = entity_var,
    timevar = time_var,
    v.names = select,
    sep = temp_sep
  )

  # Rename columns: replace last occurrence of temp_sep with spacer
  new_names <- names(wide)
  for (i in seq_along(new_names)) {
    nm <- new_names[i]
    if (grepl(temp_sep, nm)) {
      all_pos <- gregexpr(temp_sep, nm, fixed = TRUE)[[1]]
      if (length(all_pos) > 0) {
        last_pos <- all_pos[length(all_pos)]
        var_part <- substr(nm, 1, last_pos - 1)
        time_part <- substr(nm, last_pos + nchar(temp_sep), nchar(nm))
        if (invert) {
          new_names[i] <- paste0(time_part, spacer, var_part)
        } else {
          new_names[i] <- paste0(var_part, spacer, time_part)
        }
      }
    }
  }
  names(wide) <- new_names
  rownames(wide) <- NULL

  # --- Merge with static data if any ---
  if (!is.null(static_data)) {
    wide <- merge(
      wide,
      static_data,
      by = entity_var,
      all.x = TRUE,
      sort = FALSE
    )
    # New column order: entity, static, time‑varying (reshaped)
    # Reshaped columns are those whose names contain the spacer (or we can get them from select)
    # We can identify them as all columns except entity_var and static_vars
    time_varying_cols <- setdiff(names(wide), c(entity_var, static_vars))
    wide <- wide[, c(entity_var, static_vars, time_varying_cols), drop = FALSE]
  }

  # --- Duplicate column name check (after all renaming and merging) ---
  if (any(duplicated(names(wide)))) {
    dup_names <- unique(names(wide)[duplicated(names(wide))])
    stop(
      "Duplicate column names found after reshaping: ",
      paste(dup_names, collapse = ", "),
      call. = FALSE
    )
  }

  # --- Build metadata attribute ---
  if (keep_panel_class) {
    new_metadata <- panel_metadata
    new_metadata$function_name <- "make_wide"
    new_metadata$spacer <- spacer
    new_metadata$invert <- invert
    new_metadata$select <- select
  } else {
    new_metadata <- list(
      function_name = "make_wide",
      entity = entity_var,
      time = time_var,
      spacer = spacer,
      invert = invert,
      select = select
    )
  }

  new_details <- list(
    reshaped = select,
    static = static_vars
  )

  attr(wide, "metadata") <- new_metadata
  attr(wide, "details") <- new_details
  class(wide) <- c("panel_wide", class(wide))

  if (msg_printed) {
    cat("\n")
  }
  return(wide)
}
