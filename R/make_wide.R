#' Convert Panel Data from Long to Wide Format
#'
#' This function reshapes panel data from long format to wide format,
#' creating separate columns for each time period.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the
#'        entity and time variables.
#'        If not specified and data is a `panel_data` object, the entity and time values
#'        will be extracted from the data.frame attributes.
#' @param static A character vector of variable names that are time-invariant.
#'        If specified, the function verifies invariance and excludes these
#'        variables from reshaping; they appear as single columns in the wide
#'        output.
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
#' `year`, and time‑varying variables `y` and `x`. If there are two time periods
#' (e.g., 2000 and 2001), the resulting wide data.frame will have a single row
#' per entity, with columns `id`, `y_2000`, `y_2001`, `x_2000`, and `x_2001`.
#' Static variables, if declared via the `static` argument, appear as single
#' columns, replicated once per entity rather than split by time.
#'
#' The reshaped columns are ordered as follows: the entity column appears first,
#' then any static variables (in the order specified by the `static` argument),
#' and finally all time‑varying variables in the order they were present in the
#' input data. For each such variable, one column per time period is created,
#' using the unique values of the time variable sorted according to their natural
#' order. If an entity is missing a particular time period, the corresponding
#' wide column will contain `NA`.
#'
#' The returned object has class `"panel_wide"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.
#'         If the input was a `panel_data` object, the original metadata elements
#'         (entity, time, and delta) are preserved.}
#'   \item{`details`}{List containing the names of reshaped variables and detected static variables.}
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
#' # Basic usage
#' wide <- make_wide(production, index = c("firm", "year"))
#'
#' # With panel_data object
#' panel <- make_panel(production, index = c("firm", "year"))
#' wide2 <- make_wide(panel)
#'
#' # Custom spacer and inverted order
#' wide3 <- make_wide(production, index = c("firm", "year"),
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
  index = NULL,
  static = NULL,
  spacer = "_",
  invert = FALSE
) {
  # --- Input validation ---
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1], call. = FALSE)
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

  # --- Validate existence ---
  if (!entity_var %in% names(data)) {
    stop('entity variable "', entity_var, '" not found in data', call. = FALSE)
  }
  if (!time_var %in% names(data)) {
    stop('time variable "', time_var, '" not found in data', call. = FALSE)
  }
  if (time_var == entity_var) {
    stop("entity and time variables cannot be the same", call. = FALSE)
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
  # `all_na_invariant`: if TRUE, an all‑NA column is considered invariant;
  # if FALSE, all‑NA is treated as time‑varying.
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

  # --- Validate and process 'static' ---
  if (!is.null(static)) {
    if (!is.character(static)) {
      stop("'static' must be a character vector", call. = FALSE)
    }
    if (any(static %in% c(entity_var, time_var))) {
      stop("Static variables cannot be entity or time variables", call. = FALSE)
    }
    missing_static <- setdiff(static, names(data))
    if (length(missing_static) > 0) {
      stop(
        "Static variable(s) not found in data: ",
        paste(missing_static, collapse = ", "),
        call. = FALSE
      )
    }
    # Check invariance for each static variable (all‑NA is allowed)
    invariant_check <- sapply(static, function(v) {
      check_invariant(v, data, entity_var, all_na_invariant = TRUE)
    })
    if (!all(invariant_check)) {
      non_invariant <- static[!invariant_check]
      stop(
        "The following static variables are not time-invariant: ",
        paste(non_invariant, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # --- Detect all time-invariant variables (for details) ---
  # Here we treat all‑NA as varying (so they are reshaped).
  all_vars <- setdiff(names(data), c(entity_var, time_var))
  invariant_vars <- all_vars[sapply(
    all_vars,
    check_invariant,
    data = data,
    entity_var = entity_var,
    all_na_invariant = FALSE # all‑NA -> not invariant
  )]

  # --- Detect and report time-invariant variables (user messages) ---
  if (is.null(static)) {
    if (length(invariant_vars) > 0) {
      message(
        "Time-invariant variables detected: ",
        paste(invariant_vars, collapse = ", "),
        ". Consider using 'static' argument."
      )
      msg_printed <- TRUE
    }
  } else {
    # Check variables not in static
    other_vars <- setdiff(all_vars, static)
    additional <- other_vars[sapply(
      other_vars,
      check_invariant,
      data = data,
      entity_var = entity_var,
      all_na_invariant = FALSE # same treatment
    )]
    if (length(additional) > 0) {
      message(
        "Additional time-invariant variables detected: ",
        paste(additional, collapse = ", "),
        ". Consider updating 'static' argument."
      )
      msg_printed <- TRUE
    }
  }

  # --- Prepare data for reshaping ---
  if (!is.null(static)) {
    # Start with one row per entity (first occurrence)
    static_data <- data[
      !duplicated(data[[entity_var]]),
      c(entity_var, static),
      drop = FALSE
    ]

    # For each static variable, extract first non-NA value per entity while preserving class
    entities_uniq <- static_data[[entity_var]]
    for (v in static) {
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

    data_for_reshape <- data[, setdiff(names(data), static), drop = FALSE]
  } else {
    data_for_reshape <- data
  }

  varying_vars <- setdiff(names(data_for_reshape), c(entity_var, time_var))

  # --- Perform reshape ---
  if (length(varying_vars) == 0) {
    if (is.null(static)) {
      stop(
        "No variables to reshape (only entity and time found)",
        call. = FALSE
      )
    } else {
      wide <- static_data
      rownames(wide) <- NULL
    }
  } else {
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
      v.names = varying_vars,
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

    if (!is.null(static)) {
      wide <- merge(
        wide,
        static_data,
        by = entity_var,
        all.x = TRUE,
        sort = FALSE
      )
      # New column order: entity, static, time‑varying
      time_varying_cols <- setdiff(names(wide), c(entity_var, static))
      wide <- wide[, c(entity_var, static, time_varying_cols), drop = FALSE]
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
  }

  # --- Build metadata attribute ---
  if (keep_panel_class) {
    new_metadata <- panel_metadata
    new_metadata$function_name <- "make_wide"
    new_metadata$spacer <- spacer
    new_metadata$invert <- invert
    if (!is.null(static)) new_metadata$static <- static
  } else {
    new_metadata <- list(
      function_name = "make_wide",
      entity = entity_var,
      time = time_var,
      spacer = spacer,
      invert = invert
    )
    if (!is.null(static)) new_metadata$static <- static
  }

  new_details <- list(
    reshaped = varying_vars,
    static_detected = invariant_vars
  )

  attr(wide, "metadata") <- new_metadata
  attr(wide, "details") <- new_details
  class(wide) <- c("panel_wide", class(wide))

  if (msg_printed) {
    cat("\n")
  }
  return(wide)
}
