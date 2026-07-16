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
#'        values in the wide format column names. Default = `"_"`.
#' @param invert A logical flag indicating whether to put time values before
#'        variable names in column names. If `FALSE`, column names are
#'        `"variable_spacer_time"`; if `TRUE`, they are `"time_spacer_variable"`.
#'        Default = FALSE.
#'
#' @return A data.frame containing panel data in a wide format, with class
#'         `"panel_wide"` and the attributes `metadata` and `details`.
#'
#' @details
#' The function converts data from long to wide format. Below is an illustration
#' of the transformation for two time periods (t = 1, 2) and two time‑varying
#' variables (`x`, `y`) plus a static variable `z`.
#'
#' **Long format (input):**
#'
#' | id | t | x | y | z |
#' |----|---|---|---|---|
#' | 1  | 1 | 5 | 8 | A |
#' | 1  | 2 | 7 | 9 | A |
#' | 2  | 1 | 6 | 7 | B |
#' | 2  | 2 | 8 | 6 | B |
#'
#' **Wide format (output) with `spacer = "_"` and `invert = FALSE`:**
#'
#' | id | x_1 | x_2 | y_1 | y_2 | z |
#' |----|-----|-----|-----|-----|---|
#' | 1  | 5   | 7   | 8   | 9   | A |
#' | 2  | 6   | 8   | 7   | 6   | B |
#'
#' All columns not in `select` or the index are automatically treated as
#' time‑invariant. The function verifies that they are indeed constant over
#' time for each entity; if not, it stops with an error listing the offenders.
#'
#' The reshaped columns are ordered as follows: the entity column appears first,
#' then any time‑invariant variables (in the order they appear in the input data),
#' and finally the selected time‑varying variables, each expanded by time period.
#' Time periods are ordered by their natural order (as given by unique values of
#' the time variable). If an entity is missing a particular time period, the
#' corresponding wide column will contain `NA`.
#'
#' The returned object has class `"panel_wide"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.
#'         If the input was a `panel_data` object, the original metadata elements
#'         (entity, time, and delta) are preserved.}
#'   \item{`details`}{List containing information used to print the summary
#'         message, including direction, counts, j‑variable, time values,
#'         mapping between stubs and wide columns, and static variables.}
#' }
#'
#' Upon successful reshaping, a summary message similar to Stata's `reshape` is
#' printed to the console.
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

  # --- Extract index ---
  entity_var <- NULL
  time_var <- NULL
  keep_panel_class <- FALSE
  panel_metadata <- NULL

  if (inherits(data, "panel_data")) {
    meta <- attr(data, "metadata")
    if (!is.null(meta) && !is.null(meta$entity) && !is.null(meta$time)) {
      if (is.null(index)) {
        entity_var <- meta$entity
        time_var <- meta$time
        keep_panel_class <- TRUE
        panel_metadata <- meta
      } else {
        if (length(index) != 2 || !is.character(index)) {
          stop("'index' must be a character vector of length 2", call. = FALSE)
        }
        entity_var <- index[1]
        time_var <- index[2]
      }
    } else {
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
    if (is.null(index)) {
      stop("For regular data.frames, 'index' must be provided", call. = FALSE)
    }
    if (length(index) != 2 || !is.character(index)) {
      stop("'index' must be a character vector of length 2", call. = FALSE)
    }
    entity_var <- index[1]
    time_var <- index[2]
  }

  # Validate existence
  if (!entity_var %in% names(data)) {
    stop('entity variable "', entity_var, '" not found in data', call. = FALSE)
  }
  if (!time_var %in% names(data)) {
    stop('time variable "', time_var, '" not found in data', call. = FALSE)
  }
  if (time_var == entity_var) {
    stop("entity and time variables cannot be the same", call. = FALSE)
  }

  # Validate select
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

  # --- Pre‑reshape info ---
  n_obs_before <- nrow(data)
  n_vars_before <- ncol(data)

  # Remove NA rows
  na_entity <- is.na(data[[entity_var]])
  na_time <- is.na(data[[time_var]])
  if (any(na_entity)) {
    message(
      sum(na_entity),
      " rows with missing values in '",
      entity_var,
      "' variable found and excluded."
    )
  }
  if (any(na_time)) {
    message(
      sum(na_time),
      " rows with missing values in '",
      time_var,
      "' variable found and excluded."
    )
  }
  if (any(na_entity | na_time)) {
    data <- data[!(na_entity | na_time), , drop = FALSE]
    rownames(data) <- NULL
  }

  # Duplicate check
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
    example_strings <- paste0(examples[[entity_var]], "-", examples[[time_var]])
    example_str <- paste(example_strings, collapse = ", ")
    stop(
      "Duplicate entity-time combinations found: ",
      n_dup,
      " unique combinations with duplicates. Examples: ",
      example_str,
      call. = FALSE
    )
  }

  # Helper: check invariance
  check_invariant <- function(var, data, entity_var, all_na_invariant = TRUE) {
    vals <- data[[var]]
    entities <- data[[entity_var]]
    vals_list <- split(vals, entities)
    any_multiple <- any(sapply(vals_list, function(x) {
      non_na <- x[!is.na(x)]
      if (length(non_na) == 0) {
        return(!all_na_invariant)
      }
      return(length(unique(non_na)) > 1)
    }))
    return(!any_multiple)
  }

  # Determine static variables
  all_vars <- names(data)
  static_vars <- setdiff(all_vars, c(entity_var, time_var, select))

  # Check invariance
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
  }

  # Prepare static data
  if (length(static_vars) > 0) {
    static_data <- data[
      !duplicated(data[[entity_var]]),
      c(entity_var, static_vars),
      drop = FALSE
    ]
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

  # Data for reshaping
  data_for_reshape <- data[, c(entity_var, time_var, select), drop = FALSE]

  # Perform reshape
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

  # Rename columns
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

  # Merge static
  if (!is.null(static_data)) {
    wide <- merge(
      wide,
      static_data,
      by = entity_var,
      all.x = TRUE,
      sort = FALSE
    )
    time_varying_cols <- setdiff(names(wide), c(entity_var, static_vars))
    wide <- wide[, c(entity_var, static_vars, time_varying_cols), drop = FALSE]
  }

  # Duplicate column check
  if (any(duplicated(names(wide)))) {
    dup_names <- unique(names(wide)[duplicated(names(wide))])
    stop(
      "Duplicate column names found after reshaping: ",
      paste(dup_names, collapse = ", "),
      call. = FALSE
    )
  }

  # --- Post‑reshape info ---
  n_obs_after <- nrow(wide)
  n_vars_after <- ncol(wide)
  time_values <- sort(unique(data[[time_var]]))
  n_j <- length(time_values)

  # Build mapping
  xij_mapping <- list()
  for (stub in select) {
    cols <- sapply(time_values, function(t) {
      if (invert) paste0(t, spacer, stub) else paste0(stub, spacer, t)
    })
    xij_mapping[[stub]] <- cols
  }

  # --- Metadata and details ---
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
    direction = "long_to_wide",
    n_obs_before = n_obs_before,
    n_obs_after = n_obs_after,
    n_vars_before = n_vars_before,
    n_vars_after = n_vars_after,
    j_var = time_var,
    j_values = time_values,
    xij_mapping = xij_mapping,
    static = static_vars
  )

  attr(wide, "metadata") <- new_metadata
  attr(wide, "details") <- new_details
  class(wide) <- c("panel_wide", class(wide))

  # --- Print summary ---
  cat("\nData long -> wide\n")
  cat(sprintf("Number of obs. %3d -> %3d\n", n_obs_before, n_obs_after))
  cat(sprintf("Number of variables %3d -> %3d\n", n_vars_before, n_vars_after))
  cat(sprintf("j variable (%d values) %s -> (dropped)\n", n_j, time_var))
  cat("xij variables:\n")
  for (stub in select) {
    cols <- paste(xij_mapping[[stub]], collapse = " ")
    cat(sprintf("  %s -> %s\n", stub, cols))
  }
  cat("\n")

  return(wide)
}
