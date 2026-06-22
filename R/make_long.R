#' Convert Panel Data from Wide to Long Format
#'
#' This function reshapes panel data from wide format to long format,
#' stacking time-varying columns into rows based on the pattern of column names.
#' Columns that are time‑invariant (not split by time) are replicated for each
#' time period, **unless** a particular entity‑time combination has no
#' time‑varying data at all—in which case the invariant columns are set to `NA`
#' to reflect a truly missing observation.
#'
#' @param data A data.frame containing panel data in a wide format.
#' @param index A character vector of length 2 specifying the name of the
#'        entity column (first element) and the name to give to the new time
#'        column in the long format (second element).
#' @param static A character vector of variable names that are time‑invariant.
#'        Default is `NULL`, meaning the function will automatically detect
#'        columns that do not contain the time separator (or numeric suffix).
#'        If provided, the function verifies that these columns do **not** match
#'        the time‑varying pattern (otherwise an error is raised). Any other
#'        columns that also appear invariant will trigger a message.
#' @param spacer A character string used to separate variable names and time
#'        values in the wide column names. Default = `"_"`.
#' @param invert A logical flag indicating the order of components in column
#'        names. If `FALSE` (default), column names are
#'        `"variable_spacer_time"` (or `"variable"` + `time` when `spacer = ""`);
#'        if `TRUE`, they are `"time_spacer_variable"` (or `time` + `"variable"`
#'        when `spacer = ""`). Must match the structure of the input data.
#'
#' @return A data frame in long format, with one row per entity‑time combination
#'         that appears in the data. For unbalanced panels, if an entity has
#'         no time‑varying data for a given period, the invariant columns are
#'         set to `NA` for that row.
#'
#' @details
#' The function performs the following steps:
#' * If `data` has panel attributes (e.g., from `make_wide()`) and `index` is
#'   not specified, the entity column and the name for the new time column
#'   are taken from the metadata. **Note:** `spacer` and `invert` are **not**
#'   taken from the metadata; they must be supplied explicitly or use the
#'   function defaults.
#' * Columns that do not contain the `spacer` (or do not match the expected
#'   pattern when `spacer = ""`) are treated as time‑constant and are replicated
#'   for each time period.
#' * Columns that match the pattern are split into variable names and time
#'   values; the set of unique time values defines the periods.
#' * The data are reshaped to long format using `stats::reshape()`.
#'
#' If `static` is specified, those columns are **always** treated as
#' time‑invariant, even if they contain a separator or numeric suffix. The
#' function checks that they do **not** conflict with the time‑varying pattern
#' and raises an error if they do. Additional invariant columns (not listed in
#' `static`) are still treated as invariant and trigger a message.
#'
#' If `static` is `NULL`, the function automatically detects invariant columns
#' and prints a message listing them, suggesting to use the `static` argument.
#'
#' **Unbalanced panels:**
#' After reshaping, the function checks each entity‑time row. If **all** time‑varying
#' columns are `NA` for that row, the invariant columns (including those in `static`)
#' are set to `NA` as well. This ensures that a period with no observed variation
#' is treated as completely missing, rather than erroneously carrying forward
#' constant attributes.
#'
#' @note
#' When `spacer = ""`, the function assumes that all time‑varying columns have
#' a numeric suffix (if `invert = FALSE`) or numeric prefix (if `invert = TRUE`)
#' that represents the time period. The consistency checks for separator
#' ambiguity are skipped for `spacer = ""` because there is no separator to compare,
#' but the `invert` correctness check still applies.
#'
#' The function assumes that all time-varying columns follow a consistent naming
#' pattern and that every variable appears for exactly the same set of time
#' periods (balanced in the wide sense). If some variable‑time combinations are
#' missing, a message is printed and those variables are omitted.
#'
#' @seealso
#' See also [make_panel()], [make_wide()], [make_balanced()], [make_demeaned()].
#'
#' @examples
#' data(production)
#'
#' # First convert to wide, then back to long
#' wide <- make_wide(production, index = c("firm", "year"))
#' long <- make_long(wide)
#' head(long)
#'
#' # With custom spacer and invert
#' wide2 <- make_wide(production, index = c("firm", "year"), spacer = ".", invert = TRUE)
#' long2 <- make_long(wide2, spacer = ".", invert = TRUE)
#'
#' # Using panel attributes (no need to specify index)
#' panel <- make_panel(production, index = c("firm", "year"))
#' wide3 <- make_wide(panel)
#' long3 <- make_long(wide3)   # spacer and invert must match the wide format
#'
#' # Using spacer = "" (no separator)
#' wide4 <- make_wide(production, index = c("firm", "year"), spacer = "")
#' long4 <- make_long(wide4, spacer = "")
#'
#' # Explicitly declare 'region' as static (it appears as a single column)
#' wide5 <- make_wide(production, index = c("firm", "year"), static = "region")
#' long5 <- make_long(wide5, static = "region")
#'
#' @export
make_long <- function(
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
  if (!is.null(static)) {
    if (!is.character(static)) {
      stop("'static' must be a character vector", call. = FALSE)
    }
  }

  msg_printed <- FALSE
  entity_col <- NULL
  time_col <- NULL
  keep_panel_class <- FALSE
  panel_metadata <- NULL
  panel_details <- NULL

  # --- Extract metadata if data has panel attributes ---
  if (inherits(data, "panel_data")) {
    meta <- attr(data, "metadata")
    if (!is.null(meta)) {
      if (is.null(index)) {
        if (!is.null(meta$entity) && !is.null(meta$time)) {
          entity_col <- meta$entity
          time_col <- meta$time
        } else if (!is.null(meta$entity) && is.null(meta$time)) {
          entity_col <- meta$entity
          time_col <- "time"
        } else {
          stop(
            "Panel metadata missing required 'entity' information",
            call. = FALSE
          )
        }
        keep_panel_class <- TRUE
        panel_metadata <- meta
        panel_details <- attr(data, "details")
      } else {
        if (length(index) != 2 || !is.character(index)) {
          stop("'index' must be a character vector of length 2", call. = FALSE)
        }
        entity_col <- index[1]
        time_col <- index[2]
      }
    } else {
      if (is.null(index)) {
        stop("For regular data.frames, 'index' must be provided", call. = FALSE)
      }
      if (length(index) != 2 || !is.character(index)) {
        stop("'index' must be a character vector of length 2", call. = FALSE)
      }
      entity_col <- index[1]
      time_col <- index[2]
    }
  } else {
    if (is.null(index)) {
      stop("For regular data.frames, 'index' must be provided", call. = FALSE)
    }
    if (length(index) != 2 || !is.character(index)) {
      stop("'index' must be a character vector of length 2", call. = FALSE)
    }
    entity_col <- index[1]
    time_col <- index[2]
  }

  # --- Validate entity column ---
  if (!entity_col %in% names(data)) {
    stop('entity column "', entity_col, '" not found in data', call. = FALSE)
  }

  # --- Validate static variables ---
  if (!is.null(static)) {
    missing_static <- setdiff(static, names(data))
    if (length(missing_static) > 0) {
      stop(
        "Static variable(s) not found in data: ",
        paste(missing_static, collapse = ", "),
        call. = FALSE
      )
    }
    if (entity_col %in% static) {
      stop("Static variables cannot include the entity column", call. = FALSE)
    }
  }

  all_cols <- names(data)
  # Columns that can potentially be time-varying: exclude entity and static
  candidates <- setdiff(all_cols, c(entity_col, static))

  # Parse columns to identify time-varying ones (only among candidates)
  parsed <- list()
  time_values <- c()

  for (col in candidates) {
    if (spacer == "") {
      # No separator: use regular expressions
      if (invert) {
        # Pattern: time (numeric prefix) + variable (rest)
        match <- regexpr("^\\d+", col)
        if (match == -1) {
          # No leading digits -> treat as constant column
          next
        }
        time_val <- regmatches(col, match)
        var_name <- substr(col, match + attr(match, "match.length"), nchar(col))
        if (var_name == "") var_name <- "value"
      } else {
        # Pattern: variable + numeric time suffix
        match <- regexpr("\\d+$", col)
        if (match == -1) {
          next
        }
        time_val <- regmatches(col, match)
        var_name <- substr(col, 1, match - 1)
        if (var_name == "") var_name <- "value"
      }
    } else {
      # Normal separator-based splitting
      if (!grepl(spacer, col, fixed = TRUE)) {
        next
      }
      parts <- strsplit(col, spacer, fixed = TRUE)[[1]]
      if (invert) {
        if (length(parts) < 2) {
          next
        }
        time_val <- parts[1]
        var_name <- paste(parts[-1], collapse = spacer)
      } else {
        if (length(parts) < 2) {
          next
        }
        var_name <- paste(parts[-length(parts)], collapse = spacer)
        time_val <- parts[length(parts)]
      }
    }
    parsed[[col]] <- list(var = var_name, time = time_val)
    time_values <- c(time_values, time_val)
  }

  # Check if any static variable would have been parsed (conflict)
  if (!is.null(static)) {
    # Temporarily parse all columns to see if any static matches the pattern
    conflict <- c()
    for (col in static) {
      if (spacer == "") {
        if (invert) {
          if (grepl("^\\d+", col)) conflict <- c(conflict, col)
        } else {
          if (grepl("\\d+$", col)) conflict <- c(conflict, col)
        }
      } else {
        if (grepl(spacer, col, fixed = TRUE)) conflict <- c(conflict, col)
      }
    }
    if (length(conflict) > 0) {
      stop(
        "The following static variables appear to be time-varying based on the pattern ",
        "(they contain the separator or numeric suffix): ",
        paste(conflict, collapse = ", "),
        ". Please remove them from 'static' or adjust 'spacer'/'invert'.",
        call. = FALSE
      )
    }
  }

  if (length(parsed) == 0) {
    stop(
      "No time-varying columns found. Check 'spacer' and 'invert' settings.",
      call. = FALSE
    )
  }

  # --- Ambiguous separator check (skip if spacer == "") ---
  if (spacer != "") {
    unique_times <- unique(time_values)
    # Convert to character for matching; preserve numeric-like times
    time_set <- as.character(unique_times)
    ambiguous_cols <- c()

    # Determine which columns were treated as constant (among candidates only)
    parsed_cols <- names(parsed)
    constant_candidates <- setdiff(candidates, parsed_cols)

    for (col in constant_candidates) {
      if (invert) {
        # Check for leading digits followed by a non-digit (i.e., a separator)
        if (grepl("^\\d+[^0-9]", col)) {
          match <- regexpr("^\\d+", col)
          prefix_digits <- regmatches(col, match)
          if (prefix_digits %in% time_set) {
            ambiguous_cols <- c(ambiguous_cols, col)
          }
        }
      } else {
        # Check for trailing digits preceded by a non-digit (separator)
        if (grepl("[^0-9]\\d+$", col)) {
          match <- regexpr("\\d+$", col)
          suffix_digits <- regmatches(col, match)
          if (suffix_digits %in% time_set) {
            ambiguous_cols <- c(ambiguous_cols, col)
          }
        }
      }
    }

    if (length(ambiguous_cols) > 0) {
      # Get the separators used in those columns (the character before/after digits)
      sep_examples <- c()
      for (col in ambiguous_cols) {
        if (invert) {
          sep_char <- substr(
            col,
            nchar(regmatches(col, regexpr("^\\d+", col))) + 1,
            nchar(regmatches(col, regexpr("^\\d+", col))) + 1
          )
        } else {
          pos <- regexpr("\\d+$", col)
          sep_char <- substr(col, pos - 1, pos - 1)
        }
        sep_examples <- c(
          sep_examples,
          paste0("'", col, "' (uses '", sep_char, "')")
        )
      }
      stop(
        "Ambiguous separators detected. The following columns appear to be time-varying ",
        "but use a different separator than the specified spacer '",
        spacer,
        "':\n",
        paste(sep_examples, collapse = ", "),
        "\n",
        "Please ensure all time-varying columns use the same spacer.",
        call. = FALSE
      )
    }
  }

  # --- Check for correct invert specification ---
  var_names <- sapply(parsed, function(x) x$var, USE.NAMES = FALSE)
  time_vals <- sapply(parsed, function(x) x$time, USE.NAMES = FALSE)

  time_numeric <- suppressWarnings(as.numeric(time_vals))
  var_numeric <- suppressWarnings(as.numeric(var_names))

  prop_time_numeric <- sum(!is.na(time_numeric)) / length(time_vals)
  prop_var_numeric <- sum(!is.na(var_numeric)) / length(var_names)

  if (prop_time_numeric < 0.5 && prop_var_numeric > 0.5) {
    suggested_invert <- ifelse(invert, "FALSE", "TRUE")
    stop(
      "It appears that 'invert' may be incorrectly specified.\n",
      "Most extracted time values are non-numeric (",
      round((1 - prop_time_numeric) * 100),
      "%) while most variable names are numeric (",
      round(prop_var_numeric * 100),
      "%).\n",
      "This often happens when the order of variable and time components is swapped.\n",
      "Consider setting 'invert = ",
      suggested_invert,
      "'.",
      call. = FALSE
    )
  }

  # --- Identify constant (static) columns ---
  # All columns that were not parsed are constant (both candidates and static)
  parsed_cols <- names(parsed)
  constant_cols <- setdiff(all_cols, c(entity_col, parsed_cols))

  # If static is provided, we also add the explicitly static ones (they are already in constant_cols)
  # but we need to separate for messaging.
  if (!is.null(static)) {
    # Additional constant columns not in static
    additional <- setdiff(constant_cols, static)
    if (length(additional) > 0) {
      message(
        "Additional time-invariant variables detected: ",
        paste(additional, collapse = ", "),
        ". Consider updating 'static' argument."
      )
      msg_printed <- TRUE
    }
  } else {
    # No static provided; if there are constant columns, message them
    if (length(constant_cols) > 0) {
      message(
        "Time-invariant variables detected: ",
        paste(constant_cols, collapse = ", "),
        ". Consider using 'static' argument."
      )
      msg_printed <- TRUE
    }
  }

  # --- Proceed with reshape ---
  # Unique time values, try numeric sort if possible
  unique_times <- unique(time_values)
  if (all(grepl("^-?[0-9.]+$", unique_times))) {
    unique_times <- as.character(sort(as.numeric(unique_times)))
  } else {
    unique_times <- sort(unique_times)
  }

  # Group columns by variable name
  var_to_cols <- list()
  for (col in names(parsed)) {
    var <- parsed[[col]]$var
    var_to_cols[[var]] <- c(var_to_cols[[var]], col)
  }

  # Build varying list
  varying_list <- list()
  v.names <- c()
  for (var in names(var_to_cols)) {
    cols <- var_to_cols[[var]]
    time_to_col <- sapply(cols, function(c) parsed[[c]]$time, USE.NAMES = FALSE)
    names(time_to_col) <- cols
    ordered_cols <- sapply(unique_times, function(t) {
      match_col <- names(time_to_col)[time_to_col == t]
      if (length(match_col) == 0) NA else match_col[1]
    })
    if (any(is.na(ordered_cols))) {
      warning(
        "Variable '",
        var,
        "' is missing for some time periods. ",
        "Resulting long data will have NA for those entries.",
        call. = FALSE
      )
    }
    varying_list[[var]] <- ordered_cols
    v.names <- c(v.names, var)
  }

  # Keep only variables that have complete time coverage
  complete_vars <- sapply(varying_list, function(x) !any(is.na(x)))
  if (any(!complete_vars)) {
    incomplete <- names(varying_list)[!complete_vars]
    message(
      "Variables missing some time periods (will be omitted from reshape): ",
      paste(incomplete, collapse = ", ")
    )
    msg_printed <- TRUE
    varying_list <- varying_list[complete_vars]
    v.names <- v.names[complete_vars]
  }

  if (length(varying_list) == 0) {
    stop("No complete time-varying variables found", call. = FALSE)
  }

  varying <- lapply(varying_list, function(x) as.character(x))
  # All columns that are not varying and not entity are constant
  final_constant <- setdiff(all_cols, c(entity_col, unlist(varying)))

  # --- Reshape using stats::reshape ---
  wide_subset <- data[,
    c(entity_col, final_constant, unlist(varying)),
    drop = FALSE
  ]

  long <- stats::reshape(
    wide_subset,
    direction = "long",
    varying = varying,
    v.names = v.names,
    timevar = time_col,
    idvar = entity_col,
    times = unique_times,
    new.row.names = NULL
  )

  rownames(long) <- NULL

  # Try to convert time column to numeric if possible
  if (all(grepl("^-?[0-9.]+$", long[[time_col]]))) {
    long[[time_col]] <- as.numeric(long[[time_col]])
  }

  # Reorder columns
  var_cols <- v.names
  const_cols <- intersect(final_constant, names(long))
  long <- long[, c(entity_col, time_col, var_cols, const_cols), drop = FALSE]

  # Sort by entity then time
  long <- long[order(long[[entity_col]], long[[time_col]]), ]
  rownames(long) <- NULL

  # --- Handle unbalanced panels: if all time-varying cols are NA for a row,
  #     set constant columns to NA as well.
  if (length(var_cols) > 0) {
    # identify rows where all time-varying columns are NA
    all_na <- rowSums(is.na(long[, var_cols, drop = FALSE])) == length(var_cols)
    if (any(all_na)) {
      long[all_na, const_cols] <- NA
    }
  }

  # --- Build metadata ---
  if (keep_panel_class) {
    new_metadata <- panel_metadata
    new_metadata$function_name <- "make_long"
    new_metadata$spacer <- spacer
    new_metadata$invert <- invert
    new_metadata$entity <- entity_col
    new_metadata$time <- time_col
    if (!is.null(static)) new_metadata$static <- static
  } else {
    new_metadata <- list(
      function_name = "make_long",
      entity = entity_col,
      time = time_col,
      spacer = spacer,
      invert = invert
    )
    if (!is.null(static)) new_metadata$static <- static
  }

  if (keep_panel_class && !is.null(panel_details)) {
    new_details <- panel_details
  } else {
    new_details <- list()
  }

  attr(long, "metadata") <- new_metadata
  attr(long, "details") <- new_details
  class(long) <- c("panel_data", class(long))

  if (msg_printed) {
    cat("\n")
  }
  return(long)
}
