#' Convert Panel Data from Wide to Long Format
#'
#' This function reshapes panel data from wide format to long format,
#' stacking time-varying columns into rows based on the pattern of column names.
#'
#' @param data A data.frame containing panel data in a wide format.
#' @param index A character vector of length 2 specifying the name of the
#'        entity column (first element) and the name to give to the new time
#'        column in the long format (second element).
#'        If not specified and data is a `panel_data` object, the entity and time values
#'        will be extracted from the data.frame attributes.
#' @param static A character vector of variable names that are time-invariant.
#'        If not specified, the function will automatically detect columns that
#'        do not contain the time separator (or numeric suffix).
#' @param spacer A character string used to separate variable names and time
#'        values in the wide column names. Default = "_".
#' @param invert A logical flag indicating the order of components in column
#'        names. If `FALSE`, column names are `"variable_spacer_time"`; if `TRUE`, they are
#'        `"time_spacer_variable"`. Default = FALSE.
#'
#' @return A data.frame containing panel data in a long format.
#'
#' @details
#' The data are reshaped to long format using `stats::reshape()`.
#'
#' The function assumes that all time-varying columns follow a consistent naming
#' pattern. If some variable‑time combinations are missing (i.e., a column for a
#' variable and a particular time period is absent), the function will add that
#' column filled with `NA` so that the variable is preserved in the long format.
#'
#' Columns that are time-invariant (not split by time) are replicated for each
#' time period, unless a particular entity-time combination has no
#' time-varying data at all—in which case the invariant columns are set to `NA`
#' to reflect a truly missing observation.
#'
#' The returned object has class `"panel_data"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name, the entity and time
#'         variables, the `spacer`, and the `invert` setting. If the input was a
#'         `panel_data` object, the original metadata elements (`delta`, etc.)
#'         are preserved.}
#'   \item{`details`}{Preserved from the input if it was a `panel_data` object;
#'         otherwise an empty list.}
#' }
#'
#' @note
#' If `data` has panel attributes (e.g., from `make_wide()`) and `index` is
#' not specified, the entity column and the name for the new time column
#' are taken from the metadata.
#'
#' @seealso
#' See also [make_panel()], [make_wide()], [make_balanced()].
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
    time_set <- as.character(unique_times)
    ambiguous_cols <- c()

    parsed_cols <- names(parsed)
    constant_candidates <- setdiff(candidates, parsed_cols)

    for (col in constant_candidates) {
      if (invert) {
        if (grepl("^\\d+[^0-9]", col)) {
          match <- regexpr("^\\d+", col)
          prefix_digits <- regmatches(col, match)
          if (prefix_digits %in% time_set) {
            ambiguous_cols <- c(ambiguous_cols, col)
          }
        }
      } else {
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

  # --- Unique time values, try numeric sort if possible ---
  unique_times <- unique(time_values)
  if (all(grepl("^-?[0-9.]+$", unique_times))) {
    unique_times <- as.character(sort(as.numeric(unique_times)))
  } else {
    unique_times <- sort(unique_times)
  }

  # --- Group columns by variable name ---
  var_to_cols <- list()
  for (col in names(parsed)) {
    var <- parsed[[col]]$var
    var_to_cols[[var]] <- c(var_to_cols[[var]], col)
  }

  # --- Determine original order of variables (first appearance) ---
  var_first_pos <- sapply(names(var_to_cols), function(var) {
    # find the first column in this variable that exists in the original data
    cols <- var_to_cols[[var]]
    min_pos <- min(match(cols, all_cols)) # match returns positions in all_cols
    min_pos
  })
  vars_in_order <- names(var_first_pos)[order(var_first_pos)]

  # --- Helper to generate column name from variable, time, spacer, invert ---
  generate_colname <- function(var, time, spacer, invert) {
    if (spacer == "") {
      if (invert) {
        paste0(time, var)
      } else {
        paste0(var, time)
      }
    } else {
      if (invert) {
        paste0(time, spacer, var)
      } else {
        paste0(var, spacer, time)
      }
    }
  }

  # --- Add missing columns for unbalanced variables ---
  added_cols <- character(0)
  for (var in vars_in_order) {
    existing_cols <- var_to_cols[[var]]
    # Get the class of the first existing column (if any)
    if (length(existing_cols) > 0) {
      first_col <- existing_cols[1]
      col_class <- class(data[[first_col]])
    } else {
      col_class <- NULL
    }

    for (t in unique_times) {
      colname <- generate_colname(var, t, spacer, invert)
      if (!colname %in% names(data)) {
        # Create a new column filled with NA, preserving type if possible
        if (!is.null(col_class)) {
          if (is.factor(data[[first_col]])) {
            new_col <- factor(
              rep(NA, nrow(data)),
              levels = levels(data[[first_col]])
            )
          } else {
            new_col <- rep(NA, nrow(data))
            class(new_col) <- col_class
          }
        } else {
          new_col <- rep(NA, nrow(data))
        }
        data[[colname]] <- new_col
        added_cols <- c(added_cols, colname)
        var_to_cols[[var]] <- c(var_to_cols[[var]], colname)
      }
    }
  }

  if (length(added_cols) > 0) {
    message(
      "Added the following columns (filled with NA) to complete unbalanced variables: ",
      paste(added_cols, collapse = ", ")
    )
    msg_printed <- TRUE
  }

  # --- Build varying list with complete column sets, in original variable order ---
  varying_list <- list()
  v.names <- c()
  varying_cols <- c() # will hold all time-varying column names in order

  for (var in vars_in_order) {
    # Generate column names for all time periods in the sorted time order
    cols_for_var <- sapply(unique_times, function(t) {
      generate_colname(var, t, spacer, invert)
    })
    varying_list[[var]] <- cols_for_var
    v.names <- c(v.names, var)
    varying_cols <- c(varying_cols, cols_for_var)
  }

  # --- Identify constant (static) columns ---
  # All columns not entity and not in varying_cols are constant
  final_constant <- setdiff(names(data), c(entity_col, varying_cols))

  # If static is provided, we also add the explicitly static ones
  if (!is.null(static)) {
    # Additional constant columns not in static
    additional <- setdiff(final_constant, static)
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
    if (length(final_constant) > 0) {
      message(
        "Time-invariant variables detected: ",
        paste(final_constant, collapse = ", "),
        ". Consider using 'static' argument."
      )
      msg_printed <- TRUE
    }
  }

  # --- Proceed with reshape ---
  # Subset data in the desired column order: entity, constant (original order), varying (our order)
  wide_subset <- data[,
    c(entity_col, final_constant, varying_cols),
    drop = FALSE
  ]

  long <- stats::reshape(
    wide_subset,
    direction = "long",
    varying = varying_list, # order of variables matches v.names
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

  # Reorder columns: entity, time, then time-varying (v.names in order), then constants
  const_cols <- intersect(final_constant, names(long))
  long <- long[, c(entity_col, time_col, v.names, const_cols), drop = FALSE]

  # Sort by entity then time
  long <- long[order(long[[entity_col]], long[[time_col]]), ]
  rownames(long) <- NULL

  # --- Handle unbalanced panels: if all time-varying cols are NA for a row,
  #     set constant columns to NA as well.
  if (length(v.names) > 0) {
    all_na <- rowSums(is.na(long[, v.names, drop = FALSE])) == length(v.names)
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
