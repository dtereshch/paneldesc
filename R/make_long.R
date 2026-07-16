#' Convert Panel Data from Wide to Long Format
#'
#' This function reshapes panel data from wide format to long format,
#' stacking selected time‑varying columns into rows based on the pattern of column names.
#'
#' @param data A data.frame containing panel data in a wide format. If `data` has
#'        class `"panel_wide"` (from [make_wide()]), the entity and time variables
#'        are taken from its metadata unless overridden by `index`.
#' @param select A character vector specifying the stubs (variable names) of the
#'        time‑varying columns to reshape. For example, if the wide data contains
#'        columns `x_2000`, `x_2001`, `y_2000`, `y_2001`, then `select = c("x", "y")`.
#'        If columns consist solely of time values (e.g., `"2001"`, `"2002"`), you
#'        can pass `select = ""` to reshape them as a single variable named `"value"`.
#'        This argument has no default and must be provided explicitly.
#' @param index A character vector of length 2 specifying the name of the
#'        entity column (first element) and the name to give to the new time
#'        column in the long format (second element).
#'        If not specified and `data` is a `panel_wide` object, the entity and time
#'        values are extracted from the metadata.
#' @param spacer A character string used to separate variable names and time
#'        values in the wide column names. Default = `"_"`. Use `""` (empty string)
#'        when no explicit separator exists; the function will then try to
#'        identify repeated numeric or non‑numeric time substrings.
#' @param invert A logical flag indicating the order of components in column
#'        names. If `FALSE`, column names are `"variable_spacer_time"`; if `TRUE`,
#'        they are `"time_spacer_variable"`. Default = `FALSE`.
#'
#' @return A data.frame containing panel data in a long format, with class
#'         `"panel_data"` and the attributes `metadata` and `details`.
#'
#' @details
#' The function converts data from wide to long format. Below is an illustration
#' of the transformation for two time periods (t = 1, 2) and two time‑varying
#' variables (`x`, `y`) plus a static variable `z`.
#'
#' **Wide format (input):**
#'
#' | id | x_1 | x_2 | y_1 | y_2 | z |
#' |----|-----|-----|-----|-----|---|
#' | 1  | 5   | 7   | 8   | 9   | A |
#' | 2  | 6   | 8   | 7   | 6   | B |
#'
#' **Long format (output) with `spacer = "_"` and `invert = FALSE`:**
#'
#' | id | t | x | y | z |
#' |----|---|---|---|---|
#' | 1  | 1 | 5 | 8 | A |
#' | 1  | 2 | 7 | 9 | A |
#' | 2  | 1 | 6 | 7 | B |
#' | 2  | 2 | 8 | 6 | B |
#'
#' All columns not in `select` (or the entity column) are treated as
#' time‑invariant and are replicated for each time period. The function
#' does **not** verify invariance because wide data cannot be checked;
#' it relies on the user's judgment.
#'
#' The reshaped columns are ordered as follows: the entity column appears first,
#' then the time column, then any static (time‑invariant) columns, and finally
#' all time‑varying variables in the order they appear in `select`.
#' The time periods are sorted according to their natural order
#' (numerically if all values are numeric, otherwise lexicographically).
#' If numeric sorting would produce duplicate representations (e.g., `"01"` and
#' `"1"` map to the same number), the function stops with an error.
#'
#' If some variable‑time combinations are missing from the wide format, the
#' function adds those columns filled with `NA` and prints a message.
#'
#' The resulting time column is converted to the most appropriate type:
#' - If all time values are integers, the column becomes `integer`.
#' - If they are numeric but not integers, it becomes `double`.
#' - Otherwise, it remains `character`.
#'
#' Upon successful reshaping, a summary message is printed:
#' - `Reshaped variables:` the stubs (long‑form variable names).
#' - `Static variables:` the variables treated as time‑invariant.
#'
#' The returned object has class `"panel_data"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.
#'         If the input was a `panel_wide` object, the original metadata elements
#'         for the entity and time variables are preserved.}
#'   \item{`details`}{List with two components: `reshaped_variables` (character vector
#'         of the long‑form variable stubs) and `static_variables` (character vector
#'         of time‑invariant variables).}
#' }
#'
#' @note
#' **Desirable input data** – The input wide data.frame should have one row per
#' entity and column names that follow a consistent naming convention. Time‑varying
#' columns must use the same separator (if `spacer` is not empty) and the same
#' ordering of variable/time parts. The entity column must contain unique
#' identifiers (no duplicates). When `spacer = ""`, automatic detection works best
#' when column names consist of a variable name directly followed by a time suffix
#' (or preceded by a time prefix if `invert = TRUE`) using only letters, digits,
#' or a consistent numeric time pattern.
#'
#' The input wide data must have unique entity values; duplicates cause an error.
#'
#' When `spacer = ""`, the function uses heuristic detection; see the main package
#' documentation for details. A special case: if column names are purely numeric
#' (e.g., `"2000"`, `"2001"`), using `select = ""` reshapes them as a single
#' variable named `"value"`.
#'
#' A warning is issued when the automatic separator detection encounters columns
#' that use a different separator; those are treated as time‑invariant.
#'
#' @seealso
#' See also [make_panel()], [make_wide()], [make_balanced()].
#'
#' @examples
#' data(production)
#'
#' # First create a wide data frame
#' wide <- make_wide(production, select = c("sales", "labor"),
#'                   index = c("firm", "year"))
#'
#' # Basic usage
#' long <- make_long(wide, select = c("sales", "labor"),
#'                   index = c("firm", "year"))
#'
#' # With panel_wide object (uses metadata)
#' wide_panel <- make_wide(production, select = c("sales", "labor"),
#'                         index = c("firm", "year"))
#' long_panel <- make_long(wide_panel, select = c("sales", "labor"))
#'
#' # Custom spacer and inverted order
#' wide2 <- make_wide(production, select = c("sales", "labor"),
#'                    index = c("firm", "year"),
#'                    spacer = ".", invert = TRUE)
#' long2 <- make_long(wide2, select = c("sales", "labor"),
#'                    spacer = ".", invert = TRUE)
#'
#' # All‑digit column names (single variable "value") – use select = ""
#' wide_num <- data.frame(id = 1:3, `2000` = c(1,2,3), `2001` = c(4,5,6), check.names = FALSE)
#' long_num <- make_long(wide_num, select = "", index = c("id", "year"), spacer = "")
#'
#' # Accessing attributes
#' attr(long, "metadata")
#' attr(long, "details")
#'
#' @importFrom stats reshape
#' @export
make_long <- function(
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
    stop("'select' must be a character vector", call. = FALSE)
  }
  select_special <- (length(select) == 1 && select == "")
  if (!select_special && any(select == "")) {
    stop(
      "'select' cannot contain an empty string unless it is the only element",
      call. = FALSE
    )
  }
  if (!is.character(spacer) || length(spacer) != 1) {
    stop("'spacer' must be a single character string", call. = FALSE)
  }
  if (!is.logical(invert) || length(invert) != 1) {
    stop("'invert' must be a single logical value", call. = FALSE)
  }

  # --- Extract index ---
  entity_col <- NULL
  time_col <- NULL
  keep_panel_class <- FALSE
  panel_metadata <- NULL

  if (inherits(data, "panel_wide")) {
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

  # Validate entity
  if (!entity_col %in% names(data)) {
    stop('entity column "', entity_col, '" not found in data', call. = FALSE)
  }
  if (!select_special && entity_col %in% select) {
    stop(
      "'select' cannot contain the entity column '",
      entity_col,
      "'",
      call. = FALSE
    )
  }

  # Duplicate entity check
  if (any(duplicated(data[[entity_col]]))) {
    dup_entities <- unique(data[[entity_col]][duplicated(data[[entity_col]])])
    n_dup <- length(dup_entities)
    examples <- utils::head(dup_entities, 5)
    example_str <- paste(as.character(examples), collapse = ", ")
    stop(
      "Duplicate entity values found: ",
      n_dup,
      " unique entities appear more than once. Examples: ",
      example_str,
      call. = FALSE
    )
  }

  all_cols <- names(data)
  candidates <- setdiff(all_cols, entity_col)

  # --- Parse columns ---
  parsed <- list()
  time_values <- c()

  if (spacer == "") {
    # First strategy: time parts with ≥2 variables
    detect_time_parts <- function(col_names, invert) {
      time_to_vars <- list()
      for (nm in col_names) {
        len <- nchar(nm)
        if (len < 2) {
          next
        }
        if (invert) {
          for (i in 1:(len - 1)) {
            tpart <- substr(nm, 1, i)
            vpart <- substr(nm, i + 1, len)
            time_to_vars[[tpart]] <- unique(c(time_to_vars[[tpart]], vpart))
          }
        } else {
          for (i in 1:(len - 1)) {
            tpart <- substr(nm, len - i + 1, len)
            vpart <- substr(nm, 1, len - i)
            time_to_vars[[tpart]] <- unique(c(time_to_vars[[tpart]], vpart))
          }
        }
      }
      time_to_vars[lengths(time_to_vars) > 1]
    }

    time_map <- detect_time_parts(candidates, invert)
    if (length(time_map) > 0) {
      sorted_times <- names(time_map)[order(
        nchar(names(time_map)),
        decreasing = TRUE
      )]
      for (col in candidates) {
        found <- FALSE
        for (tpart in sorted_times) {
          if (invert) {
            if (startsWith(col, tpart) && nchar(col) > nchar(tpart)) {
              var_name <- substr(col, nchar(tpart) + 1, nchar(col))
              parsed[[col]] <- list(var = var_name, time = tpart, bare = FALSE)
              time_values <- c(time_values, tpart)
              found <- TRUE
              break
            }
          } else {
            if (endsWith(col, tpart) && nchar(col) > nchar(tpart)) {
              var_name <- substr(col, 1, nchar(col) - nchar(tpart))
              parsed[[col]] <- list(var = var_name, time = tpart, bare = FALSE)
              time_values <- c(time_values, tpart)
              found <- TRUE
              break
            }
          }
        }
      }
    }

    # Second strategy: var parts with ≥2 times
    if (length(parsed) == 0) {
      detect_var_parts <- function(col_names, invert) {
        var_to_times <- list()
        for (nm in col_names) {
          len <- nchar(nm)
          if (len < 2) {
            next
          }
          if (invert) {
            for (i in 1:(len - 1)) {
              tpart <- substr(nm, 1, i)
              vpart <- substr(nm, i + 1, len)
              var_to_times[[vpart]] <- unique(c(var_to_times[[vpart]], tpart))
            }
          } else {
            for (i in 1:(len - 1)) {
              vpart <- substr(nm, 1, i)
              tpart <- substr(nm, i + 1, len)
              var_to_times[[vpart]] <- unique(c(var_to_times[[vpart]], tpart))
            }
          }
        }
        var_to_times[lengths(var_to_times) > 1]
      }

      var_map <- detect_var_parts(candidates, invert)
      if (length(var_map) > 0) {
        keep <- logical(length(var_map))
        names(keep) <- names(var_map)
        for (v in names(var_map)) {
          times <- var_map[[v]]
          num_times <- suppressWarnings(as.numeric(times))
          if (all(!is.na(num_times))) {
            keep[v] <- TRUE
          } else {
            keep[v] <- length(unique(nchar(times))) == 1
          }
        }
        var_map <- var_map[keep]
        if (length(var_map) > 0) {
          sorted_vars <- names(var_map)[order(
            nchar(names(var_map)),
            decreasing = TRUE
          )]
          for (col in candidates) {
            found <- FALSE
            for (vpart in sorted_vars) {
              if (invert) {
                if (endsWith(col, vpart) && nchar(col) > nchar(vpart)) {
                  tpart <- substr(col, 1, nchar(col) - nchar(vpart))
                  if (tpart %in% var_map[[vpart]]) {
                    parsed[[col]] <- list(
                      var = vpart,
                      time = tpart,
                      bare = FALSE
                    )
                    time_values <- c(time_values, tpart)
                    found <- TRUE
                    break
                  }
                }
              } else {
                if (startsWith(col, vpart) && nchar(col) > nchar(vpart)) {
                  tpart <- substr(col, nchar(vpart) + 1, nchar(col))
                  if (tpart %in% var_map[[vpart]]) {
                    parsed[[col]] <- list(
                      var = vpart,
                      time = tpart,
                      bare = FALSE
                    )
                    time_values <- c(time_values, tpart)
                    found <- TRUE
                    break
                  }
                }
              }
            }
          }
        }
      }
    }

    # Fallback: numeric-only suffixes/prefixes
    if (length(parsed) == 0) {
      message(
        "No time structure detected with automatic suffix/prefix detection. ",
        "Falling back to numeric-only time values. Consider using a non-empty 'spacer'."
      )
      for (col in candidates) {
        if (invert) {
          match <- regexpr("^\\d+", col)
          if (match == -1) {
            next
          }
          time_val <- regmatches(col, match)
          var_name <- substr(
            col,
            match + attr(match, "match.length"),
            nchar(col)
          )
          bare <- (var_name == "")
          if (bare) var_name <- ""
        } else {
          match <- regexpr("\\d+$", col)
          if (match == -1) {
            next
          }
          time_val <- regmatches(col, match)
          var_name <- substr(col, 1, match - 1)
          bare <- (var_name == "")
          if (bare) var_name <- ""
        }
        parsed[[col]] <- list(var = var_name, time = time_val, bare = bare)
        time_values <- c(time_values, time_val)
      }
    }
  } else {
    # Explicit separator
    for (col in candidates) {
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
      parsed[[col]] <- list(var = var_name, time = time_val, bare = FALSE)
      time_values <- c(time_values, time_val)
    }
  }

  # --- Special case select = "" ---
  if (select_special) {
    bare_entries <- parsed[sapply(parsed, function(x) x$bare)]
    if (length(bare_entries) == 0) {
      stop(
        "No bare columns (columns consisting solely of time values) found. ",
        "If you intended to reshape columns with variable names, use a non-empty 'select'.",
        call. = FALSE
      )
    }
    for (col in names(bare_entries)) {
      bare_entries[[col]]$var <- "value"
      bare_entries[[col]]$bare <- FALSE
    }
    parsed <- bare_entries
    select <- "value"
  } else {
    parsed <- parsed[sapply(parsed, function(x) x$var %in% select)]
  }

  if (length(parsed) == 0) {
    stop(
      "No columns found that match the variable stubs in 'select'.\n",
      "Check 'spacer' and 'invert' settings, and ensure that the column names ",
      "follow the pattern variable + spacer + time (or time + spacer + variable).",
      call. = FALSE
    )
  }

  if (!select_special) {
    present_stubs <- unique(sapply(parsed, function(x) x$var))
    missing_stubs <- setdiff(select, present_stubs)
    if (length(missing_stubs) > 0) {
      stop(
        "The following stubs in 'select' were not found in any column: ",
        paste(missing_stubs, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # --- Check invert (warning) ---
  var_names <- sapply(parsed, function(x) x$var, USE.NAMES = FALSE)
  time_vals <- sapply(parsed, function(x) x$time, USE.NAMES = FALSE)
  time_numeric <- suppressWarnings(as.numeric(time_vals))
  var_numeric <- suppressWarnings(as.numeric(var_names))
  prop_time_numeric <- sum(!is.na(time_numeric)) / length(time_vals)
  prop_var_numeric <- sum(!is.na(var_numeric)) / length(var_names)
  if (prop_time_numeric < 0.5 && prop_var_numeric > 0.5) {
    suggested_invert <- ifelse(invert, "FALSE", "TRUE")
    warning(
      "It appears that 'invert' may be incorrectly specified.\n",
      "Most extracted time values are non-numeric (",
      round((1 - prop_time_numeric) * 100),
      "%) while most variable names are numeric (",
      round(prop_var_numeric * 100),
      "%).\nThis often happens when the order of variable and time components is swapped.\n",
      "Consider setting 'invert = ",
      suggested_invert,
      "'.",
      call. = FALSE
    )
  }

  # --- Sort time values ---
  unique_times <- unique(time_values)
  time_nums <- suppressWarnings(as.numeric(unique_times))
  if (all(!is.na(time_nums))) {
    if (any(duplicated(time_nums))) {
      stop(
        "Time labels contain values that become identical after numeric conversion ",
        "(e.g., '01' and '1'). Please ensure unique numeric representations or treat times as character.",
        call. = FALSE
      )
    }
    unique_times <- unique_times[order(time_nums)]
  } else {
    unique_times <- sort(unique_times)
  }

  # --- Group by variable ---
  var_to_cols <- list()
  for (col in names(parsed)) {
    var <- parsed[[col]]$var
    var_to_cols[[var]] <- c(var_to_cols[[var]], col)
  }

  if (select_special) {
    vars_in_order <- "value"
  } else {
    vars_in_order <- select[select %in% names(var_to_cols)]
  }

  # Determine bare flags
  bare_vars <- sapply(vars_in_order, function(var) {
    all(sapply(var_to_cols[[var]], function(col) parsed[[col]]$bare))
  })
  names(bare_vars) <- vars_in_order

  # Helper to generate column name
  generate_colname <- function(var, time, spacer, invert, bare = FALSE) {
    if (bare) {
      return(as.character(time))
    }
    if (spacer == "") {
      if (invert) paste0(time, var) else paste0(var, time)
    } else {
      if (invert) paste0(time, spacer, var) else paste0(var, spacer, time)
    }
  }

  # Add missing columns
  added_cols <- character(0)
  for (var in vars_in_order) {
    existing_cols <- var_to_cols[[var]]
    first_col <- if (length(existing_cols) > 0) existing_cols[1] else NULL
    bare <- bare_vars[var]
    for (t in unique_times) {
      colname <- generate_colname(var, t, spacer, invert, bare = bare)
      if (!colname %in% names(data)) {
        if (!is.null(first_col)) {
          template <- data[[first_col]]
          if (is.factor(template)) {
            new_col <- factor(
              rep(NA, nrow(data)),
              levels = levels(template),
              ordered = is.ordered(template)
            )
            attrs <- attributes(template)
            attrs_to_copy <- attrs[setdiff(names(attrs), c("class", "levels"))]
            if (length(attrs_to_copy) > 0) {
              attributes(new_col) <- c(attributes(new_col), attrs_to_copy)
            }
          } else {
            if (any(!is.na(template))) {
              valid_idx <- which(!is.na(template))[1]
              template_val <- template[valid_idx]
              template_val[1] <- NA
            } else {
              template_val <- template[1]
            }
            new_col <- rep(template_val, nrow(data))
            attrs <- attributes(template)
            if (!is.null(attrs)) attributes(new_col) <- attrs
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
  }

  # --- Build varying list ---
  varying_list <- list()
  v.names <- c()
  varying_cols <- c()
  for (var in vars_in_order) {
    bare <- bare_vars[var]
    cols_for_var <- sapply(unique_times, function(t) {
      generate_colname(var, t, spacer, invert, bare = bare)
    })
    varying_list[[var]] <- cols_for_var
    v.names <- c(v.names, var)
    varying_cols <- c(varying_cols, cols_for_var)
  }

  # --- Conflicts and constant detection ---
  existing_tv_cols <- names(parsed)
  generated_new_cols <- setdiff(varying_cols, existing_tv_cols)
  constant_overlap <- intersect(
    generated_new_cols,
    setdiff(names(data), c(entity_col, existing_tv_cols))
  )
  if (length(constant_overlap) > 0) {
    stop(
      "Column name conflict: the following existing column(s) would be treated as time-varying ",
      "but are not recognized as such: ",
      paste(constant_overlap, collapse = ", "),
      ". Please rename these columns or adjust 'spacer'/'invert'.",
      call. = FALSE
    )
  }

  final_constant <- setdiff(names(data), c(entity_col, varying_cols))

  # Time column conflict
  if (time_col %in% c(entity_col, final_constant, varying_cols)) {
    stop(
      "The chosen time column name '",
      time_col,
      "' already exists in the wide data as a constant or varying column. ",
      "Please choose a different name for the time column using the 'index' argument.",
      call. = FALSE
    )
  }

  overlap <- intersect(v.names, c(entity_col, time_col, final_constant))
  if (length(overlap) > 0) {
    stop(
      "Reshaped variable name(s) conflict with existing column names: ",
      paste(overlap, collapse = ", "),
      ". Please rename the variables or choose different entity/time column names.",
      call. = FALSE
    )
  }

  # --- Reshape ---
  wide_subset <- data[,
    c(entity_col, final_constant, varying_cols),
    drop = FALSE
  ]
  long <- stats::reshape(
    wide_subset,
    direction = "long",
    varying = varying_list,
    v.names = v.names,
    timevar = time_col,
    idvar = entity_col,
    times = unique_times,
    new.row.names = NULL
  )
  rownames(long) <- NULL

  # --- Convert time column to appropriate type ---
  # The time column from reshape is character. Convert to numeric if possible,
  # and to integer if all values are whole numbers.
  time_char <- as.character(long[[time_col]])
  time_num <- suppressWarnings(as.numeric(time_char))
  if (all(!is.na(time_num))) {
    # All values are numeric
    if (all(time_num == as.integer(time_num))) {
      # All are integers, convert to integer
      long[[time_col]] <- as.integer(time_num)
    } else {
      long[[time_col]] <- time_num # keep as double
    }
  }
  # else keep as character

  # Order columns
  const_cols <- intersect(final_constant, names(long))
  long <- long[, c(entity_col, time_col, const_cols, v.names), drop = FALSE]

  # Sort
  long <- long[order(long[[entity_col]], long[[time_col]]), ]
  rownames(long) <- NULL

  # Mark constant as NA if all varying are missing
  if (length(v.names) > 0) {
    all_na <- rowSums(is.na(long[, v.names, drop = FALSE])) == length(v.names)
    if (any(all_na)) {
      long[all_na, const_cols] <- NA
    }
  }

  # --- Metadata and details ---
  if (keep_panel_class) {
    new_metadata <- panel_metadata
    new_metadata$function_name <- "make_long"
    new_metadata$spacer <- spacer
    new_metadata$invert <- invert
    new_metadata$entity <- entity_col
    new_metadata$time <- time_col
    new_metadata$select <- select
  } else {
    new_metadata <- list(
      function_name = "make_long",
      entity = entity_col,
      time = time_col,
      spacer = spacer,
      invert = invert,
      select = select
    )
  }

  new_details <- list(
    reshaped_variables = v.names,
    static_variables = final_constant
  )

  attr(long, "metadata") <- new_metadata
  attr(long, "details") <- new_details
  class(long) <- c("panel_data", class(long))

  # --- Print summary ---
  cat("Reshaped variables:", paste(v.names, collapse = ", "), "\n")
  if (length(final_constant) > 0) {
    cat("Static variables:", paste(final_constant, collapse = ", "), "\n")
  } else {
    cat("Static variables: none\n")
  }
  cat("\n")

  return(long)
}
