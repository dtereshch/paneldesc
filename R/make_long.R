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
#' The structure of the returned data.frame depends on the input. For example,
#' suppose your wide panel data contains an entity column `id`, time‑varying
#' variables `y` and `x` with time periods 2000 and 2001 (columns `y_2000`,
#' `y_2001`, `x_2000`, `x_2001`), and a static variable `z`. If you specify
#' `select = c("y", "x")`, the resulting long data.frame will have multiple rows
#' per entity, one for each time period, with columns `id`, `year`, `z`, `y`, and `x`.
#' All columns not in `select` (and not the entity column) are treated as
#' time‑invariant and are replicated for each time period.
#'
#' The reshaped columns are ordered as follows: the entity column appears first,
#' then the time column, then any static (time‑invariant) columns, and finally
#' all time‑varying variables in the order they appear in `select`.
#' The time periods are sorted according to their natural order
#' (numerically if all values are numeric, otherwise lexicographically).
#' **If numeric sorting would produce duplicate representations (e.g., `"01"` and
#' `"1"` map to the same number), the function stops with an error to prevent
#' ambiguous column mapping.**
#'
#' If some variable‑time combinations are missing from the wide format (i.e., a
#' column for a variable and a particular time period is absent), the function
#' adds that column filled with `NA` so that the variable is preserved in the
#' long format. A message is printed indicating which columns were added.
#'
#' Columns that are time‑invariant (not selected) are replicated for each time
#' period, unless a particular entity‑time combination has no time‑varying data
#' at all—in which case the invariant columns are set to `NA` to reflect a truly
#' missing observation.
#'
#' The function tries to convert the resulting time column to numeric if all time
#' values are coercible, and sorts the output by entity then time.
#'
#' The returned object has class `"panel_data"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.
#'         If the input was a `panel_wide` object, the original metadata elements
#'         for the entity and time variables are preserved; `spacer` and `invert`
#'         must be supplied explicitly when they differ from the defaults.}
#'   \item{`details`}{List containing the names of reshaped variables (`select`) and detected static variables.}
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
#' or a consistent numeric time pattern. If these conditions are met, the reshaping
#' will be reliable.
#'
#' The input wide data must have unique entity values; each row must
#' correspond to a distinct entity. If duplicates are found, the function stops
#' with an error listing the number of duplicate entities and up to five examples.
#'
#' When `spacer = ""` (no separator), the function first tries to identify
#' time suffixes that appear with multiple different variables. If that fails,
#' it then looks for variable stems that appear with multiple different time
#' suffixes, requiring those times to be either all numeric or all of the same
#' character length (e.g., `"Q1"`, `"Q2"`). If still no structure is found, it
#' falls back to detecting leading or trailing digits. The automatic detection
#' is a best‑effort heuristic; if the structure is irregular, consider using a
#' non‑empty `spacer` or renaming columns.
#'
#' **Columns with all‑digit names** – When column names consist entirely of
#' digits (e.g., `"2000"`, `"2001"`) and `spacer = ""`, the automatic parsing
#' treats them as time values of a single unnamed variable. The resulting
#' variable in long format will be named `"value"`. To use this feature, set
#' `select = ""`. If you have several different time‑varying variables whose names
#' are all digits, you should use an explicit `spacer` (e.g., `"_"`) to disambiguate
#' variable and time components, renaming columns to a pattern like
#' `"var1_2000"`, `"var2_2000"`, etc.
#'
#' A warning is issued when the automatic separator detection encounters columns
#' that look like they could be time‑varying but use a separator different from
#' `spacer`. These columns are treated as time‑invariant. To avoid confusion,
#' ensure a consistent naming scheme.
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
#' # Using spacer = "" (no separator) – automatic suffix detection
#' wide3 <- make_wide(production, select = c("sales", "labor"),
#'                    index = c("firm", "year"), spacer = "")
#' long3 <- make_long(wide3, select = c("sales", "labor"), spacer = "")
#'
#' # All‑digit column names (single variable "value") – use select = ""
#' wide_num <- data.frame(id = 1:3, `2000` = c(1,2,3), `2001` = c(4,5,6), check.names = FALSE)
#' long_num <- make_long(wide_num, select = "", index = c("id", "year"), spacer = "")
#'
#' # Accessing attributes
#' attr(long3, "metadata")
#' attr(long3, "details")
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
  # Allow select = "" (single empty string) as a special case
  if (length(select) == 1 && select == "") {
    select_special <- TRUE
  } else {
    select_special <- FALSE
    if (any(select == "")) {
      stop(
        "'select' cannot contain an empty string unless it is the only element",
        call. = FALSE
      )
    }
  }
  if (!is.character(spacer) || length(spacer) != 1) {
    stop("'spacer' must be a single character string", call. = FALSE)
  }
  if (!is.logical(invert) || length(invert) != 1) {
    stop("'invert' must be a single logical value", call. = FALSE)
  }

  entity_col <- NULL
  time_col <- NULL
  keep_panel_class <- FALSE
  panel_metadata <- NULL

  # --- Extract metadata from panel_wide object (if present) ---
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

  # --- Validate entity column ---
  if (!entity_col %in% names(data)) {
    stop('entity column "', entity_col, '" not found in data', call. = FALSE)
  }

  # --- Validate select: stubs cannot be entity column (except special empty) ---
  if (!select_special && entity_col %in% select) {
    stop(
      "'select' cannot contain the entity column '",
      entity_col,
      "'",
      call. = FALSE
    )
  }

  # --- Duplicate entity check ---
  if (any(duplicated(data[[entity_col]]))) {
    dup_entities <- unique(data[[entity_col]][duplicated(data[[entity_col]])])
    n_dup <- length(dup_entities)
    examples <- utils::head(dup_entities, 5)
    example_str <- paste(as.character(examples), collapse = ", ")
    stop(
      "Duplicate entity values found: ",
      n_dup,
      " unique entities appear more than once. ",
      "Examples: ",
      example_str,
      call. = FALSE
    )
  }

  all_cols <- names(data)
  candidates <- setdiff(all_cols, entity_col)

  # --- Parse columns to identify time-varying ones ---
  parsed <- list()
  time_values <- c()

  if (spacer == "") {
    # ---- No explicit separator: detect time substrings automatically ----
    # First strategy: find time parts that appear with ≥2 distinct variable names
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
      time_to_vars[lengths(time_to_vars) > 1] # keep only time parts with ≥2 distinct variables
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

    # Second strategy: if nothing found, try variable parts that appear with ≥2 distinct times
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
        var_to_times[lengths(var_to_times) > 1] # keep var parts with ≥2 distinct times
      }

      var_map <- detect_var_parts(candidates, invert)
      if (length(var_map) > 0) {
        # Consistency filter: for each stem, times must be all-numeric or all same-length
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

    # Fallback: if still no structure found, try numeric-only suffixes/prefixes
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
          if (bare) var_name <- "" # store empty, will be handled later
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
    # ---- Explicit separator: split on the separator ----
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

  # --- Handle special case select = "" (bare columns) ---
  if (select_special) {
    # Keep only bare columns
    bare_entries <- parsed[sapply(parsed, function(x) x$bare)]
    if (length(bare_entries) == 0) {
      stop(
        "No bare columns (columns consisting solely of time values) found. ",
        "If you intended to reshape columns with variable names, use a non-empty 'select'.",
        call. = FALSE
      )
    }
    # Rename variable to "value" for all these entries
    for (col in names(bare_entries)) {
      bare_entries[[col]]$var <- "value"
      bare_entries[[col]]$bare <- FALSE # not bare anymore after renaming
    }
    # Override parsed with these entries, and set select to "value"
    parsed <- bare_entries
    select <- "value"
    # time_values already contain the time parts from these columns
  } else {
    # Normal case: keep only parsed entries whose var is in select
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

  # --- Check that every stub in select appears in at least one column (skip for special case handled) ---
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

  # --- Check for correct invert specification ---
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
      "%).\n",
      "This often happens when the order of variable and time components is swapped.\n",
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

  # --- Group parsed columns by variable name ---
  var_to_cols <- list()
  for (col in names(parsed)) {
    var <- parsed[[col]]$var
    var_to_cols[[var]] <- c(var_to_cols[[var]], col)
  }

  # --- Preserve order: if select_special, order is just "value"; otherwise use order from select ---
  if (select_special) {
    vars_in_order <- "value"
  } else {
    vars_in_order <- select[select %in% names(var_to_cols)]
  }

  # --- Determine which vars are "bare" (column name = time only) - after renaming, none should be bare ---
  # However, if we had bare columns and select_special, we set bare=FALSE, so all are non-bare.
  # For normal case, we keep the bare flag as is (they are always FALSE for non-bare).
  bare_vars <- sapply(vars_in_order, function(var) {
    all(sapply(var_to_cols[[var]], function(col) parsed[[col]]$bare))
  })
  names(bare_vars) <- vars_in_order

  # --- Helper to generate a column name from var, time, spacer, invert, and bare flag ---
  generate_colname <- function(var, time, spacer, invert, bare = FALSE) {
    if (bare) {
      return(as.character(time))
    } else {
      if (spacer == "") {
        if (invert) paste0(time, var) else paste0(var, time)
      } else {
        if (invert) paste0(time, spacer, var) else paste0(var, spacer, time)
      }
    }
  }

  # --- Add missing columns for unbalanced variables ---
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

  # --- Build varying list for stats::reshape ---
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

  # --- Detect conflicts between generated time-varying column names and existing constant columns ---
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

  # --- Identify constant (time‑invariant) columns ---
  final_constant <- setdiff(names(data), c(entity_col, varying_cols))

  # Report static variables
  if (length(final_constant) > 0) {
    message(
      "Variables treated as time-invariant: ",
      paste(final_constant, collapse = ", ")
    )
  }

  # --- Check for time column name conflict ---
  if (time_col %in% c(entity_col, final_constant, varying_cols)) {
    stop(
      "The chosen time column name '",
      time_col,
      "' already exists in the wide data as a constant or varying column. ",
      "Please choose a different name for the time column using the 'index' argument.",
      call. = FALSE
    )
  }

  # --- Ensure reshaped variable names do not conflict with existing columns ---
  overlap <- intersect(v.names, c(entity_col, time_col, final_constant))
  if (length(overlap) > 0) {
    stop(
      "Reshaped variable name(s) conflict with existing column names: ",
      paste(overlap, collapse = ", "),
      ". Please rename the variables or choose different entity/time column names.",
      call. = FALSE
    )
  }

  # --- Reshape to long format ---
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

  # Convert time column to numeric only if all values can be converted without loss
  time_char <- as.character(long[[time_col]])
  time_num_test <- suppressWarnings(as.numeric(time_char))
  if (all(!is.na(time_num_test))) {
    long[[time_col]] <- time_num_test
  }

  # Order columns: entity, time, static, time‑varying (in order of select)
  const_cols <- intersect(final_constant, names(long))
  long <- long[, c(entity_col, time_col, const_cols, v.names), drop = FALSE]

  # Sort by entity then time
  long <- long[order(long[[entity_col]], long[[time_col]]), ]
  rownames(long) <- NULL

  # Mark constant columns as NA when all time‑varying data for a row are missing
  if (length(v.names) > 0) {
    all_na <- rowSums(is.na(long[, v.names, drop = FALSE])) == length(v.names)
    if (any(all_na)) {
      long[all_na, const_cols] <- NA
    }
  }

  # --- Build metadata and details attributes ---
  if (keep_panel_class) {
    new_metadata <- panel_metadata
    new_metadata$function_name <- "make_long"
    new_metadata$spacer <- spacer
    new_metadata$invert <- invert
    new_metadata$entity <- entity_col
    new_metadata$time <- time_col
    new_metadata$select <- select # will be "value" in special case
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
    reshaped = v.names,
    static_detected = final_constant
  )

  attr(long, "metadata") <- new_metadata
  attr(long, "details") <- new_details
  class(long) <- c("panel_data", class(long))

  return(long)
}
