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
#' time‑invariant and are replicated for each time period.
#'
#' The reshaped columns are ordered as follows: the entity column appears first,
#' then the time column, then static columns, and finally the reshaped variables
#' in the order of `select`. Time periods are sorted naturally.
#'
#' If some variable‑time combinations are missing, the function adds those
#' columns filled with `NA`.
#'
#' The resulting time column is converted to the most appropriate type.
#'
#' Upon successful reshaping, a summary message is printed with aligned headers:
#' - `Static variables:` (indented to align the colon with `Reshaped variables:`)
#' - `Reshaped variables:` (the stubs)
#'
#' The returned object has class `"panel_data"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List with components:
#'         \describe{
#'           \item{`reshaped_variables`}{character vector of the stubs}
#'           \item{`static_variables`}{character vector of time‑invariant columns}
#'         }
#'   }
#' }
#'
#' @note
#' The input wide data must have one row per entity (unique entity values).
#' Column names must follow a consistent pattern. If `spacer = ""`, heuristic
#' detection is used; all‑digit column names are not recognized – use a non‑empty
#' spacer and proper variable names.
#'
#' @seealso [make_panel()], [make_wide()], [make_balanced()]
#'
#' @examples
#' data(production)
#' wide <- make_wide(production, select = c("sales", "labor"),
#'                   index = c("firm", "year"))
#' long <- make_long(wide, select = c("sales", "labor"),
#'                   index = c("firm", "year"))
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
  # ---- Input validation ----
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1], call. = FALSE)
  }
  if (missing(select) || is.null(select)) {
    stop("'select' must be provided and cannot be NULL", call. = FALSE)
  }
  if (!is.character(select) || length(select) == 0) {
    stop("'select' must be a non-empty character vector", call. = FALSE)
  }
  if (any(select == "")) {
    stop("'select' cannot contain empty strings", call. = FALSE)
  }
  if (!is.character(spacer) || length(spacer) != 1) {
    stop("'spacer' must be a single character string", call. = FALSE)
  }
  if (!is.logical(invert) || length(invert) != 1) {
    stop("'invert' must be a single logical value", call. = FALSE)
  }

  # ---- Extract index ----
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
  if (entity_col %in% select) {
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

  # ---- Parse columns ----
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
              if (var_name != "") {
                parsed[[col]] <- list(var = var_name, time = tpart)
                time_values <- c(time_values, tpart)
                found <- TRUE
                break
              }
            }
          } else {
            if (endsWith(col, tpart) && nchar(col) > nchar(tpart)) {
              var_name <- substr(col, 1, nchar(col) - nchar(tpart))
              if (var_name != "") {
                parsed[[col]] <- list(var = var_name, time = tpart)
                time_values <- c(time_values, tpart)
                found <- TRUE
                break
              }
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
                    parsed[[col]] <- list(var = vpart, time = tpart)
                    time_values <- c(time_values, tpart)
                    found <- TRUE
                    break
                  }
                }
              } else {
                if (startsWith(col, vpart) && nchar(col) > nchar(vpart)) {
                  tpart <- substr(col, nchar(vpart) + 1, nchar(col))
                  if (tpart %in% var_map[[vpart]]) {
                    parsed[[col]] <- list(var = vpart, time = tpart)
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

    # Fallback: numeric-only suffixes/prefixes (skip bare columns)
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
          if (var_name != "") {
            parsed[[col]] <- list(var = var_name, time = time_val)
            time_values <- c(time_values, time_val)
          }
        } else {
          match <- regexpr("\\d+$", col)
          if (match == -1) {
            next
          }
          time_val <- regmatches(col, match)
          var_name <- substr(col, 1, match - 1)
          if (var_name != "") {
            parsed[[col]] <- list(var = var_name, time = time_val)
            time_values <- c(time_values, time_val)
          }
        }
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
      if (var_name != "") {
        parsed[[col]] <- list(var = var_name, time = time_val)
        time_values <- c(time_values, time_val)
      }
    }
  }

  # ---- Early check: if no columns parsed ----
  if (length(parsed) == 0) {
    stop(
      "No columns were identified as time-varying based on the current settings.\n",
      "Check 'select', 'spacer', and 'invert' arguments.",
      call. = FALSE
    )
  }

  # ---- Filter parsed to those with var in select ----
  keep <- sapply(parsed, function(x) x$var %in% select)
  if (is.list(keep)) {
    keep <- unlist(keep)
  }
  parsed <- parsed[keep]
  if (length(parsed) == 0) {
    stop(
      "No columns found that match the variable stubs in 'select'.\n",
      "Check 'select', 'spacer', and 'invert' settings, and ensure that the column names\n",
      "follow the pattern variable + spacer + time (or time + spacer + variable).",
      call. = FALSE
    )
  }

  # Verify every stub appears at least once
  present_stubs <- unique(sapply(parsed, function(x) x$var))
  missing_stubs <- setdiff(select, present_stubs)
  if (length(missing_stubs) > 0) {
    stop(
      "The following stubs in 'select' were not found in any column: ",
      paste(missing_stubs, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- Check invert (warning) ----
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

  # ---- Sort time values ----
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

  # ---- Group by variable ----
  var_to_cols <- list()
  for (col in names(parsed)) {
    var <- parsed[[col]]$var
    var_to_cols[[var]] <- c(var_to_cols[[var]], col)
  }

  vars_in_order <- select[select %in% names(var_to_cols)]

  # Helper to generate column name
  generate_colname <- function(var, time, spacer, invert) {
    if (spacer == "") {
      if (invert) paste0(time, var) else paste0(var, time)
    } else {
      if (invert) paste0(time, spacer, var) else paste0(var, spacer, time)
    }
  }

  # ---- Build expected column names ----
  varying_list <- list()
  v.names <- c()
  varying_cols <- c()
  for (var in vars_in_order) {
    cols_for_var <- sapply(unique_times, function(t) {
      generate_colname(var, t, spacer, invert)
    })
    varying_list[[var]] <- cols_for_var
    v.names <- c(v.names, var)
    varying_cols <- c(varying_cols, cols_for_var)
  }

  # ---- Conflict check ----
  existing_tv_cols <- names(parsed)
  non_tv_cols <- setdiff(names(data), c(entity_col, existing_tv_cols))
  conflict_cols <- intersect(varying_cols, non_tv_cols)
  if (length(conflict_cols) > 0) {
    stop(
      "The following columns appear to be time-varying but do not match the expected pattern:\n",
      paste(conflict_cols, collapse = ", "),
      "\n",
      "They may use a different separator or have inconsistent naming.\n",
      "Check 'select', 'spacer', and 'invert' settings.",
      call. = FALSE
    )
  }

  # ---- Add missing columns ----
  added_cols <- character(0)
  for (var in vars_in_order) {
    existing_cols <- var_to_cols[[var]]
    first_col <- if (length(existing_cols) > 0) existing_cols[1] else NULL
    for (t in unique_times) {
      colname <- generate_colname(var, t, spacer, invert)
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

  # ---- Detect constant columns ----
  final_constant <- setdiff(names(data), c(entity_col, varying_cols))

  # ---- Time column conflict ----
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

  # ---- Reshape ----
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

  # ---- Convert time column to appropriate type ----
  time_char <- as.character(long[[time_col]])
  time_num <- suppressWarnings(as.numeric(time_char))
  if (all(!is.na(time_num))) {
    if (all(time_num == as.integer(time_num))) {
      long[[time_col]] <- as.integer(time_num)
    } else {
      long[[time_col]] <- time_num
    }
  }

  # ---- Order columns ----
  const_cols <- intersect(final_constant, names(long))
  long <- long[, c(entity_col, time_col, const_cols, v.names), drop = FALSE]

  # ---- Sort ----
  long <- long[order(long[[entity_col]], long[[time_col]]), ]
  rownames(long) <- NULL

  # ---- Mark constant as NA if all varying are missing ----
  if (length(v.names) > 0) {
    all_na <- rowSums(is.na(long[, v.names, drop = FALSE])) == length(v.names)
    if (any(all_na)) {
      long[all_na, const_cols] <- NA
    }
  }

  # ---- Metadata and details ----
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

  # ---- Print summary ----
  wrap_vars <- function(
    vars,
    prefix,
    cont_indent,
    max_width = getOption("width") - 2
  ) {
    if (length(vars) == 0) {
      return(paste0(prefix, "none"))
    }
    full_str <- paste(vars, collapse = ", ")
    if (nchar(prefix) + nchar(full_str) <= max_width) {
      return(paste0(prefix, full_str))
    }
    lines <- c()
    current_line <- prefix
    for (i in seq_along(vars)) {
      item <- vars[i]
      if (i == 1) {
        if (nchar(current_line) + nchar(item) + 2 <= max_width) {
          current_line <- paste0(current_line, item)
        } else {
          lines <- c(lines, current_line)
          current_line <- paste0(cont_indent, item)
        }
      } else {
        if (nchar(current_line) + nchar(item) + 2 <= max_width) {
          current_line <- paste0(current_line, ", ", item)
        } else {
          lines <- c(lines, current_line)
          current_line <- paste0(cont_indent, item)
        }
      }
    }
    lines <- c(lines, current_line)
    return(paste(lines, collapse = "\n"))
  }

  prefix_static <- paste0(
    strrep(" ", 9 - nchar("Static ")),
    "Static variables: "
  )
  prefix_reshaped <- "Reshaped variables: "
  cont_indent <- paste0(rep(" ", nchar(prefix_reshaped)), collapse = "")

  static_line <- wrap_vars(final_constant, prefix_static, cont_indent)
  cat(static_line, "\n")

  reshaped_line <- wrap_vars(v.names, prefix_reshaped, cont_indent)
  cat(reshaped_line, "\n")
  cat("\n")

  return(long)
}
