#' Convert Panel Data from Wide to Long Format
#'
#' This function reshapes panel data from wide format to long format.
#'
#' @param data A data.frame containing panel data in a wide format.
#' @param select A character vector specifying the stubs of the names of the
#'        time‑varying variables to reshape.
#' @param index A character vector of length 2 specifying the name of the
#'        entity column (first element) and the name to give to the new time
#'        column in the long format (second element).
#'        If not specified and `data` is a `panel_wide` object, the entity and time
#'        values are extracted from the metadata.
#' @param static An optional character vector of names of time‑invariant variables.
#' @param spacer A character string used to separate variable names and time
#'        values in the wide column names. Default = `"_"`.
#' @param invert A logical flag indicating the order of components in column
#'        names. If FALSE, column names are `variable + spacer + time`; if TRUE,
#'        they are `time + spacer + variable`. Default = FALSE.
#'
#' @return A data.frame containing panel data in a long format.
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
#' then the time column, then any static (time‑invariant) columns, and finally
#' all time‑varying variables in the order they appear in `select`.
#' The time periods are sorted according to their natural order
#' (numerically if all values are numeric, otherwise lexicographically).
#'
#' After reshaping, if a row has NA for all reshaped variables,
#' the static columns for that row are also set to NA.  This prevents
#' a false impression that the entity had a valid observation at that time
#' point (only time‑varying variables were missing).
#'
#' The resulting time column is converted to the most appropriate type:
#' if all time values are integers, the column becomes `integer`;
#' if they are numeric but not integers, it becomes `double`;
#' otherwise, it remains `character`.
#'
#' The returned object has class `"panel_data"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.
#'         If the input was a `panel_wide` object, the original metadata elements
#'         for the entity and time variables are preserved.}
#'   \item{`details`}{List containing character vectors of reshaped variables names
#'         and static variables names.}
#' }
#'
#' @note
#' The input wide data.frame should have column names that follow a consistent
#' naming convention. **It highly recommended to use a non‑empty uniform `spacer`
#' and explicit variable names.**
#' Purely numeric column names (e.g., `"2000"`, `"2001"`) are not recognised
#' as time‑varying.
#'
#' If no explicit separator exists and there is no better option, use `spacer = ""` (empty string).
#' In this case, treat the results with caution, as the function may not work perfectly.
#'
#' In an attempt to avoid cases of ambiguity, when `spacer = ""` the function splits
#' column names using the stubs given in `select` by matching the longest possible stub.
#' The remainder of the column name after removing the matched stub is taken as the time value.
#'
#' @seealso
#' See also [make_panel()], [make_wide()], [make_balanced()].
#'
#' @examples
#' data(production)
#'
#' # Define the time-varying variables to reshape (optional, for reuse)
#' vars <- c("sales", "capital", "labor", "industry", "ownership")
#'
#' # First create a wide data frame (using direct specification)
#' wide <- make_wide(production,
#'                   select = c("sales", "capital", "labor", "industry", "ownership"),
#'                   index = c("firm", "year"))
#'
#' # Direct selection of variables inside the function
#' long <- make_long(wide,
#'                   select = c("sales", "capital", "labor", "industry", "ownership"),
#'                   index = c("firm", "year"))
#'
#' # Using a pre-defined vector
#' long2 <- make_long(wide, select = vars, index = c("firm", "year"))
#'
#' # Custom spacer and inverted order
#' wide_custom <- make_wide(production,
#'                          select = vars,
#'                          index = c("firm", "year"),
#'                          spacer = ".", invert = TRUE)
#' long3 <- make_long(wide_custom,
#'                    select = vars,
#'                    spacer = ".", invert = TRUE)
#'
#' # With panel_wide object (uses metadata)
#' panel <- make_panel(production, index = c("firm", "year"))
#' wide_panel <- make_wide(panel, select = vars)
#' long_panel <- make_long(wide_panel, select = vars)
#'
#' # Using the `static` argument to explicitly mark a time‑invariant variable
#' long_region <- make_long(wide, select = vars, index = c("firm", "year"),
#'                          static = "region")
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
  static = NULL,
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
  if (anyDuplicated(select)) {
    dup_stubs <- unique(select[duplicated(select)])
    stop(
      "'select' must contain unique variable stubs. Duplicates found: ",
      paste(dup_stubs, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.character(spacer) || length(spacer) != 1) {
    stop("'spacer' must be a single character string", call. = FALSE)
  }
  if (!is.logical(invert) || length(invert) != 1) {
    stop("'invert' must be a single logical value", call. = FALSE)
  }

  # ---- Validate static ----
  if (!is.null(static)) {
    if (!is.character(static) || length(static) == 0) {
      stop(
        "'static' must be a non-empty character vector or NULL",
        call. = FALSE
      )
    }
    if (any(static == "")) {
      stop("'static' cannot contain empty strings", call. = FALSE)
    }
    if (anyDuplicated(static)) {
      dup_static <- unique(static[duplicated(static)])
      stop(
        "'static' must contain unique variable names. Duplicates found: ",
        paste(dup_static, collapse = ", "),
        call. = FALSE
      )
    }
    missing_static <- setdiff(static, names(data))
    if (length(missing_static) > 0) {
      stop(
        "The following variables in 'static' were not found in 'data': ",
        paste(missing_static, collapse = ", "),
        call. = FALSE
      )
    }
    overlap_select <- intersect(static, select)
    if (length(overlap_select) > 0) {
      stop(
        "'static' and 'select' must not intersect. Overlapping variables: ",
        paste(overlap_select, collapse = ", "),
        call. = FALSE
      )
    }
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

  if (!is.null(static)) {
    if (entity_col %in% static) {
      stop(
        "'static' cannot contain the entity column '",
        entity_col,
        "'",
        call. = FALSE
      )
    }
    if (time_col %in% static) {
      stop(
        "'static' cannot contain the time column name '",
        time_col,
        "'",
        call. = FALSE
      )
    }
  }

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
  static_names <- if (is.null(static)) character(0) else static
  stubs_sorted <- select[order(nchar(select), decreasing = TRUE)]

  for (col in candidates) {
    if (col %in% static_names) {
      next
    }

    if (spacer != "") {
      matched <- FALSE
      for (stub in stubs_sorted) {
        if (!invert) {
          pattern <- paste0(stub, spacer)
          if (startsWith(col, pattern) && nchar(col) > nchar(pattern)) {
            time_val <- substr(col, nchar(pattern) + 1, nchar(col))
            parsed[[col]] <- list(var = stub, time = time_val)
            time_values <- c(time_values, time_val)
            matched <- TRUE
            break
          }
        } else {
          pattern <- paste0(spacer, stub)
          if (endsWith(col, pattern) && nchar(col) > nchar(pattern)) {
            time_val <- substr(col, 1, nchar(col) - nchar(pattern))
            parsed[[col]] <- list(var = stub, time = time_val)
            time_values <- c(time_values, time_val)
            matched <- TRUE
            break
          }
        }
      }
      if (matched) next
    } else {
      for (stub in stubs_sorted) {
        if (!invert) {
          if (startsWith(col, stub) && nchar(col) > nchar(stub)) {
            time_val <- substr(col, nchar(stub) + 1, nchar(col))
            parsed[[col]] <- list(var = stub, time = time_val)
            time_values <- c(time_values, time_val)
            break
          }
        } else {
          if (endsWith(col, stub) && nchar(col) > nchar(stub)) {
            time_val <- substr(col, 1, nchar(col) - nchar(stub))
            parsed[[col]] <- list(var = stub, time = time_val)
            time_values <- c(time_values, time_val)
            break
          }
        }
      }
    }
  }

  if (length(parsed) == 0) {
    stop(
      "No columns were identified as time‑varying. ",
      "Check 'select', 'spacer', and 'invert'.",
      call. = FALSE
    )
  }

  present_stubs <- unique(sapply(parsed, `[[`, "var"))
  missing_stubs <- setdiff(select, present_stubs)
  if (length(missing_stubs) > 0) {
    if (spacer != "") {
      different_sep_found <- FALSE
      for (stub in missing_stubs) {
        candidates_cols <- grep(
          paste0("^", stub, "."),
          names(data),
          value = TRUE
        )
        if (length(candidates_cols) > 0) {
          different_sep_found <- TRUE
          break
        }
      }
      if (different_sep_found) {
        stop(
          "The following stubs in 'select' were not found: ",
          paste(missing_stubs, collapse = ", "),
          ". Different separators are detected. Check and fix variable names.",
          call. = FALSE
        )
      }
    }
    stop(
      "The following stubs in 'select' were not found in any column: ",
      paste(missing_stubs, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- Warn and rename static columns that match time‑varying patterns ----
  static_rename_map <- list()
  if (length(static_names) > 0) {
    maybe_varying <- character(0)
    for (col in static_names) {
      if (spacer != "") {
        for (stub in stubs_sorted) {
          if (!invert) {
            pattern <- paste0(stub, spacer)
            if (startsWith(col, pattern) && nchar(col) > nchar(pattern)) {
              maybe_varying <- c(maybe_varying, col)
              break
            }
          } else {
            pattern <- paste0(spacer, stub)
            if (endsWith(col, pattern) && nchar(col) > nchar(pattern)) {
              maybe_varying <- c(maybe_varying, col)
              break
            }
          }
        }
      } else {
        for (stub in stubs_sorted) {
          if (!invert) {
            if (startsWith(col, stub) && nchar(col) > nchar(stub)) {
              maybe_varying <- c(maybe_varying, col)
              break
            }
          } else {
            if (endsWith(col, stub) && nchar(col) > nchar(stub)) {
              maybe_varying <- c(maybe_varying, col)
              break
            }
          }
        }
      }
    }
    if (length(maybe_varying) > 0) {
      warning(
        "The following static variables have names that match time‑varying patterns: ",
        paste(maybe_varying, collapse = ", "),
        ". They will be treated as static (temporarily renamed for reshaping). Ensure this is intentional.",
        call. = FALSE
      )
      # Rename them to temporary names
      for (col in maybe_varying) {
        tmpname <- paste0(".static_", col)
        while (tmpname %in% names(data)) {
          tmpname <- paste0(tmpname, "_")
        }
        names(data)[names(data) == col] <- tmpname
        static_rename_map[[col]] <- tmpname
        # Update static_names so that subsequent checks treat the new name as static
        static_names[static_names == col] <- tmpname
      }
    }
  }

  # ---- Sort time values ----
  unique_times <- unique(time_values)
  time_nums <- suppressWarnings(as.numeric(unique_times))
  if (all(!is.na(time_nums))) {
    if (any(duplicated(time_nums))) {
      stop(
        "Time labels contain values that become identical after numeric conversion ",
        "(e.g., '01' and '1').",
        call. = FALSE
      )
    }
    unique_times <- unique_times[order(time_nums)]
  } else {
    unique_times <- sort(unique_times)
  }

  # ---- Group columns by variable ----
  var_to_cols <- list()
  for (col in names(parsed)) {
    var <- parsed[[col]]$var
    var_to_cols[[var]] <- c(var_to_cols[[var]], col)
  }
  vars_in_order <- select[select %in% names(var_to_cols)]

  # ---- Column name generator ----
  generate_colname <- function(var, time, spacer, invert) {
    if (spacer == "") {
      if (invert) paste0(time, var) else paste0(var, time)
    } else if (invert) {
      paste0(time, spacer, var)
    } else {
      paste0(var, spacer, time)
    }
  }

  # ---- Build expected column names ----
  # (Static columns that were renamed are now absent under their original names,
  #  so they will be added as NA placeholders.)
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

  # ---- Conflict detection ----
  existing_tv_cols <- names(parsed)
  non_tv_cols <- setdiff(names(data), c(entity_col, existing_tv_cols))
  conflict_cols <- intersect(varying_cols, non_tv_cols)
  if (length(conflict_cols) > 0) {
    stop(
      "The following columns appear to be time‑varying but do not match the expected pattern:\n",
      paste(conflict_cols, collapse = ", "),
      "\nCheck 'select', 'spacer', and 'invert'.",
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
      if (colname %in% names(data) || colname %in% static_names) {
        next
      }
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
          template_val <- template
          if (any(!is.na(template))) {
            valid_idx <- which(!is.na(template))[1]
            template_val <- template[valid_idx]
          }
          template_val[1] <- NA
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
  if (length(added_cols) > 0) {
    message(
      "Added the following columns (filled with NA) to complete unbalanced variables: ",
      paste(added_cols, collapse = ", ")
    )
  }

  # ---- Detect constant columns ----
  final_constant <- setdiff(names(data), c(entity_col, varying_cols))

  if (time_col %in% c(entity_col, final_constant, varying_cols)) {
    stop(
      "The chosen time column name '",
      time_col,
      "' already exists in the wide data as a constant or varying column. ",
      "Please choose a different name using 'index'.",
      call. = FALSE
    )
  }

  overlap <- intersect(v.names, c(entity_col, time_col, final_constant))
  if (length(overlap) > 0) {
    stop(
      "Reshaped variable name(s) conflict with existing column names: ",
      paste(overlap, collapse = ", "),
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

  # ---- Restore original names for temporarily renamed static columns ----
  if (length(static_rename_map) > 0) {
    for (orig in names(static_rename_map)) {
      tmp <- static_rename_map[[orig]]
      if (tmp %in% names(long)) {
        names(long)[names(long) == tmp] <- orig
      }
    }
    # Update constant column names accordingly
    final_constant <- setdiff(names(long), c(entity_col, time_col, v.names))
  }

  # ---- Check time‑invariance of static variables ----
  const_cols <- intersect(final_constant, names(long))
  if (length(const_cols) > 0) {
    violation_list <- list()
    entity_groups <- split(long, long[[entity_col]])
    for (var in const_cols) {
      bad_entities <- character(0)
      for (grp in entity_groups) {
        vals <- grp[[var]]
        if (length(unique(vals)) > 1) {
          bad_entities <- c(bad_entities, as.character(grp[[entity_col]][1]))
        }
      }
      if (length(bad_entities) > 0) {
        violation_list[[var]] <- bad_entities
      }
    }
    if (length(violation_list) > 0) {
      msg_lines <- "The following static variables are not time‑invariant for some entities:"
      for (v in names(violation_list)) {
        ents <- violation_list[[v]]
        msg_lines <- c(
          msg_lines,
          sprintf(
            "  %s (entities: %s)",
            v,
            paste(utils::head(ents, 5), collapse = ", ")
          )
        )
        if (length(ents) > 5) {
          msg_lines <- c(
            msg_lines,
            sprintf("    ... and %d more", length(ents) - 5)
          )
        }
      }
      warning(paste(msg_lines, collapse = "\n"), call. = FALSE)
    }
  }

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
  long <- long[, c(entity_col, time_col, const_cols, v.names), drop = FALSE]

  # ---- Sort ----
  long <- long[order(long[[entity_col]], long[[time_col]]), ]
  rownames(long) <- NULL

  # ---- NA handling for constant columns ----
  if (length(v.names) > 0) {
    all_na <- rowSums(is.na(long[, v.names, drop = FALSE])) == length(v.names)
    if (any(all_na)) long[all_na, const_cols] <- NA
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
    paste(lines, collapse = "\n")
  }

  prefix_static <- paste0(
    strrep(" ", 9 - nchar("Static ")),
    "Static variables: "
  )
  prefix_reshaped <- "Reshaped variables: "
  cont_indent <- paste0(rep(" ", nchar(prefix_reshaped)), collapse = "")

  # Prepare display names for static variables (using original names)
  user_static <- static # original user input
  auto_static <- setdiff(final_constant, user_static)

  if (!is.null(static) && length(auto_static) > 0) {
    display_static <- character(length(final_constant))
    for (i in seq_along(final_constant)) {
      v <- final_constant[i]
      if (v %in% user_static) {
        display_static[i] <- paste0(v, " (user-defined)")
      } else {
        display_static[i] <- paste0(v, " (auto-detected)")
      }
    }
    reorder <- order(match(
      final_constant,
      user_static,
      nomatch = length(final_constant)
    ))
    display_static <- display_static[reorder]
  } else {
    display_static <- final_constant
  }

  static_line <- wrap_vars(display_static, prefix_static, cont_indent)
  cat(static_line, "\n")
  reshaped_line <- wrap_vars(v.names, prefix_reshaped, cont_indent)
  cat(reshaped_line, "\n\n")

  return(long)
}
