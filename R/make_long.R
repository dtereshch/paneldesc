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
#'        The stubs must be unique (no duplicate entries). This argument has no
#'        default and must be provided explicitly.
#' @param index A character vector of length 2 specifying the name of the
#'        entity column (first element) and the name to give to the new time
#'        column in the long format (second element).
#'        If not specified and `data` is a `panel_wide` object, the entity and time
#'        values are extracted from the metadata.
#' @param static An optional character vector of names of time‑invariant variables.
#'        When provided, these columns are explicitly treated as static (not reshaped)
#'        and must be present in `data`. If `NULL` (default), the function behaves as
#'        before: all columns not matched by `select` (or the entity column) are
#'        considered static. This argument is never taken from the attributes of
#'        a `panel_wide` object – it must be supplied explicitly.
#' @param spacer A character string used to separate variable names and time
#'        values in the wide column names. Default = `"_"`. Use `""` (empty string)
#'        when no explicit separator exists. When `spacer` is not empty, the function
#'        splits column names on **every** occurrence of the separator: if
#'        `invert = FALSE` the **last** component is taken as the time value and the
#'        preceding components (re‑joined with the separator) form the variable stub;
#'        if `invert = TRUE` the **first** component is the time value and the rest
#'        (re‑joined) is the stub. The stub is then matched against `select`.
#'        When `spacer` is empty, column names are matched to stubs using a
#'        **longest‑prefix rule**: a column must **start** (if `invert = FALSE`)
#'        or **end** (if `invert = TRUE`) with one of the stubs in `select`, and
#'        the remaining part becomes the time value. If multiple stubs could match,
#'        the **longest** one is chosen automatically — this allows, for example,
#'        `select = c("gdp", "gdp_ppp")` to correctly assign `gdp_ppp2000` to the
#'        stub `"gdp_ppp"` and `gdp2000` to `"gdp"`.
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
#' Only the time periods that appear in the columns of the selected stubs are
#' used. Columns belonging to unselected stubs are ignored when determining the
#' set of time points.
#'
#' If some variable‑time combinations are missing from the wide format, the
#' function adds those columns filled with `NA` and prints a message.
#' After reshaping, if a row has `NA` for **all** reshaped variables,
#' the static columns for that row are also set to `NA`.  This prevents
#' a false impression that the entity had a valid observation at that time
#' point (only time‑varying variables were missing).
#'
#' The resulting time column is converted to the most appropriate type:
#' - If all time values are integers, the column becomes `integer`.
#' - If they are numeric but not integers, it becomes `double`.
#' - Otherwise, it remains `character`.
#'
#' Upon successful reshaping, a summary message is printed with aligned headers:
#' - `Static variables:` (indented to align the colon with `Reshaped variables:`)
#' - `Reshaped variables:` (the stubs)
#'
#' The returned object has class `"panel_data"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.
#'         If the input was a `panel_wide` object, the original metadata elements
#'         for the entity and time variables are preserved.}
#'   \item{`details`}{List with components:
#'         \describe{
#'           \item{`reshaped_variables`}{character vector of the long‑form variable stubs}
#'           \item{`static_variables`}{character vector of time‑invariant columns}
#'         }
#'   }
#' }
#'
#' @section Handling of `spacer = ""`:
#' When `spacer = ""` the function splits column names using the **stubs given in
#' `select`** by matching the longest possible stub. For each column:
#' \itemize{
#'   \item If `invert = FALSE`, it checks whether the column **starts with** one
#'         of the stubs; if multiple stubs match, the **longest** one is used.
#'   \item If `invert = TRUE`, it checks whether the column **ends with** one
#'         of the stubs, again choosing the longest match.
#' }
#' The remainder of the column name after removing the matched stub is taken as
#' the time value. This approach works intuitively with stubs that share common
#' prefixes (e.g., `"gdp"` and `"gdp_ppp"`): a column like `gdp_ppp2000` will
#' correctly be assigned to `"gdp_ppp"` because it is longer than `"gdp"`, while
#' `gdp2000` will be assigned to `"gdp"`. If you encounter a scenario where the
#' longest‑match rule does not give the desired split, use a non‑empty `spacer`
#' (e.g., `"_"`) to provide an explicit delimiter.
#'
#' @section Using the `static` argument:
#' In wide data, column names like `x_1`, `x_2`, `x_const` may cause ambiguity:
#' with `select = "x"` and `spacer = "_"`, `x_const` would be interpreted as
#' variable `x` at time `const`. To explicitly mark `x_const` as time‑invariant,
#' supply `static = "x_const"`. This ensures it is never reshaped.
#'
#' When `static` is specified, the function:
#' \itemize{
#'   \item Fixes the indicated columns as static.
#'   \item Searches for any other columns not matched by `select` (auto‑detected).
#'   \item Checks that **all** static variables (user‑defined and auto‑detected)
#'         are actually constant within each entity; a warning is issued if any
#'         variable shows variation.
#' }
#' The summary message distinguishes between the two groups only when both are
#' present; otherwise it appears as usual.
#'
#' @note
#' **Desirable input data** – The input wide data.frame should have one row per
#' entity and column names that follow a consistent naming convention. Time‑varying
#' columns must use the same separator (if `spacer` is not empty) or the same
#' stub‑time pattern (if `spacer` is empty). The entity column must contain unique
#' identifiers (no duplicates). Purely numeric column names (e.g., `"2000"`,
#' `"2001"`) are **not** recognised as time‑varying; use a non‑empty `spacer` and
#' explicit variable names in such cases.
#'
#' The input wide data must have unique entity values; duplicates cause an error.
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
#' # The `production` dataset contains `region`, which is constant over time.
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
    # Check existence in data (early, before further processing)
    missing_static <- setdiff(static, names(data))
    if (length(missing_static) > 0) {
      stop(
        "The following variables in 'static' were not found in 'data': ",
        paste(missing_static, collapse = ", "),
        call. = FALSE
      )
    }
    # Check no overlap with select
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

  # Static must not contain entity or time column
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

  # ---- Parse columns (SIMPLIFIED, longest‑match for spacer == "") ----
  parsed <- list()
  time_values <- c()

  # Prepare static column names to be skipped during parsing
  static_names <- if (is.null(static)) character(0) else static

  # For empty spacer, sort stubs by decreasing length so longest matches first
  stubs_sorted <- if (spacer == "") {
    select[order(nchar(select), decreasing = TRUE)]
  } else {
    select
  }

  for (col in candidates) {
    # Skip columns explicitly declared as static
    if (col %in% static_names) {
      next
    }

    if (spacer != "") {
      if (!grepl(spacer, col, fixed = TRUE)) {
        next
      }
      parts <- strsplit(col, spacer, fixed = TRUE)[[1]]
      if (length(parts) < 2) {
        next
      }
      if (invert) {
        time_val <- parts[1]
        var_name <- paste(parts[-1], collapse = spacer)
      } else {
        time_val <- parts[length(parts)]
        var_name <- paste(parts[-length(parts)], collapse = spacer)
      }
      if (var_name != "" && var_name %in% select) {
        parsed[[col]] <- list(var = var_name, time = time_val)
        time_values <- c(time_values, time_val)
      }
    } else {
      # spacer == "" : longest‑prefix (or suffix) match
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
    stop(
      "The following stubs in 'select' were not found in any column: ",
      paste(missing_stubs, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- Optional invert‑warning ----
  var_names <- sapply(parsed, `[[`, "var", USE.NAMES = FALSE)
  time_vals <- sapply(parsed, `[[`, "time", USE.NAMES = FALSE)
  time_numeric <- suppressWarnings(as.numeric(time_vals))
  var_numeric <- suppressWarnings(as.numeric(var_names))
  prop_time_numeric <- sum(!is.na(time_numeric)) / length(time_vals)
  prop_var_numeric <- sum(!is.na(var_numeric)) / length(var_names)
  if (prop_time_numeric < 0.5 && prop_var_numeric > 0.5) {
    suggested <- ifelse(invert, "FALSE", "TRUE")
    warning(
      "It appears that 'invert' may be incorrectly specified.\n",
      "Most extracted time values are non‑numeric (",
      round((1 - prop_time_numeric) * 100),
      "%) while most variable names are numeric (",
      round(prop_var_numeric * 100),
      "%).\nConsider setting 'invert = ",
      suggested,
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

  # ---- Build expected column names and detect conflicts ----
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
  }
  if (length(added_cols) > 0) {
    message(
      "Added the following columns (filled with NA) to complete unbalanced variables: ",
      paste(added_cols, collapse = ", ")
    )
  }

  # ---- Detect constant columns ----
  # All columns not entity and not varying are considered static.
  final_constant <- setdiff(names(data), c(entity_col, varying_cols))

  # ---- Time column name conflict ----
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

  # ---- Check time‑invariance of static variables ----
  const_cols <- intersect(final_constant, names(long))
  if (length(const_cols) > 0) {
    violation_list <- list()
    # Group by entity and check uniqueness
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

  # Prepare display names for static variables
  auto_static <- setdiff(final_constant, static_names)
  user_static <- static_names # may be empty

  if (!is.null(static) && length(auto_static) > 0) {
    # Annotate user-defined and auto-detected
    display_static <- character(length(final_constant))
    for (i in seq_along(final_constant)) {
      v <- final_constant[i]
      if (v %in% user_static) {
        display_static[i] <- paste0(v, " (user-defined)")
      } else {
        display_static[i] <- paste0(v, " (auto-detected)")
      }
    }
    # Ensure user-defined appear first (already in original order: static first
    # because we didn't reorder final_constant; static_names precede others?
    # final_constant order is as in names(data): static_names come wherever they are.
    # To guarantee user-defined first, we can sort display_static accordingly.
    reorder <- order(match(
      final_constant,
      static_names,
      nomatch = length(final_constant)
    ))
    display_static <- display_static[reorder]
  } else {
    # No annotation: plain names (works for NULL static or no auto-detected)
    display_static <- final_constant
  }

  static_line <- wrap_vars(display_static, prefix_static, cont_indent)
  cat(static_line, "\n")
  reshaped_line <- wrap_vars(v.names, prefix_reshaped, cont_indent)
  cat(reshaped_line, "\n\n")

  return(long)
}
