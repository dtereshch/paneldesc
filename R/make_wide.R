#' Convert Panel Data from Long to Wide Format
#'
#' This function reshapes panel data from long format to wide format.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param select A character vector specifying the names of the time‑varying
#'        variables to reshape.
#' @param index A character vector of length 2 specifying the names of the
#'        entity and time variables.
#'        If not specified and data is a `panel_data` object, the entity and time values
#'        will be extracted from the data.frame attributes.
#' @param static An optional character vector of names of time‑invariant variables.
#' @param spacer A character string to insert between variable names and time
#'        values in the wide format column names. Default = `"_"`.
#' @param invert A logical flag indicating whether to put time values before
#'        variable names in column names. If FALSE, column names are
#'        `variable + spacer + time`; if TRUE, they are `time + spacer + variable`.
#'        Default = FALSE.
#'
#' @return A data.frame containing panel data in a wide format.
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
#' and finally the selected time‑varying variables, grouped by variable
#' (i.e., all time periods for the first variable, then all time periods for
#' the second variable, and so on). Time periods are ordered by their natural
#' order. For character or numeric time variables this means increasing order.
#' For factor time variables the order follows the factor levels.
#'
#' The returned object has class `"panel_wide"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.
#'         If the input was a `panel_data` object, the original metadata elements
#'         (entity, time, and delta) are preserved.}
#'   \item{`details`}{List containing character vectors of reshaped variables names
#'         and static variables names.}
#' }
#'
#' @note
#' The reshaping preserves standard atomic types and factors. When extracting
#' static variables, the function attempts to preserve the original column class
#' (e.g., `Date`, `POSIXct`). If the class cannot be maintained (for example
#' because the column’s assignment method does not handle NA values as
#' expected), a warning is issued and you should consider converting the column
#' to a simpler type before calling `make_wide()`.
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
#' # Define the time-varying variables to reshape (optional, for reuse)
#' vars <- c("sales", "capital", "labor", "industry", "ownership")
#'
#' # Direct selection of variables inside the function
#' wide <- make_wide(production,
#'                   select = c("sales", "capital", "labor", "industry", "ownership"),
#'                   index = c("firm", "year"))
#'
#' # Using a pre-defined vector
#' wide2 <- make_wide(production, select = vars, index = c("firm", "year"))
#'
#' # Custom spacer and inverted order
#' wide3 <- make_wide(production,
#'                    select = vars,
#'                    index = c("firm", "year"),
#'                    spacer = ".", invert = TRUE)
#'
#' # With panel_data object
#' panel <- make_panel(production, index = c("firm", "year"))
#' wide4 <- make_wide(panel, select = vars)
#'
#' # Using `static` to explicitly mark a time‑invariant variable
#' wide_region <- make_wide(production, select = vars,
#'                          index = c("firm", "year"), static = "region")
#'
#' # Accessing attributes
#' attr(wide, "metadata")
#' attr(wide, "details")
#'
#' @importFrom utils head
#' @importFrom stats reshape
#' @export
make_wide <- function(
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
  if (!is.character(spacer) || length(spacer) != 1) {
    stop("'spacer' must be a single character string", call. = FALSE)
  }
  if (!is.logical(invert) || length(invert) != 1) {
    stop("'invert' must be a single logical value", call. = FALSE)
  }

  # ---- Extract index ----
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
        stop(
          "For panel_data objects with incomplete metadata, 'index' must be provided",
          call. = FALSE
        )
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

  # Validate existence of index variables
  if (!entity_var %in% names(data)) {
    stop('entity variable "', entity_var, '" not found in data', call. = FALSE)
  }
  if (!time_var %in% names(data)) {
    stop('time variable "', time_var, '" not found in data', call. = FALSE)
  }
  if (time_var == entity_var) {
    stop("entity and time variables cannot be the same", call. = FALSE)
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
    # Check existence in data
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
    # Must not contain index variables
    if (entity_var %in% static) {
      stop(
        "'static' cannot contain the entity variable '",
        entity_var,
        "'",
        call. = FALSE
      )
    }
    if (time_var %in% static) {
      stop(
        "'static' cannot contain the time variable '",
        time_var,
        "'",
        call. = FALSE
      )
    }
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

  # Determine static variables (all non‑index, non‑select columns)
  all_vars <- names(data)
  static_vars <- setdiff(all_vars, c(entity_var, time_var, select))

  # Check invariance of all static variables
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
  }

  # Prepare static data (class-preserving version)
  if (length(static_vars) > 0) {
    static_data <- data[
      !duplicated(data[[entity_var]]),
      c(entity_var, static_vars),
      drop = FALSE
    ]
    entities_uniq <- static_data[[entity_var]]
    for (v in static_vars) {
      vals_list <- split(data[[v]], data[[entity_var]])
      if (is.factor(data[[v]])) {
        first_vals <- lapply(entities_uniq, function(e) {
          vals <- vals_list[[as.character(e)]]
          non_na <- vals[!is.na(vals)]
          if (length(non_na) == 0) NA else non_na[1]
        })
        static_data[[v]] <- factor(
          unlist(first_vals, use.names = FALSE),
          levels = levels(data[[v]]),
          ordered = is.ordered(data[[v]])
        )
      } else {
        static_data[[v]] <- data[[v]][1:length(entities_uniq)]
        for (i in seq_along(entities_uniq)) {
          vals <- vals_list[[as.character(entities_uniq[i])]]
          non_na <- vals[!is.na(vals)]
          if (length(non_na) == 0) {
            static_data[[v]][i] <- NA
          } else {
            static_data[[v]][i] <- non_na[1]
          }
        }
        if (!identical(class(static_data[[v]]), class(data[[v]]))) {
          warning(
            "Static variable '",
            v,
            "' lost its original class during extraction. ",
            "Please check and possibly convert to a simpler type.",
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

  # ---- Store sorted unique time values for later use ----
  time_values <- sort(unique(data[[time_var]]))

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

  # ---- Restore factor attributes for reshaped variables ----
  factor_select <- select[sapply(data[select], is.factor)]
  if (length(factor_select) > 0) {
    for (var in factor_select) {
      orig_levels <- levels(data[[var]])
      orig_ordered <- is.ordered(data[[var]])
      if (invert) {
        cols <- paste0(time_values, spacer, var)
      } else {
        cols <- paste0(var, spacer, time_values)
      }
      cols <- intersect(cols, names(wide))
      for (col in cols) {
        wide[[col]] <- factor(
          orig_levels[wide[[col]]],
          levels = orig_levels,
          ordered = orig_ordered
        )
      }
    }
  }

  # Merge static
  if (!is.null(static_data)) {
    wide <- merge(
      wide,
      static_data,
      by = entity_var,
      all.x = TRUE,
      sort = FALSE
    )
  }

  # Reorder time-varying columns: variable-major (x_1, x_2, y_1, y_2)
  tv_cols_all <- setdiff(names(wide), c(entity_var, static_vars))
  tv_ordered <- c()
  for (var in select) {
    for (t in time_values) {
      if (invert) {
        colname <- paste0(t, spacer, var)
      } else {
        colname <- paste0(var, spacer, t)
      }
      if (colname %in% tv_cols_all) {
        tv_ordered <- c(tv_ordered, colname)
      }
    }
  }
  missing_tv <- setdiff(tv_cols_all, tv_ordered)
  if (length(missing_tv) > 0) {
    tv_ordered <- c(tv_ordered, missing_tv)
  }
  wide <- wide[, c(entity_var, static_vars, tv_ordered), drop = FALSE]

  # Duplicate column check
  if (any(duplicated(names(wide)))) {
    dup_names <- unique(names(wide)[duplicated(names(wide))])
    stop(
      "Duplicate column names found after reshaping: ",
      paste(dup_names, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- Build groups for summary ----
  groups <- list()
  for (stub in select) {
    groups[[stub]] <- sapply(time_values, function(t) {
      if (invert) paste0(t, spacer, stub) else paste0(stub, spacer, t)
    })
  }
  flat_reshaped <- unlist(groups, use.names = FALSE)

  # ---- Metadata and details ----
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
    reshaped_variables = flat_reshaped,
    reshaped_groups = groups,
    static_variables = static_vars
  )

  attr(wide, "metadata") <- new_metadata
  attr(wide, "details") <- new_details
  class(wide) <- c("panel_wide", class(wide))

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

  # Prepare annotated static variable display if needed
  user_static <- if (is.null(static)) character(0) else static
  auto_static <- setdiff(static_vars, user_static)

  if (!is.null(static) && length(auto_static) > 0) {
    # Distinguish user-defined and auto-detected
    display_static <- character(length(static_vars))
    for (i in seq_along(static_vars)) {
      v <- static_vars[i]
      if (v %in% user_static) {
        display_static[i] <- paste0(v, " (user-defined)")
      } else {
        display_static[i] <- paste0(v, " (auto-detected)")
      }
    }
    # Put user-defined first
    reorder <- order(match(
      static_vars,
      user_static,
      nomatch = length(static_vars)
    ))
    display_static <- display_static[reorder]
  } else {
    display_static <- static_vars
  }

  static_line <- wrap_vars(display_static, prefix_static, cont_indent)
  cat(static_line, "\n")

  # Reshaped groups
  if (length(groups) > 0) {
    group_names <- names(groups)
    first_group <- groups[[group_names[1]]]
    first_line <- paste0(prefix_reshaped, paste(first_group, collapse = ", "))
    cat(first_line, "\n")
    if (length(groups) > 1) {
      for (g in group_names[-1]) {
        line <- paste0(cont_indent, paste(groups[[g]], collapse = ", "))
        cat(line, "\n")
      }
    }
  } else {
    cat(prefix_reshaped, "none\n")
  }
  cat("\n")

  return(wide)
}
