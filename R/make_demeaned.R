#' Within-Group Demeaning (Centering) for Panel Data
#'
#' This function performs within-group demeaning (centering) for all numeric
#' variables in a data frame. For each group defined by the `group` argument,
#' the group mean is subtracted from each observation. If no grouping is
#' provided, the overall mean is subtracted (grand mean centering).
#'
#' @param data A data.frame containing the variables to be demeaned.
#' @param group A character vector specifying the grouping variable(s). If not
#'        specified and `data` has panel attributes, the entity and time
#'        variables are used as grouping variables. Otherwise, overall demeaning
#'        is performed.
#'
#' @return The input data frame with all numeric variables replaced by their
#'         demeaned versions.
#'
#' @details
#' * If `group` is not specified and `data` is **not** a `panel_data` object,
#'   simple overall demeaning is performed: for each numeric variable, the
#'   overall mean (ignoring `NA`s) is subtracted.
#' * If `group` is specified, the grouping variables are used to define the
#'   groups. Observations with `NA` in any grouping variable are removed before
#'   demeaning.
#' * If `data` inherits from `panel_data` and `group` is not specified, the
#'   function automatically uses the entity and time variables stored in the
#'   `metadata` attribute as grouping variables, and the returned object retains
#'   the `panel_data` class and its attributes.
#'
#' Non‑numeric variables are excluded from demeaning; a message lists them,
#' and their names are stored in the `details` attribute.
#'
#' The returned object has a `metadata` attribute and a `details` attribute:
#' \describe{
#'   \item{`metadata`}{List containing the function name (`"make_demeaned"`)
#'         and the grouping variables used (`group`). If the input was a
#'         `panel_data` object and `group` was not specified, the original
#'         panel metadata (`entity`, `time`, and `delta` if present) are also
#'         included.}
#'   \item{`details`}{List with the names of non‑numeric variables that were
#'         excluded from demeaning (`excluded_variables`). If the input was a
#'         `panel_data` object and `group` was not specified, the original
#'         panel details are preserved and augmented with this element.}
#' }
#'
#' @seealso
#' See also [make_panel()], [make_balanced()], [make_wide()], [make_long()].
#'
#' @examples
#' data(production)
#'
#' # Simple overall demeaning
#' prod_demeaned <- make_demeaned(production)
#' head(prod_demeaned$labor)
#'
#' # Demeaning by a single group (e.g., firm)
#' prod_demeaned_firm <- make_demeaned(production, group = "firm")
#'
#' # Demeaning by multiple groups (e.g., firm and year)
#' prod_demeaned_both <- make_demeaned(production, group = c("firm", "year"))
#'
#' # Using a panel_data object: automatically demeans by firm and year
#' panel <- make_panel(production, index = c("firm", "year"))
#' panel_demeaned <- make_demeaned(panel)
#'
#' @export
make_demeaned <- function(data, group = NULL) {
  # --- Input validation ---
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1], call. = FALSE)
  }

  msg_printed <- FALSE
  keep_panel_class <- FALSE
  panel_metadata <- NULL
  panel_details <- NULL
  panel_class <- NULL
  group_used <- group
  entity_time_from_metadata <- FALSE

  # --- Determine grouping and whether to keep panel class ---
  if (inherits(data, "panel_data") && is.null(group)) {
    meta <- attr(data, "metadata")
    if (is.null(meta) || is.null(meta$entity) || is.null(meta$time)) {
      stop(
        "'panel_data' object missing required metadata (entity and time)",
        call. = FALSE
      )
    }
    group_used <- c(meta$entity, meta$time)
    keep_panel_class <- TRUE
    panel_metadata <- meta
    panel_details <- attr(data, "details")
    panel_class <- class(data)
    entity_time_from_metadata <- TRUE
  }

  # --- Validate group_used ---
  if (!is.null(group_used)) {
    if (!is.character(group_used)) {
      stop(
        "'group' must be a character vector or NULL, not ",
        class(group_used)[1],
        call. = FALSE
      )
    }
    if (length(group_used) == 0) {
      group_used <- NULL
    } else {
      missing_vars <- group_used[!group_used %in% names(data)]
      if (length(missing_vars) > 0) {
        stop(
          "grouping variable(s) not found in data: ",
          paste(missing_vars, collapse = ", "),
          call. = FALSE
        )
      }
    }
  }

  # --- Remove rows with NA in grouping variables ---
  if (!is.null(group_used)) {
    na_rows <- rep(FALSE, nrow(data))
    for (g in group_used) {
      na_rows <- na_rows | is.na(data[[g]])
    }
    if (any(na_rows)) {
      message(
        sum(na_rows),
        " rows with missing values in grouping variable(s) found and excluded."
      )
      data <- data[!na_rows, , drop = FALSE]
      msg_printed <- TRUE
      if (nrow(data) == 0) {
        stop(
          "No observations left after removing rows with missing grouping values.",
          call. = FALSE
        )
      }
    }
  }

  # --- Duplicate check (only when both entity and time are used as groups) ---
  if (
    !is.null(group_used) &&
      length(group_used) == 2 &&
      !entity_time_from_metadata
  ) {
    dup_rows <- duplicated(data[group_used]) |
      duplicated(data[group_used], fromLast = TRUE)
    if (any(dup_rows)) {
      dup_combinations <- unique(data[dup_rows, group_used, drop = FALSE])
      n_dup <- nrow(dup_combinations)
      examples <- utils::head(dup_combinations, 5)
      example_strings <- paste0(examples[[1]], "-", examples[[2]])
      example_str <- paste(example_strings, collapse = ", ")
      message(
        n_dup,
        " duplicate entity-time combinations found. Examples: ",
        example_str
      )
      msg_printed <- TRUE
    }
  }

  # --- Identify numeric and non-numeric variables ---
  is_numeric <- vapply(data, is.numeric, logical(1))
  numeric_vars <- names(data)[is_numeric]
  non_numeric_vars <- names(data)[!is_numeric]

  if (length(non_numeric_vars) > 0) {
    message(
      "Excluding non-numeric variables: ",
      paste(non_numeric_vars, collapse = ", ")
    )
    msg_printed <- TRUE
  }

  # --- Identify variables to demean (numeric and not in group) ---
  exclude_vars <- if (!is.null(group_used)) group_used else character(0)
  demean_vars <- setdiff(numeric_vars, exclude_vars)

  # --- Perform demeaning ---
  if (length(demean_vars) > 0) {
    if (is.null(group_used)) {
      # Overall demeaning (grand mean centering)
      for (var in demean_vars) {
        x <- data[[var]]
        data[[var]] <- x - mean(x, na.rm = TRUE)
      }
    } else {
      # Within-group demeaning
      group_factors <- lapply(group_used, function(g) {
        gf <- data[[g]]
        if (!is.factor(gf)) {
          gf <- as.factor(gf)
        }
        gf
      })
      names(group_factors) <- group_used

      for (var in demean_vars) {
        x <- data[[var]]
        args <- c(
          list(x),
          group_factors,
          list(FUN = function(y) y - mean(y, na.rm = TRUE))
        )
        data[[var]] <- do.call(ave, args)
      }
    }
  }

  # --- Build metadata attribute ---
  if (keep_panel_class) {
    # Preserve original panel metadata and add function_name + group
    new_metadata <- panel_metadata
    new_metadata$function_name <- "make_demeaned"
    new_metadata$group <- group_used
    # Ensure delta exists (may be NULL)
    if (!"delta" %in% names(new_metadata)) new_metadata$delta <- NULL
  } else {
    new_metadata <- list(
      function_name = "make_demeaned",
      group = group_used
    )
  }

  # --- Build details attribute ---
  if (keep_panel_class) {
    # Preserve original panel details and add excluded_variables
    new_details <- panel_details
    if (is.null(new_details)) {
      new_details <- list()
    }
    new_details$excluded_variables <- non_numeric_vars
  } else {
    new_details <- list(excluded_variables = non_numeric_vars)
  }

  # --- Attach attributes and class ---
  if (keep_panel_class) {
    attr(data, "metadata") <- new_metadata
    attr(data, "details") <- new_details
    class(data) <- panel_class
  } else {
    attr(data, "metadata") <- new_metadata
    attr(data, "details") <- new_details
    # Ensure no leftover panel_data class
    if (inherits(data, "panel_data")) {
      class(data) <- setdiff(class(data), "panel_data")
    }
  }

  if (msg_printed) {
    cat("\n")
  }
  return(data)
}
