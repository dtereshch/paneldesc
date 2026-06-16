#' Add Group Means for Mundlak-style Modeling
#'
#' This function adds group means of numeric variables to a data frame, which can
#' be used for Mundlak-style (correlated random effects) modeling or for other
#' purposes where within-group averages are needed.
#'
#' @param data A data.frame containing the variables.
#' @param group A character vector specifying the grouping variable(s). If not
#'        specified and `data` has panel attributes (class `panel_data`), the
#'        entity and time variables are extracted from the metadata and used as
#'        grouping variables. Otherwise, an error is thrown.
#'
#' @return The input data frame with new columns for each combination of numeric
#'         variable (excluding the grouping variables) and each grouping variable.
#'         The new columns are named `<var>_mean_<group>`. Rows with missing
#'         values in the grouping variables are removed. If the input was a
#'         `panel_data` object and `group` was not specified, the returned
#'         object retains the `panel_data` class and its attributes.
#'
#' @details
#' * For each numeric variable (excluding the grouping variables) and for each
#'   grouping variable, the group mean is computed (ignoring `NA`s) and added
#'   as a new column.
#' * If `group` is not specified and `data` is **not** a `panel_data` object,
#'   an error is raised because the grouping variables cannot be determined.
#' * Observations with `NA` in any grouping variable are removed before
#'   computing means; a message reports the number of removed rows.
#' * Missing values in the numeric variables are allowed; group means are
#'   computed with `na.rm = TRUE`.
#' * If the input is a `panel_data` object and `group` is not specified, the
#'   function uses the entity and time variables stored in the `metadata`
#'   attribute as grouping variables, and the returned object retains the
#'   `panel_data` class and its metadata.
#'
#' **Naming convention:**
#' For a numeric variable `x` and a grouping variable `g`, the new column is
#' named `x_mean_g`. For example, with grouping variables `"firm"` and `"year"`,
#' variable `"labor"` yields columns `labor_mean_firm` and `labor_mean_year`.
#'
#' **References:**
#' Mundlak, Y. (1978). On the Pooling of Time Series and Cross Section Data.
#' *Econometrica*, 46(1), 69–85. \doi{10.2307/1913646}.
#'
#' @seealso
#' See also [make_demeaned()], [summarize_numeric()], [plot_heterogeneity()].
#'
#' @examples
#' data(production)
#'
#' # Basic usage with explicit grouping
#' prod_mundlak <- make_mundlak(production, group = c("firm", "year"))
#' head(prod_mundlak)
#'
#' # Using a panel_data object: automatically uses entity and time
#' panel <- make_panel(production, index = c("firm", "year"))
#' panel_mundlak <- make_mundlak(panel)
#' head(panel_mundlak)
#'
#' # Variable names illustrate the pattern: labor_mean_firm, labor_mean_year, etc.
#' @export
make_mundlak <- function(data, group = NULL) {
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
  if (is.null(group_used)) {
    stop(
      "'group' must be specified or data must be a 'panel_data' object with metadata.",
      call. = FALSE
    )
  }
  if (!is.character(group_used)) {
    stop(
      "'group' must be a character vector, not ",
      class(group_used)[1],
      call. = FALSE
    )
  }
  if (length(group_used) == 0) {
    stop("'group' must have at least one variable.", call. = FALSE)
  }
  missing_vars <- group_used[!group_used %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(
      "grouping variable(s) not found in data: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # --- Remove rows with NA in grouping variables ---
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

  # --- Duplicate check (only when both entity and time are used as groups) ---
  if (
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

  # --- Identify numeric variables (non-numeric are ignored) ---
  is_numeric <- vapply(data, is.numeric, logical(1))
  numeric_vars <- names(data)[is_numeric]

  # --- Identify variables for which to compute means (exclude group variables) ---
  mean_vars <- setdiff(numeric_vars, group_used)

  # --- Message about which variables will get means ---
  if (length(mean_vars) > 0) {
    message(
      "Adding group means for numeric variables: ",
      paste(mean_vars, collapse = ", ")
    )
    msg_printed <- TRUE
  } else {
    message(
      "No numeric variables to compute means for (excluding grouping variables)."
    )
    msg_printed <- TRUE
  }

  # --- Compute and add group means ---
  if (length(mean_vars) > 0) {
    for (var in mean_vars) {
      for (g in group_used) {
        new_col <- paste0(var, "_mean_", g)
        # Compute group means using ave, ignoring NA in the variable
        data[[new_col]] <- ave(
          data[[var]],
          data[[g]],
          FUN = function(x) mean(x, na.rm = TRUE)
        )
      }
    }
  }

  # --- Build metadata attribute ---
  if (keep_panel_class) {
    new_metadata <- panel_metadata
    new_metadata$function_name <- "make_mundlak"
    new_metadata$group <- group_used
    if (!"delta" %in% names(new_metadata)) new_metadata$delta <- NULL
  } else {
    new_metadata <- list(
      function_name = "make_mundlak",
      group = group_used
    )
  }

  # --- Build details attribute ---
  if (keep_panel_class) {
    new_details <- panel_details
    if (is.null(new_details)) {
      new_details <- list()
    }
  } else {
    new_details <- list()
  }

  # --- Attach attributes and class ---
  if (keep_panel_class) {
    attr(data, "metadata") <- new_metadata
    attr(data, "details") <- new_details
    class(data) <- panel_class
  } else {
    attr(data, "metadata") <- new_metadata
    attr(data, "details") <- new_details
    if (inherits(data, "panel_data")) {
      class(data) <- setdiff(class(data), "panel_data")
    }
  }

  if (msg_printed) {
    cat("\n")
  }
  return(data)
}
