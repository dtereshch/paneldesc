#' Add Group Means to a Data Frame
#'
#' This function adds group means of numeric variables to a data frame, which can
#' be used for Mundlak-style (correlated random effects) modeling or for other purposes where
#' within-group averages are needed.
#'
#' @param data A data.frame containing the variables.
#' @param group A character string or vector of character strings specifying
#'        the grouping variable(s).
#'        If not specified and data is a `panel_data` object, the entity and time values
#'        will be extracted from the data.frame attributes to define grouping variables.
#'        Otherwise, overall means are computed and added as constant columns.
#'
#' @return The input data frame with additional columns for group means.
#'
#' @details
#' For each numeric variable (excluding the grouping variables) and for each
#' grouping variable, the group mean is computed (ignoring `NA`s) and added
#' as a new column.
#'
#' For a numeric variable `x` and a grouping variable `g`, the new column is
#' named `x_mean_g`. For example, with grouping variables `"firm"` and `"year"`,
#' variable `"labor"` yields columns `labor_mean_firm` and `labor_mean_year`.
#'
#' If no grouping variable is supplied and `data` is not a `panel_data` object,
#' the overall mean of each numeric variable is added as a constant column named
#' `x_mean`.
#'
#' The returned object has class `"panel_data"` if the input was a `panel_data` object;
#' otherwise it is a `data.frame` with additional attributes.
#' In both cases, the object has the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.
#'         If the input was a `panel_data` object, the original metadata elements
#'         (entity, time, and delta) are preserved.}
#'   \item{`details`}{A list containing data.frames with the computed group means.
#'         If overall means were computed (no grouping), it contains a named vector
#'         of the overall means.}
#' }
#'
#' @seealso
#' See also [make_demeaned()], [summarize_numeric()], [plot_heterogeneity()].
#'
#' @examples
#' data(production)
#'
#' # Basic usage with a single group
#' prod_means_1 <- add_means(production, group = "firm")
#'
#' # Using multiple grouping variables
#' prod_means_2 <- add_means(production, group = c("firm", "year"))
#'
#' # With panel_data object (automatically uses entity and time)
#' panel <- make_panel(production, index = c("firm", "year"))
#' panel_means <- add_means(panel)
#'
#' # Overall means (no grouping)
#' prod_overall <- add_means(production)
#'
#' # Accessing attributes
#' attr(panel_means, "metadata")
#' attr(panel_means, "details")
#'
#' @export
add_means <- function(data, group = NULL) {
  # --- Input validation ---
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1], call. = FALSE)
  }

  msg_printed <- FALSE
  keep_panel_class <- FALSE
  panel_metadata <- NULL
  panel_class <- NULL
  group_used <- group
  entity_time_from_metadata <- FALSE

  # --- Determine grouping and whether to keep panel class ---
  if (inherits(data, "panel_data")) {
    keep_panel_class <- TRUE
    panel_metadata <- attr(data, "metadata")
    panel_class <- class(data)

    if (is.null(group)) {
      # Use entity and time from metadata
      if (
        is.null(panel_metadata) ||
          is.null(panel_metadata$entity) ||
          is.null(panel_metadata$time)
      ) {
        stop(
          "'panel_data' object missing required metadata (entity and time)",
          call. = FALSE
        )
      }
      group_used <- c(panel_metadata$entity, panel_metadata$time)
      entity_time_from_metadata <- TRUE
    } else {
      # User supplied grouping, but we still keep panel_data class
      group_used <- group
      entity_time_from_metadata <- FALSE
    }
  } else {
    # Not a panel_data object
    if (!is.null(group)) {
      group_used <- group
    } else {
      group_used <- NULL # overall means will be added
    }
    entity_time_from_metadata <- FALSE
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

  # --- Remove rows with NA in grouping variables (only if grouping) ---
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

  # --- Identify numeric variables (non-numeric are ignored) ---
  is_numeric <- vapply(data, is.numeric, logical(1))
  numeric_vars <- names(data)[is_numeric]

  # --- Identify variables for which to compute means (exclude group variables) ---
  exclude_vars <- if (!is.null(group_used)) group_used else character(0)
  mean_vars <- setdiff(numeric_vars, exclude_vars)

  # --- Message about which variables will get means ---
  if (length(mean_vars) > 0) {
    if (is.null(group_used)) {
      message(
        "Adding overall means for numeric variables: ",
        paste(mean_vars, collapse = ", ")
      )
    } else {
      message(
        "Adding group means for numeric variables: ",
        paste(mean_vars, collapse = ", ")
      )
    }
    msg_printed <- TRUE
  } else {
    message(
      "No numeric variables to compute means for (excluding grouping variables)."
    )
    msg_printed <- TRUE
  }

  # --- Compute and add means (as columns) ---
  details <- list()

  if (length(mean_vars) > 0) {
    if (is.null(group_used)) {
      # Overall means: add constant column for each numeric variable
      overall_means <- vapply(
        mean_vars,
        function(var) mean(data[[var]], na.rm = TRUE),
        numeric(1)
      )
      names(overall_means) <- mean_vars
      for (var in mean_vars) {
        new_col <- paste0(var, "_mean")
        data[[new_col]] <- overall_means[var]
      }
      details$means <- overall_means
    } else {
      # Group means: one column per (var, group) combination
      for (var in mean_vars) {
        for (g in group_used) {
          new_col <- paste0(var, "_mean_", g)
          data[[new_col]] <- ave(
            data[[var]],
            data[[g]],
            FUN = function(x) mean(x, na.rm = TRUE)
          )
        }
      }
      # Store group means summaries as data frames
      for (g in group_used) {
        group_means <- aggregate(
          data[, mean_vars, drop = FALSE],
          by = list(group = data[[g]]),
          FUN = mean,
          na.rm = TRUE
        )
        colnames(group_means)[1] <- g
        details[[paste0("means_", g)]] <- group_means
      }
    }
  }

  # --- Build metadata attribute ---
  if (keep_panel_class) {
    new_metadata <- panel_metadata
    new_metadata$function_name <- "add_means"
    new_metadata$group <- group_used
    if (!"delta" %in% names(new_metadata)) new_metadata$delta <- NULL
  } else {
    new_metadata <- list(
      function_name = "add_means",
      group = group_used
    )
  }

  # --- Attach attributes and class ---
  if (keep_panel_class) {
    attr(data, "metadata") <- new_metadata
    attr(data, "details") <- details
    class(data) <- panel_class
  } else {
    attr(data, "metadata") <- new_metadata
    attr(data, "details") <- details
    if (inherits(data, "panel_data")) {
      class(data) <- setdiff(class(data), "panel_data")
    }
  }

  if (msg_printed) {
    cat("\n")
  }
  return(data)
}
