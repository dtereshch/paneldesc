#' Within-Group Demeaning (Centering) for Panel Data
#'
#' This function performs within-group demeaning (centering) for all numeric
#' variables in a data frame.
#' Non-numeric variables are not demeaned and are returned unchanged.
#'
#' @param data A data.frame containing variables to be demeaned.
#' @param group A character string or vector of character strings specifying
#'        the grouping variable(s).
#'        If not specified and data is a `panel_data` object, the entity and time values
#'        will be extracted from the data.frame attributes to define grouping variables.
#'        Otherwise, overall demeaning is performed.
#'
#' @return The input data frame containing all numeric variables replaced by their
#'         demeaned versions and unchanged non-numeric variables.
#'
#' @details
#' Depending on the value of `group` argument, the function uses different
#' demeaning procedures:
#' * If grouping variable is not specified and `data` is not a `panel_data` object,
#'   simple overall demeaning is performed: for each numeric variable, the
#'   overall mean (ignoring `NA`s) is subtracted.
#' * If one group variable is specified, the exact calculation is used:
#'   the group mean is subtracted from each observation.
#' * If two or more groups are specidied, iterative Gauss–Seidel algorithm is used.
#'   The algorithm runs up to **2000 iterations** with tolerance **1e-6**
#'   (matching the defaults of `fixest::demean()`);
#'   a warning is issued if convergence is not reached.
#'
#' The returned object has class `"panel_data"` if the input was a `panel_data` object;
#' otherwise it is a `data.frame` with additional attributes.
#' In both cases, the object has the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.
#'         If the input was a `panel_data` object, the original metadata elements
#'         (entity, time, and delta) are preserved.}
#'   \item{`details`}{A list containing data.frames with the computed group means.
#'         If overall demeaning was performed (no grouping), it contains a named vector
#'         of the overall means.}
#' }
#'
#' @seealso
#' See also [plot_demeaned()], [make_mundlak()], [make_panel()].
#'
#' @examples
#' data(production)
#'
#' # Basic usage (overall demeaning)
#' prod_demeaned <- make_demeaned(production)
#'
#' # Demeaning by a single group
#' prod_demeaned_firm <- make_demeaned(production, group = "firm")
#'
#' # Demeaning by two groups
#' prod_demeaned_both <- make_demeaned(production, group = c("firm", "year"))
#'
#' # With panel_data object (automatically demeans by entity and time)
#' panel <- make_panel(production, index = c("firm", "year"))
#' panel_demeaned <- make_demeaned(panel)
#'
#' # Accessing attributes
#' attr(panel_demeaned, "metadata")
#' attr(panel_demeaned, "details")
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
      group_used <- NULL
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

  # --- Identify numeric variables (non-numeric are left unchanged) ---
  is_numeric <- vapply(data, is.numeric, logical(1))
  numeric_vars <- names(data)[is_numeric]

  # --- Identify variables to demean (numeric and not in group) ---
  exclude_vars <- if (!is.null(group_used)) group_used else character(0)
  demean_vars <- setdiff(numeric_vars, exclude_vars)

  # --- Message about which variables will be demeaned ---
  if (length(demean_vars) > 0) {
    message(
      "Demeaning numeric variables: ",
      paste(demean_vars, collapse = ", ")
    )
    msg_printed <- TRUE
  }

  # --- Perform demeaning ---
  if (length(demean_vars) > 0) {
    if (is.null(group_used)) {
      # Overall demeaning (grand mean centering)
      for (var in demean_vars) {
        x <- data[[var]]
        data[[var]] <- x - mean(x, na.rm = TRUE)
      }
    } else if (length(group_used) == 1) {
      # Single grouping variable – fast within transformation via ave()
      group_fact <- as.factor(data[[group_used]])
      for (var in demean_vars) {
        x <- data[[var]]
        data[[var]] <- x -
          ave(x, group_fact, FUN = function(y) mean(y, na.rm = TRUE))
      }
    } else {
      # Two or more grouping variables – iterative projection (Gauss–Seidel)
      for (var in demean_vars) {
        resid <- data[[var]]
        max_iter <- 2000
        tol <- 1e-6
        converged <- FALSE
        for (iter in 1:max_iter) {
          old_resid <- resid
          for (g in group_used) {
            group_mean <- ave(resid, data[[g]], FUN = function(y) {
              mean(y, na.rm = TRUE)
            })
            resid <- resid - group_mean
          }
          change <- max(abs(resid - old_resid), na.rm = TRUE)
          if (change < tol) {
            converged <- TRUE
            break
          }
        }
        if (!converged) {
          warning(
            "Iterative demeaning did not converge for variable '",
            var,
            "' after ",
            max_iter,
            " iterations. Change = ",
            change,
            call. = FALSE
          )
        }
        data[[var]] <- resid
      }
    }
  }

  # --- Compute details ---
  details <- list()
  if (length(demean_vars) > 0) {
    if (!is.null(group_used)) {
      # Grouped demeaning: store group means as data frames per group
      for (g in group_used) {
        group_means <- aggregate(
          data[, demean_vars, drop = FALSE],
          by = list(group = data[[g]]),
          FUN = mean,
          na.rm = TRUE
        )
        colnames(group_means)[1] <- g
        details[[paste0("means_", g)]] <- group_means
      }
    } else {
      # Overall demeaning: store overall means as named vector
      overall_means <- vapply(
        demean_vars,
        function(var) mean(data[[var]], na.rm = TRUE),
        numeric(1)
      )
      names(overall_means) <- demean_vars
      details$means <- overall_means
    }
  }
  # else details remains empty list (no numeric variables to demean)

  # --- Build metadata attribute ---
  if (keep_panel_class) {
    new_metadata <- panel_metadata
    new_metadata$function_name <- "make_demeaned"
    new_metadata$group <- group_used
    if (!"delta" %in% names(new_metadata)) new_metadata$delta <- NULL
  } else {
    new_metadata <- list(
      function_name = "make_demeaned",
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
