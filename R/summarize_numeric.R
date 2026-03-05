#' Summary Statistics for Numeric Variables
#'
#' This function calculates summary statistics for numeric variables,
#' either overall or grouped by a single grouping variable.
#'
#' @param data A data.frame containing variables for analysis.
#' @param select A character vector specifying which numeric variables to analyze.
#'        If not specified, all numeric variables in the data.frame will be used.
#' @param group A character string specifying the grouping variable name.
#'        If not specified, overall statistics will be returned.
#' @param detail A logical flag indicating whether to return additional statistics (25th, 50th, and 75th percentiles).
#'        Default = FALSE.
#' @param digits An integer specifying the number of decimal places for rounding statistics.
#'        Default = 3.
#'
#' @return A data.frame with descriptive statistics summary.
#'
#' @details
#' The returned data.frame contains columns depending on the arguments:
#'
#' When no grouping variable is specified (overall):
#' \describe{
#'   \item{\code{variable}}{The name of the numeric variable.}
#'   \item{\code{count}}{Number of non‑NA observations.}
#'   \item{\code{mean}}{Arithmetic mean.}
#'   \item{\code{std}}{Standard deviation.}
#'   \item{\code{min}}{Minimum value.}
#'   \item{\code{max}}{Maximum value.}
#' }
#'
#' When `detail = TRUE`, additional columns are included:
#' \describe{
#'   \item{\code{p25}}{25th percentile (first quartile).}
#'   \item{\code{p50}}{50th percentile (median).}
#'   \item{\code{p75}}{75th percentile (third quartile).}
#' }
#'
#' When a grouping variable is specified, statistics are calculated for each group,
#' and the data.frame includes a column named after the grouping variable,
#' followed by the same statistics columns as above.
#'
#' The object has class `"panel_summary"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List with counts of variables, groups, and total observations.}
#' }
#'
#' @note
#' If the input data is a `panel_data` object (e.g., from the **panelr** package),
#' the function automatically retrieves the entity and time identifiers from its
#' metadata. These variables are then excluded from the default selection of
#' numeric variables (i.e., when `select = NULL`) because they typically do
#' not represent substantive measurements. However, if you explicitly include
#' them in the `select` argument, they will be analyzed. The `group` argument
#' for grouping summaries is independent and can be set to any variable,
#' including the panel's time or entity variable.
#'
#' This function does **not** use panel attributes for grouping; it is designed for general use.
#'
#' @seealso
#' See also [decompose_numeric()], [plot_heterogeneity()].
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' summarize_numeric(production)
#'
#' # Selecting specific variables
#' summarize_numeric(production, select = "sales")
#' summarize_numeric(production, select = c("capital", "labor"))
#'
#' # Grouped statistics
#' summarize_numeric(production, group = "year")
#'
#' # Detailed statistics
#' summarize_numeric(production, detail = TRUE)
#'
#' # Custom rounding
#' summarize_numeric(production, digits = 2)
#'
#' # Accessing attributes
#' out_sum_num <- summarize_numeric(production)
#' attr(out_sum_num, "metadata")
#' attr(out_sum_num, "details")
#'
#' @export
summarize_numeric <- function(
  data,
  select = NULL,
  group = NULL,
  detail = FALSE,
  digits = 3
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
  }

  if (!is.null(select) && !is.character(select)) {
    stop("'select' must be a character vector or NULL, not ", class(select)[1])
  }

  if (!is.null(group) && (!is.character(group) || length(group) != 1)) {
    stop(
      "'group' must be a single character string or NULL, not ",
      class(group)[1]
    )
  }

  if (!is.null(group) && !group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  # Validate detail parameter
  if (!is.logical(detail) || length(detail) != 1) {
    stop("'detail' must be a single logical value, not ", class(detail)[1])
  }

  # Harmonized digits validation
  if (!is.numeric(digits) || length(digits) != 1) {
    stop("'digits' must be a single non-negative integer")
  }
  if (digits < 0 || digits != round(digits)) {
    stop("'digits' must be a non-negative integer")
  }
  digits <- as.integer(digits)

  # --- Panel data handling: extract entity/time identifiers if present ---
  panel_id_vars <- character(0)
  if (inherits(data, "panel_data")) {
    metadata <- attr(data, "metadata")
    if (
      !is.null(metadata) && !is.null(metadata$entity) && !is.null(metadata$time)
    ) {
      panel_id_vars <- c(metadata$entity, metadata$time)
    }
  }

  # Track if any messages were printed
  msg_printed <- FALSE

  # If select is NULL, use all numeric variables with message
  if (is.null(select)) {
    numeric_vars <- vapply(data, is.numeric, FUN.VALUE = logical(1))
    analyze_vars <- names(data)[numeric_vars]

    if (length(analyze_vars) == 0) {
      stop("no numeric variables found in the dataset")
    }

    # Determine variables to exclude from default selection:
    # - the grouping variable (if provided and numeric)
    # - panel identifiers (if they are numeric and present)
    exclude_vars <- character(0)
    if (!is.null(group) && group %in% analyze_vars) {
      exclude_vars <- c(exclude_vars, group)
    }
    if (length(panel_id_vars) > 0) {
      panel_id_vars_in_selection <- intersect(panel_id_vars, analyze_vars)
      if (length(panel_id_vars_in_selection) > 0) {
        exclude_vars <- c(exclude_vars, panel_id_vars_in_selection)
      }
    }
    if (length(exclude_vars) > 0) {
      analyze_vars <- analyze_vars[!analyze_vars %in% exclude_vars]
    }

    if (length(analyze_vars) == 0) {
      stop(
        "after removing grouping variable and panel identifiers, there are no numeric variables to analyze"
      )
    }

    message(
      "Analyzing all numeric variables: ",
      paste(analyze_vars, collapse = ", ")
    )
    msg_printed <- TRUE
  } else {
    analyze_vars <- select
  }

  # Validate select
  if (length(analyze_vars) == 0) {
    stop("no numeric variables found to analyze")
  }

  missing_vars <- analyze_vars[!analyze_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(
      "the following variables were not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Check if specified columns are numeric
  non_numeric_vars <- analyze_vars[
    !vapply(data[analyze_vars], is.numeric, logical(1))
  ]
  if (length(non_numeric_vars) > 0) {
    stop(
      "the following variables are not numeric: ",
      paste(non_numeric_vars, collapse = ", ")
    )
  }

  # Check that select does not include the grouping variable
  if (!is.null(group) && group %in% analyze_vars) {
    stop(
      "'select' cannot contain the same variable as the 'group' variable ('",
      group,
      "')"
    )
  }

  # Validate group if provided
  if (!is.null(group)) {
    if (length(group) > 1) {
      stop("only one grouping variable is supported")
    }

    if (!group %in% names(data)) {
      stop('variable "', group, '" not found in data')
    }
  }

  # Get group information for attributes
  n_groups <- if (!is.null(group)) length(unique(data[[group]])) else NULL
  n_obs <- nrow(data)

  # Helper function to count non-NA values
  count_non_na <- function(x) {
    sum(!is.na(x))
  }

  # Calculate statistics without grouping
  if (is.null(group)) {
    results_list <- lapply(analyze_vars, function(var) {
      x <- data[[var]]

      # Handle case where all values are NA
      if (all(is.na(x))) {
        if (detail) {
          stats_row <- data.frame(
            count = 0,
            mean = NA_real_,
            std = NA_real_,
            min = NA_real_,
            p25 = NA_real_,
            p50 = NA_real_,
            p75 = NA_real_,
            max = NA_real_
          )
        } else {
          stats_row <- data.frame(
            count = 0,
            mean = NA_real_,
            std = NA_real_,
            min = NA_real_,
            max = NA_real_
          )
        }
      } else {
        if (detail) {
          stats_row <- data.frame(
            count = count_non_na(x),
            mean = mean(x, na.rm = TRUE),
            std = sd(x, na.rm = TRUE),
            min = min(x, na.rm = TRUE),
            p25 = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE),
            p50 = quantile(x, probs = 0.50, na.rm = TRUE, names = FALSE),
            p75 = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE),
            max = max(x, na.rm = TRUE)
          )
        } else {
          stats_row <- data.frame(
            count = count_non_na(x),
            mean = mean(x, na.rm = TRUE),
            std = sd(x, na.rm = TRUE),
            min = min(x, na.rm = TRUE),
            max = max(x, na.rm = TRUE)
          )
        }
      }

      # Round numeric statistics
      stats_row[] <- lapply(stats_row, function(col) {
        round_if_needed(col, digits)
      })

      # Add variable name only (no group column)
      data.frame(variable = var, stats_row)
    })

    out <- do.call(rbind, results_list)
  } else {
    # Calculate statistics with grouping
    group_combinations <- unique(data[[group]])
    group_combinations <- sort(group_combinations[!is.na(group_combinations)])

    results_list <- list()

    for (i in seq_along(group_combinations)) {
      current_group <- group_combinations[i]

      # Create subset for current group
      group_subset <- data[
        data[[group]] == current_group & !is.na(data[[group]]),
      ]

      # Calculate statistics for each variable in current group
      group_results <- lapply(analyze_vars, function(var) {
        x <- group_subset[[var]]

        # Handle case where all values are NA
        if (all(is.na(x))) {
          if (detail) {
            stats_row <- data.frame(
              count = 0,
              mean = NA_real_,
              std = NA_real_,
              min = NA_real_,
              p25 = NA_real_,
              p50 = NA_real_,
              p75 = NA_real_,
              max = NA_real_
            )
          } else {
            stats_row <- data.frame(
              count = 0,
              mean = NA_real_,
              std = NA_real_,
              min = NA_real_,
              max = NA_real_
            )
          }
        } else {
          if (detail) {
            stats_row <- data.frame(
              count = count_non_na(x),
              mean = mean(x, na.rm = TRUE),
              std = sd(x, na.rm = TRUE),
              min = min(x, na.rm = TRUE),
              p25 = quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE),
              p50 = quantile(x, probs = 0.50, na.rm = TRUE, names = FALSE),
              p75 = quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE),
              max = max(x, na.rm = TRUE)
            )
          } else {
            stats_row <- data.frame(
              count = count_non_na(x),
              mean = mean(x, na.rm = TRUE),
              std = sd(x, na.rm = TRUE),
              min = min(x, na.rm = TRUE),
              max = max(x, na.rm = TRUE)
            )
          }
        }

        # Round numeric statistics
        stats_row[] <- lapply(stats_row, function(col) {
          round_if_needed(col, digits)
        })

        # Add group information and variable name
        data.frame(
          setNames(data.frame(as.character(current_group)), group),
          variable = var,
          stats_row
        )
      })

      results_list <- c(results_list, group_results)
    }

    out <- do.call(rbind, results_list)
  }

  # Reset row names
  rownames(out) <- NULL

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    select = analyze_vars,
    group = group,
    detail = detail,
    digits = digits
  )

  # Build details list (only non-metadata info)
  details <- list(
    count_variables = length(analyze_vars),
    count_groups = n_groups,
    count_obs = n_obs
  )

  # Set attributes
  attr(out, "metadata") <- metadata
  attr(out, "details") <- details

  # Set class
  class(out) <- c("panel_summary", "data.frame")

  # Add empty line before returning data.frame if messages were printed
  if (msg_printed) {
    cat("\n")
  }

  return(out)
}
