#' Summary Statistics for Numeric Variables
#'
#' This function calculates summary statistics for numeric variables,
#' either overall or grouped by a single grouping variable.
#'
#' @param data A data.frame containing the variables for analysis.
#' @param selection A character vector specifying which numeric variables to analyze.
#'        If not specified, all numeric variables in the data.frame will be used.
#' @param group A character string specifying the grouping variable name.
#'        If not specified, overall statistics will be returned.
#' @param detailed A logical flag indicating whether to return additional statistics (25th, 50th, and 75th percentiles).
#'        Default = FALSE.
#' @param digits An integer specifying the number of decimal places for rounding statistics.
#'        Default = 3.
#'
#' @return A data.frame with descriptive statistics summary.
#'
#' @details
#' The returned data.frame contains the following columns:
#' \describe{
#'   \item{\code{variable}}{The name of the numeric variable (always present)}
#'   \item{\code{group}}{The grouping variable (present only when `group` is specified)}
#'   \item{\code{count}}{Number of non-NA observations}
#'   \item{\code{mean}}{Arithmetic mean}
#'   \item{\code{std}}{Standard deviation}
#'   \item{\code{min}}{Minimum value}
#'   \item{\code{max}}{Maximum value}
#' }
#'
#' When `detailed = TRUE`, additional columns are included:
#' \describe{
#'   \item{\code{p25}}{25th percentile (first quartile)}
#'   \item{\code{p50}}{50th percentile (median)}
#'   \item{\code{p75}}{75th percentile (third quartile)}
#' }
#'
#' When no grouping variable is specified, statistics are calculated overall.
#' When a grouping variable is specified, statistics are calculated for each group.
#'
#' The returned data.frame has class `"panel_summary"` and the following attributes:
#' \describe{
#'   \item{`details`}{List containing additional information: `detailed`, `digits`, `n_variables`,
#'         `n_groups` (if grouping provided), `total_obs`.}
#'   \item{`metadata`}{List containing the function name, selection, group, detailed, digits.}
#' }
#' Note: This function does **not** use panel attributes; it is designed for general use.
#'
#' @seealso
#' [decompose_numeric()], [decompose_factor()], [summarize_transition()], [summarize_missing()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage with statistics for all numeric variables
#' summarize_numeric(production)
#'
#' # Detailed output
#' summarize_numeric(production, detailed = TRUE)
#'
#' # Show statistics for a single variable
#' summarize_numeric(production, selection = "sales")
#'
#' # Show statistics for multiple variables
#' summarize_numeric(production, selection = c("capital", "labor"))
#'
#' # Show grouped statistics
#' summarize_numeric(production, group = "year")
#'
#' # Show statistics with two digits rounding
#' summarize_numeric(production, digits = 2)
#'
#' # Effectively no rounding (use large digit value)
#' summarize_numeric(production, digits = 999999)
#'
#' @export
summarize_numeric <- function(
  data,
  selection = NULL,
  group = NULL,
  detailed = FALSE,
  digits = 3
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
  }

  if (!is.null(selection) && !is.character(selection)) {
    stop(
      "'selection' must be a character vector or NULL, not ",
      class(selection)[1]
    )
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

  # Validate detailed parameter
  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("'detailed' must be a single logical value, not ", class(detailed)[1])
  }

  # Harmonized digits validation
  if (!is.numeric(digits) || length(digits) != 1) {
    stop("'digits' must be a single non-negative integer")
  }
  if (digits < 0 || digits != round(digits)) {
    stop("'digits' must be a non-negative integer")
  }
  digits <- as.integer(digits)

  # Helper function for rounding
  round_if_needed <- function(x, digits) {
    if (is.numeric(x) && !all(is.na(x))) {
      round(x, digits)
    } else {
      x
    }
  }

  # Track if any messages were printed
  messages_printed <- FALSE

  # If selection is NULL, use all numeric variables with message
  if (is.null(selection)) {
    # Use vapply for more robust type checking
    numeric_vars <- vapply(data, is.numeric, FUN.VALUE = logical(1))
    selection <- names(data)[numeric_vars]

    # If no numeric variables found, stop with error
    if (length(selection) == 0) {
      stop("no numeric variables found in the dataset")
    }

    # Remove the group variable from selection if it's numeric and provided
    if (!is.null(group) && group %in% selection) {
      selection <- selection[selection != group]
    }

    if (length(selection) == 0) {
      stop("no numeric variables remaining after removing ID-like variables")
    }

    message(
      "Analyzing all numeric variable(s): ",
      paste(selection, collapse = ", ")
    )
    messages_printed <- TRUE
  }

  # Validate selection
  if (length(selection) == 0) {
    stop("no numeric variables found to analyze")
  }

  missing_vars <- selection[!selection %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(
      "the following variables were not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Check if specified columns are numeric
  non_numeric_vars <- selection[!sapply(data[selection], is.numeric)]
  if (length(non_numeric_vars) > 0) {
    stop(
      "the following variables are not numeric: ",
      paste(non_numeric_vars, collapse = ", ")
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
  total_obs <- nrow(data)

  # Helper function to count non-NA values
  count_non_na <- function(x) {
    sum(!is.na(x))
  }

  # Calculate statistics without grouping
  if (is.null(group)) {
    results <- lapply(selection, function(var) {
      x <- data[[var]]

      # Handle case where all values are NA
      if (all(is.na(x))) {
        if (detailed) {
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
        if (detailed) {
          stats_row <- data.frame(
            count = count_non_na(x),
            mean = mean(x, na.rm = TRUE),
            std = stats::sd(x, na.rm = TRUE),
            min = min(x, na.rm = TRUE),
            p25 = stats::quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE),
            p50 = stats::quantile(x, probs = 0.50, na.rm = TRUE, names = FALSE),
            p75 = stats::quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE),
            max = max(x, na.rm = TRUE)
          )
        } else {
          stats_row <- data.frame(
            count = count_non_na(x),
            mean = mean(x, na.rm = TRUE),
            std = stats::sd(x, na.rm = TRUE),
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

    result_df <- do.call(rbind, results)
  } else {
    # Calculate statistics with grouping
    group_combinations <- unique(data[[group]])
    group_combinations <- sort(group_combinations[!is.na(group_combinations)])

    results <- list()

    for (i in seq_along(group_combinations)) {
      current_group <- group_combinations[i]

      # Create subset for current group
      group_subset <- data[
        data[[group]] == current_group & !is.na(data[[group]]),
      ]

      # Calculate statistics for each variable in current group
      group_results <- lapply(selection, function(var) {
        x <- group_subset[[var]]

        # Handle case where all values are NA
        if (all(is.na(x))) {
          if (detailed) {
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
          if (detailed) {
            stats_row <- data.frame(
              count = count_non_na(x),
              mean = mean(x, na.rm = TRUE),
              std = stats::sd(x, na.rm = TRUE),
              min = min(x, na.rm = TRUE),
              p25 = stats::quantile(
                x,
                probs = 0.25,
                na.rm = TRUE,
                names = FALSE
              ),
              p50 = stats::quantile(
                x,
                probs = 0.50,
                na.rm = TRUE,
                names = FALSE
              ),
              p75 = stats::quantile(
                x,
                probs = 0.75,
                na.rm = TRUE,
                names = FALSE
              ),
              max = max(x, na.rm = TRUE)
            )
          } else {
            stats_row <- data.frame(
              count = count_non_na(x),
              mean = mean(x, na.rm = TRUE),
              std = stats::sd(x, na.rm = TRUE),
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

      results <- c(results, group_results)
    }

    result_df <- do.call(rbind, results)
  }

  # Reset row names
  rownames(result_df) <- NULL

  # Build metadata (no panel_info used)
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    selection = selection,
    group = group,
    detailed = detailed,
    digits = digits
  )

  # Build details list (no panel_info)
  details <- list(
    detailed = detailed,
    digits = digits,
    n_variables = length(selection),
    n_groups = n_groups,
    total_obs = total_obs
  )

  # Set attributes in desired order (no panel_info)
  attr(result_df, "details") <- details
  attr(result_df, "metadata") <- metadata

  # Set class
  class(result_df) <- c("panel_summary", "data.frame")

  # Add empty line before returning data.frame if messages were printed
  if (messages_printed) {
    cat("\n")
  }

  return(result_df)
}
