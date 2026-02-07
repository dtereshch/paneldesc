#' Data Summary Statistics
#'
#' This function calculates summary statistics for numeric variables,
#' either overall or grouped by a single grouping variable.
#'
#' @param data A data.frame containing the variables for analysis.
#' @param selection A character vector specifying which numeric variables to analyze.
#'        If not specified, all numeric variables in the data.frame will be used.
#' @param group A character string specifying the grouping variable name.
#'        If not specified, overall statistics will be returned.
#' @param detailed A logical flag indicating whether to return additional statistics in
#'        the style of pandas describe method. If TRUE, includes count, mean, sd,
#'        min, p25, median, p75, and max. Default = FALSE.
#' @param digits An integer specifying the number of decimal places for rounding
#'        statistics. Default = 3.
#'
#' @return A data.frame with descriptive statistics summary.
#'
#' @details
#' The returned data.frame contains the following columns:
#' \describe{
#'   \item{\code{variable}}{The name of the numeric variable (always present)}
#'   \item{\code{group}}{The grouping variable (present only when `group` is specified)}
#'   \item{\code{n}}{Number of non-NA observations}
#'   \item{\code{mean}}{Arithmetic mean}
#'   \item{\code{median}}{Median value}
#'   \item{\code{sd}}{Standard deviation}
#'   \item{\code{min}}{Minimum value}
#'   \item{\code{max}}{Maximum value}
#' }
#'
#' When `detailed = TRUE`, additional columns are included:
#' \describe{
#'   \item{\code{p25}}{25th percentile (first quartile)}
#'   \item{\code{p75}}{75th percentile (third quartile)}
#' }
#'
#' When no grouping variable is specified, statistics are calculated overall.
#' When a grouping variable is specified, statistics are calculated for each group.
#'
#' @seealso
#' [summarize_panel()], [summarize_transition()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage with statistics for all numeric variables
#' summarize_data(production)
#'
#' # Detailed output
#' summarize_data(production, detailed = TRUE)
#'
#' # Show statistics for a single variable
#' summarize_data(production, selection = "sales")
#'
#' # Show statistics for multiple variables
#' summarize_data(production, selection = c("capital", "labor"))
#'
#' # Show grouped statistics
#' summarize_data(production, group = "year")
#'
#' # Show statistics with two digits rounding
#' summarize_data(production, digits = 2)
#'
#' # Show statistics with no rounding
#' summarize_data(production, digits = 999999)
#'
#' @export
summarize_data <- function(
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

  data <- .check_and_convert_data_robust(data, arg_name = "data")

  # Validate detailed parameter
  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("'detailed' must be a single logical value, not ", class(detailed)[1])
  }

  # Validate digits
  if (!is.numeric(digits) || length(digits) != 1 || digits < 0) {
    stop(
      "'digits' must be a single non-negative integer, not ",
      class(digits)[1]
    )
  }

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

  # Helper function to count non-NA values
  count_non_na <- function(x) {
    sum(!is.na(x))
  }

  # Helper function to round numeric values
  round_values <- function(x) {
    if (is.numeric(x)) {
      round(x, digits = digits)
    } else {
      x
    }
  }

  # Calculate statistics without grouping
  if (is.null(group)) {
    results <- lapply(selection, function(var) {
      x <- data[[var]]

      # Handle case where all values are NA
      if (all(is.na(x))) {
        if (detailed) {
          stats_row <- data.frame(
            n = 0,
            mean = NA_real_,
            sd = NA_real_,
            min = NA_real_,
            p25 = NA_real_,
            median = NA_real_,
            p75 = NA_real_,
            max = NA_real_
          )
        } else {
          stats_row <- data.frame(
            n = 0,
            mean = NA_real_,
            median = NA_real_,
            sd = NA_real_,
            min = NA_real_,
            max = NA_real_
          )
        }
      } else {
        if (detailed) {
          stats_row <- data.frame(
            n = count_non_na(x),
            mean = mean(x, na.rm = TRUE),
            sd = stats::sd(x, na.rm = TRUE),
            min = min(x, na.rm = TRUE),
            p25 = stats::quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE),
            median = median(x, na.rm = TRUE),
            p75 = stats::quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE),
            max = max(x, na.rm = TRUE)
          )
        } else {
          stats_row <- data.frame(
            n = count_non_na(x),
            mean = mean(x, na.rm = TRUE),
            median = median(x, na.rm = TRUE),
            sd = stats::sd(x, na.rm = TRUE),
            min = min(x, na.rm = TRUE),
            max = max(x, na.rm = TRUE)
          )
        }
      }

      # Round numeric statistics
      stats_row[] <- lapply(stats_row, round_values)

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
              n = 0,
              mean = NA_real_,
              sd = NA_real_,
              min = NA_real_,
              p25 = NA_real_,
              median = NA_real_,
              p75 = NA_real_,
              max = NA_real_
            )
          } else {
            stats_row <- data.frame(
              n = 0,
              mean = NA_real_,
              median = NA_real_,
              sd = NA_real_,
              min = NA_real_,
              max = NA_real_
            )
          }
        } else {
          if (detailed) {
            stats_row <- data.frame(
              n = count_non_na(x),
              mean = mean(x, na.rm = TRUE),
              sd = stats::sd(x, na.rm = TRUE),
              min = min(x, na.rm = TRUE),
              p25 = stats::quantile(
                x,
                probs = 0.25,
                na.rm = TRUE,
                names = FALSE
              ),
              median = median(x, na.rm = TRUE),
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
              n = count_non_na(x),
              mean = mean(x, na.rm = TRUE),
              median = median(x, na.rm = TRUE),
              sd = stats::sd(x, na.rm = TRUE),
              min = min(x, na.rm = TRUE),
              max = max(x, na.rm = TRUE)
            )
          }
        }

        # Round numeric statistics
        stats_row[] <- lapply(stats_row, round_values)

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

  return(result_df)
}
