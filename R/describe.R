#' Descriptive Statistics
#'
#' This function calculates comprehensive descriptive statistics for numeric variables,
#' either overall or grouped by a single grouping variable.
#'
#' @param data A data.frame containing the variables for analysis.
#' @param variables A character vector specifying which numeric variables to analyze.
#'        If not specified, all numeric variables in the data.frame will be used.
#' @param group A character string specifying the grouping variable name.
#'        If not provided, overall statistics will be returned.
#' @param detailed A logical flag indicating whether to return additional statistics in
#'        the style of pandas describe method. If TRUE, includes count, mean, sd,
#'        min, p25, median, p75, and max. Default = FALSE.
#' @param digits An integer specifying the number of decimal places for rounding
#'        statistics. Default = 3.
#'
#' @return A data.frame with descriptive statistics. When grouped, contains:
#'   \item{group}{The grouping variable}
#'   \item{variable}{The name of the numeric variable}
#'   \item{n}{Number of non-NA observations}
#'   \item{mean}{Arithmetic mean}
#'   \item{median}{Median value}
#'   \item{sd}{Standard deviation}
#'   \item{min}{Minimum value}
#'   \item{max}{Maximum value}
#'   When `detailed = TRUE`, also includes:
#'   \item{p25}{25th percentile (first quartile)}
#'   \item{p75}{75th percentile (third quartile)}
#'   When ungrouped, the group column is omitted.
#'
#' @seealso
#' [decompose_variation()]
#'
#' @examples
#' data(production)
#'
#' # Overall statistics (no grouping)
#' describe(production)
#' describe(production, variables = "sales")
#' describe(production, variables = c("sales", "capital", "labor"))
#'
#' # Grouped statistics
#' describe(production, group = "year")
#' describe(production, variables = "sales", group = "year")
#' describe(production, variables = c("sales", "capital", "labor"), group = "firm")
#'
#' # Custom rounding
#' describe(production, digits = 2)
#' describe(production, group = "year", digits = 4)
#'
#' # Detailed statistics (pandas describe style)
#' describe(production, detailed = TRUE)
#' describe(production, group = "year", detailed = TRUE)
#'
#' @export
describe <- function(data, variables, group, detailed = FALSE, digits = 3) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("describe: 'data' must be a data.frame, not ", class(data)[1])
  }

  if (!missing(variables) && !is.character(variables)) {
    stop(
      "describe: 'variables' must be a character vector, not ",
      class(variables)[1]
    )
  }

  if (!missing(group) && (!is.character(group) || length(group) != 1)) {
    stop(
      "describe: 'group' must be a single character string, not ",
      class(group)[1]
    )
  }

  if (!missing(group) && !group %in% names(data)) {
    stop('describe: variable "', group, '" not found in data')
  }

  data <- .check_and_convert_data_robust(data, arg_name = "data")

  # Validate detailed parameter
  if (!is.logical(detailed) || length(detailed) != 1) {
    stop(
      "describe: 'detailed' must be a single logical value, not ",
      class(detailed)[1]
    )
  }

  # Validate digits
  if (!is.numeric(digits) || length(digits) != 1 || digits < 0) {
    stop(
      "describe: 'digits' must be a single non-negative integer, not ",
      class(digits)[1]
    )
  }

  # If variables not specified, use all numeric variables with message
  if (missing(variables)) {
    # Use vapply for more robust type checking
    numeric_vars <- vapply(data, is.numeric, FUN.VALUE = logical(1))
    variables <- names(data)[numeric_vars]

    # If no numeric variables found, stop with error
    if (length(variables) == 0) {
      stop("describe: no numeric variables found in the dataset")
    }

    # Remove the group variable from variables if it's numeric
    if (!missing(group) && group %in% variables) {
      variables <- variables[variables != group]
    }

    # Remove other potential ID variables
    id_like_vars <- c(
      "id",
      "ID",
      "Id",
      "year",
      "time",
      "period",
      "date",
      "firm",
      "company",
      "subject",
      "participant"
    )
    variables <- variables[!variables %in% id_like_vars]

    if (length(variables) == 0) {
      stop(
        "describe: no numeric variables remaining after removing ID-like variables"
      )
    }

    message(
      "Note: analyzing all numeric variable(s): ",
      paste(variables, collapse = ", ")
    )
  }

  # Validate variables
  if (length(variables) == 0) {
    stop("describe: no numeric variables found to analyze")
  }

  missing_vars <- variables[!variables %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(
      "describe: the following variables were not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Check if specified columns are numeric
  non_numeric_vars <- variables[!sapply(data[variables], is.numeric)]
  if (length(non_numeric_vars) > 0) {
    stop(
      "describe: the following variables are not numeric: ",
      paste(non_numeric_vars, collapse = ", ")
    )
  }

  # Validate group if provided
  if (!missing(group)) {
    if (length(group) > 1) {
      stop("describe: only one grouping variable is supported")
    }

    missing_groups <- group[!group %in% names(data)]
    if (length(missing_groups) > 0) {
      stop(
        'describe: variable "',
        paste(missing_groups, collapse = ", "),
        '" not found in data'
      )
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
  if (missing(group)) {
    results <- lapply(variables, function(var) {
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
      group_results <- lapply(variables, function(var) {
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
          group = as.character(current_group),
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
