#' Missing Values Summary for Panel Data
#'
#' This function calculates summary statistics for missing values (NAs) in panel data,
#' providing both overall and detailed period-specific missing value counts.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param selection A character vector specifying which variables to analyze for missing values.
#'        If not specified, all variables in the data.frame will be used.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'              Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'             Not required if data has panel attributes.
#' @param detailed A logical flag indicating whether to return detailed period-specific NA counts.
#'        Default = FALSE.
#' @param digits An integer indicating the number of decimal places to round the na_share column.
#'        Default = 3.
#'
#' @return A data.frame with missing value summary statistics.
#'
#' @details
#' When `detailed = FALSE` (default), returns a data.frame with the following columns:
#' \describe{
#'   \item{\code{variable}}{The name of the analyzed variable}
#'   \item{\code{na_count}}{Total number of missing values (NAs) for the variable}
#'   \item{\code{na_share}}{Proportion of observations that are missing (0 to 1), rounded to specified digits}
#'   \item{\code{entities_with_na}}{Number of entities/groups with at least one missing value for this variable}
#'   \item{\code{periods_with_na}}{Number of time periods with at least one missing value for this variable}
#' }
#'
#' When `detailed = TRUE`, additional columns are included:
#' \describe{
#'   \item{\code{[period1]}}{Column with NA counts for period1 (name matches the time period value)}
#'   \item{\code{[period2]}}{Column with NA counts for period2 (name matches the time period value)}
#'   \item{...}{Additional columns for each unique time period in the data}
#' }
#'
#' The data.frame has additional attributes:
#' \describe{
#'   \item{\code{panel_group}}{The grouping variable name}
#'   \item{\code{panel_time}}{The time variable name}
#'   \item{\code{panel_detailed}}{Logical indicating detailed output}
#'   \item{\code{panel_digits}}{Number of decimal places used for rounding na_share}
#'   \item{\code{panel_total_obs}}{Total number of observations in the data}
#'   \item{\code{panel_n_entities}}{Total number of unique entities/groups}
#'   \item{\code{panel_n_periods}}{Total number of unique time periods}
#' }
#'
#' @seealso
#' [describe_incomplete()], [describe_balance()], [summarize_data()], [summarize_panel()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage with statistics for all variables
#' summarize_missing(production, group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' summarize_missing(panel_data)
#'
#' # Detailed output with period-specific NA counts
#' summarize_missing(production, group = "firm", time = "year", detailed = TRUE)
#'
#' # Show missing values for a specific variable
#' summarize_missing(production, selection = "sales", group = "firm", time = "year")
#'
#' # Show missing values for multiple variables
#' summarize_missing(production, selection = c("capital", "labor"), group = "firm", time = "year")
#'
#' # Customize rounding
#' summarize_missing(production, group = "firm", time = "year", digits = 4)
#'
#' @export
summarize_missing <- function(
  data,
  selection = NULL,
  group = NULL,
  time = NULL,
  detailed = FALSE,
  digits = 3
) {
  # Check if data has panel attributes
  has_panel_attrs <- !is.null(attr(data, "panel_group")) &&
    !is.null(attr(data, "panel_time"))

  if (has_panel_attrs) {
    # Extract group and time from attributes
    group <- attr(data, "panel_group")
    time <- attr(data, "panel_time")
  } else {
    # Handle regular data.frame
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame, not ", class(data)[1])
    }

    if (is.null(group) || is.null(time)) {
      stop(
        "For regular data.frames, both 'group' and 'time' arguments must be provided"
      )
    }
  }

  # Common validation
  if (!is.null(selection) && !is.character(selection)) {
    stop(
      "'selection' must be a character vector or NULL, not ",
      class(selection)[1]
    )
  }

  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string, not ", class(time)[1])
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  if (!time %in% names(data)) {
    stop('variable "', time, '" not found in data')
  }

  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("'detailed' must be a single logical value, not ", class(detailed)[1])
  }

  if (!is.numeric(digits) || length(digits) != 1 || digits < 0) {
    stop(
      "'digits' must be a single non-negative integer, not ",
      class(digits)[1]
    )
  }

  digits <- as.integer(digits)

  # Get data for analysis
  data_df <- data # Using original data as .check_and_convert_data_robust is not defined

  # If selection is not specified, use all variables except group and time
  if (is.null(selection)) {
    selection <- setdiff(names(data_df), c(group, time))

    if (length(selection) == 0) {
      stop("no variables found to analyze (besides group and time variables)")
    }

    message(
      "Analyzing all variable(s): ",
      paste(selection, collapse = ", ")
    )
  }

  # Validate selection
  missing_vars <- selection[!selection %in% names(data_df)]
  if (length(missing_vars) > 0) {
    stop(
      "the following variables were not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Remove group and time from selection if included
  selection <- setdiff(selection, c(group, time))
  if (length(selection) == 0) {
    stop("no variables to analyze (excluding group and time variables)")
  }

  # Get unique time periods and sort them
  time_values <- as.character(data_df[[time]])
  unique_periods <- unique(time_values)

  # Sort time periods if they appear numeric
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", unique_periods))) {
    ordered_periods <- as.character(sort(as.numeric(unique_periods)))
  } else {
    ordered_periods <- sort(unique_periods)
  }

  # Get unique groups
  unique_groups <- unique(as.character(data_df[[group]]))

  # Total counts for attributes
  total_obs <- nrow(data_df)
  total_entities <- length(unique_groups)
  total_periods <- length(ordered_periods)

  # Initialize result list
  results <- list()

  # Calculate missing value statistics for each variable
  for (var in selection) {
    # Total NA count
    na_count <- sum(is.na(data_df[[var]]))

    # NA share (proportion) - rounded to specified digits
    na_share <- ifelse(total_obs > 0, na_count / total_obs, 0)
    na_share <- round(na_share, digits)

    # Entities with at least one NA
    if (na_count > 0) {
      # Group by entity and check if any NA in the variable
      entity_has_na <- tapply(data_df[[var]], data_df[[group]], function(x) {
        any(is.na(x))
      })
      entities_with_na <- sum(entity_has_na, na.rm = TRUE)
    } else {
      entities_with_na <- 0
    }

    # Periods with at least one NA
    if (na_count > 0) {
      # Group by period and check if any NA in the variable
      period_has_na <- tapply(data_df[[var]], data_df[[time]], function(x) {
        any(is.na(x))
      })
      periods_with_na <- sum(period_has_na, na.rm = TRUE)
    } else {
      periods_with_na <- 0
    }

    # Create base result row
    result_row <- data.frame(
      variable = var,
      na_count = na_count,
      na_share = na_share,
      entities_with_na = entities_with_na,
      periods_with_na = periods_with_na,
      stringsAsFactors = FALSE
    )

    # Add detailed period-specific NA counts if requested
    if (detailed) {
      # Calculate NA counts for each period
      for (period in ordered_periods) {
        period_data <- data_df[time_values == period, var, drop = FALSE]
        period_na_count <- sum(is.na(period_data[[var]]))
        result_row[[period]] <- period_na_count
      }
    }

    results[[var]] <- result_row
  }

  # Combine all results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  # Add standardized attributes
  attr(result_df, "panel_group") <- group
  attr(result_df, "panel_time") <- time
  attr(result_df, "panel_detailed") <- detailed
  attr(result_df, "panel_digits") <- digits
  attr(result_df, "panel_total_obs") <- total_obs
  attr(result_df, "panel_n_entities") <- total_entities
  attr(result_df, "panel_n_periods") <- total_periods

  return(result_df)
}
