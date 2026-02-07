#' Time Period Coverage Description
#'
#' Provides detailed coverage statistics for each time period in panel data.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'              Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'             Not required if data has panel attributes.
#'
#' @return A data.frame with time period coverage statistics.
#'
#' @details
#' The data.frame contains the following columns:
#' \describe{
#'   \item{\code{[time]}}{Time period identifier (name matches the input `time` argument)}
#'   \item{\code{total}}{Total number of observations in each period (all rows)}
#'   \item{\code{balanced}}{Number of observations with at least one non-NA value in substantive variables}
#'   \item{\code{complete}}{Number of observations without any NA values in substantive variables}
#' }
#'
#' Time periods are sorted naturally (numeric values as numbers, others alphabetically).
#'
#' The data.frame has additional attributes:
#' \describe{
#'   \item{\code{panel_group}}{The grouping variable name}
#'   \item{\code{panel_time}}{The time variable name}
#'   \item{\code{panel_n_entities}}{Total number of unique entities/groups}
#'   \item{\code{panel_n_periods}}{Total number of unique time periods}
#'   \item{\code{panel_total_obs}}{Total number of observations in the data}
#' }
#'
#' @seealso
#' [describe_balance()], [explore_balance()], [describe_participation()]
#'
#' @examples
#' data(production)
#' describe_periods(production, group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' describe_periods(panel_data)
#'
#' @export
describe_periods <- function(data, group = NULL, time = NULL) {
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

  # Extract variables
  group_var <- as.character(data[[group]])
  time_var <- as.character(data[[time]])

  # Get unique values
  unique_times <- unique(time_var)
  unique_groups <- unique(group_var)

  # Order time periods
  if (all(grepl("^-?\\d+\\.?\\d*$", unique_times))) {
    time_order <- order(as.numeric(unique_times))
  } else {
    time_order <- order(unique_times)
  }
  ordered_times <- unique_times[time_order]

  # Identify substantive variables (excluding group and time)
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Initialize result vectors
  total_counts <- integer(length(ordered_times))
  balanced_counts <- integer(length(ordered_times))
  complete_counts <- integer(length(ordered_times))

  # Calculate statistics for each time period
  for (i in seq_along(ordered_times)) {
    current_time <- ordered_times[i]

    # Get indices for current time period
    time_indices <- which(time_var == current_time)

    # Total count (all rows in this period)
    total_counts[i] <- length(time_indices)

    if (length(time_indices) > 0) {
      # Extract data for current time period
      period_data <- data[time_indices, substantive_vars, drop = FALSE]

      # Balanced: at least one non-NA value in substantive variables
      has_some_data <- apply(period_data, 1, function(x) any(!is.na(x)))
      balanced_counts[i] <- sum(has_some_data)

      # Complete: no NA values in substantive variables
      has_all_data <- apply(period_data, 1, function(x) all(!is.na(x)))
      complete_counts[i] <- sum(has_all_data)
    } else {
      balanced_counts[i] <- 0
      complete_counts[i] <- 0
    }
  }

  # Create result data.frame
  result_df <- data.frame(
    time_period = ordered_times,
    total = total_counts,
    balanced = balanced_counts,
    complete = complete_counts,
    stringsAsFactors = FALSE
  )

  # Rename first column to match the time variable name
  names(result_df)[1] <- time

  # Add standardized attributes
  attr(result_df, "panel_group") <- group
  attr(result_df, "panel_time") <- time
  attr(result_df, "panel_n_entities") <- length(unique_groups)
  attr(result_df, "panel_n_periods") <- length(ordered_times)
  attr(result_df, "panel_total_obs") <- nrow(data)

  return(result_df)
}
