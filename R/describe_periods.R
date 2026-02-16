#' Time Periods Completeness Description
#'
#' This function caluclates number of enitities/groups for each time period in panel data.
#' The function uses different ways to detect entity/group presence in each period.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#'
#' @return A data.frame with entities/groups presence summary by time period.
#'
#' @details
#' The data.frame contains the following columns:
#' \describe{
#'   \item{\code{[time]}}{Time period identifier (name matches the input `time` argument)}
#'   \item{\code{nominal}}{Number of rows in each period (all rows, regardless of data completeness)}
#'   \item{\code{observed}}{Number of rows with at least one non-NA value in substantive variables}
#'   \item{\code{complete}}{Number of rows without any NA values in substantive variables}
#' }
#'
#' Time periods are sorted naturally (numeric values as numbers, others alphabetically).
#'
#' The returned data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`panel_info`}{Named character vector with elements `group_var` and `time_var`.}
#'   \item{`details`}{List containing additional information: `n_entities`, `n_periods`, `total_rows`.}
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#' }
#'
#' @seealso
#' [plot_periods()], [describe_balance()], [describe_patterns()]
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
  # Check for panel_data class and extract info
  if (inherits(data, "panel_data")) {
    panel_info <- attr(data, "panel_info")
    if (
      is.null(panel_info) ||
        is.null(panel_info["group_var"]) ||
        is.null(panel_info["time_var"])
    ) {
      stop(
        "Object has class 'panel_data' but missing or incomplete 'panel_info' attribute."
      )
    }
    group <- panel_info["group_var"]
    time <- panel_info["time_var"]
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
  nominal_counts <- integer(length(ordered_times))
  observed_counts <- integer(length(ordered_times))
  complete_counts <- integer(length(ordered_times))

  # Calculate statistics for each time period
  for (i in seq_along(ordered_times)) {
    current_time <- ordered_times[i]

    # Get indices for current time period
    time_indices <- which(time_var == current_time)

    # Nominal count (all rows in this period)
    nominal_counts[i] <- length(time_indices)

    if (length(time_indices) > 0) {
      # Extract data for current time period
      period_data <- data[time_indices, substantive_vars, drop = FALSE]

      # Observed: at least one non-NA value in substantive variables
      has_some_data <- apply(period_data, 1, function(x) any(!is.na(x)))
      observed_counts[i] <- sum(has_some_data)

      # Complete: no NA values in substantive variables
      has_all_data <- apply(period_data, 1, function(x) all(!is.na(x)))
      complete_counts[i] <- sum(has_all_data)
    } else {
      observed_counts[i] <- 0
      complete_counts[i] <- 0
    }
  }

  # Create result data.frame
  result_df <- data.frame(
    time_period = ordered_times,
    nominal = nominal_counts,
    observed = observed_counts,
    complete = complete_counts,
    stringsAsFactors = FALSE
  )

  # Rename first column to match the time variable name
  names(result_df)[1] <- time

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time
  )

  # Build details list
  details <- list(
    n_entities = length(unique_groups),
    n_periods = length(ordered_times),
    total_rows = nrow(data)
  )

  # Set attributes in desired order
  attr(result_df, "panel_info") <- c(group_var = group, time_var = time)
  attr(result_df, "details") <- details
  attr(result_df, "metadata") <- metadata

  # Set class
  class(result_df) <- c("panel_description", "data.frame")

  return(result_df)
}
