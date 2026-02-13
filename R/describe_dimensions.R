#' Panel Data Dimensions Description
#'
#' This function provides basic summary information about panel data structure.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable in panel data.
#'        Not required if data has panel attributes.
#'
#' @return A data.frame with two columns:
#' \describe{
#'   \item{\code{dimension}}{Dimension name: "rows", "entities", or "periods"}
#'   \item{\code{count}}{The count for each dimension}
#' }
#'
#' The data.frame has additional attributes:
#' \describe{
#'   \item{\code{panel_group}}{The grouping variable name}
#'   \item{\code{panel_time}}{The time variable name}
#'   \item{\code{entity_values}}{Vector of all unique entity/group values}
#'   \item{\code{period_values}}{Vector of all unique time period values}
#' }
#'
#' @details
#' This function provides basic panel data structure information including
#' the number of unique entities, time periods, and total rows.
#' When provided with a data.frame that has panel attributes (created by set_panel()),
#' the function automatically extracts group and time variable names from the attributes.
#'
#' @examples
#' data(production)
#'
#' # Method 1: With regular data.frame
#' panel_desc <- describe_dimensions(production, group = "firm", time = "year")
#' print(panel_desc)
#'
#' # Method 2: With data.frame with panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' panel_desc <- describe_dimensions(panel_data)
#' print(panel_desc)
#'
#' @seealso
#' [check_panel()], [describe_balance()], [describe_periods()], [set_panel()]
#'
#' @export
describe_dimensions <- function(data, group = NULL, time = NULL) {
  # Check if data has panel attributes
  has_panel_attrs <- !is.null(attr(data, "panel_group")) &&
    !is.null(attr(data, "panel_time"))

  if (has_panel_attrs) {
    # Extract group and time from attributes
    group <- attr(data, "panel_group")
    time <- attr(data, "panel_time")
  } else {
    # Handle regular data.frame
    # Input validation for regular data.frame
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame, not ", class(data)[1])
    }

    if (is.null(group) || is.null(time)) {
      stop(
        "For regular data.frames, both 'group' and 'time' arguments must be provided"
      )
    }
  }

  # Common validation for both cases
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string")
  }

  if (!group %in% names(data)) {
    stop('group variable "', group, '" not found in data')
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string")
  }

  if (!time %in% names(data)) {
    stop('time variable "', time, '" not found in data')
  }

  if (time == group) {
    stop("'time' and 'group' cannot be the same variable")
  }

  # Basic panel statistics
  n_groups <- length(unique(data[[group]]))
  n_periods <- length(unique(data[[time]]))
  n_rows <- nrow(data)

  # Get unique values
  entity_values <- sort(unique(data[[group]]))
  period_values <- sort(unique(data[[time]]))

  # Create result data.frame with transposed structure
  result <- data.frame(
    dimension = c("rows", "entities", "periods"),
    count = c(n_rows, n_groups, n_periods),
    stringsAsFactors = FALSE
  )

  # Add standardized attributes
  attr(result, "panel_group") <- group
  attr(result, "panel_time") <- time
  attr(result, "entity_values") <- entity_values
  attr(result, "period_values") <- period_values

  return(result)
}
