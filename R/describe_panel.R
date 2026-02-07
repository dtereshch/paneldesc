#' Panel Data Structure Description
#'
#' This function provides basic summary information about panel data structure.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable
#'              in panel data. Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable in
#'             panel data. Not required if data has panel attributes.
#'
#' @return A data.frame with three columns:
#' \describe{
#'   \item{\code{observations}}{Total number of observations in the panel}
#'   \item{\code{entities}}{Number of unique groups/entities in the panel}
#'   \item{\code{periods}}{Number of unique time periods in the panel}
#' }
#'
#' @details
#' This function provides basic panel data structure information including
#' the number of unique entities, time periods, and total observations.
#' When provided with a data.frame that has panel attributes (created by set_panel()),
#' the function automatically extracts group and time variable names from the attributes.
#'
#' @examples
#' data(production)
#'
#' # Method 1: With regular data.frame
#' panel_desc <- describe_panel(production, group = "firm", time = "year")
#' print(panel_desc)
#'
#' # Method 2: With data.frame with panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' panel_desc <- describe_panel(panel_data)
#' print(panel_desc)
#'
#' @seealso
#' [explore_panel()], [set_panel()]
#'
#' @export
describe_panel <- function(data, group = NULL, time = NULL) {
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
  n_obs <- nrow(data)

  # Create result data.frame
  result <- data.frame(
    observations = n_obs,
    entities = n_groups,
    periods = n_periods,
    stringsAsFactors = FALSE
  )

  # Add class for potential future methods
  class(result) <- c("panel_description", "data.frame")

  return(result)
}
