#' Panel Data Structure Description
#'
#' This function provides basic summary information about panel data structure.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#' @param time A character string specifying the name of the time variable in panel data.
#'
#' @return A data.frame with three columns:
#' \describe{
#'   \item{\code{entities}}{Number of unique groups/entities in the panel}
#'   \item{\code{time_periods}}{Number of unique time periods in the panel}
#'   \item{\code{observations}}{Total number of observations in the panel}
#' }
#'
#' @details
#' This function provides basic panel data structure information including
#' the number of unique entities, time periods, and total observations.
#'
#' @examples
#' data(production)
#'
#' # Get panel description
#' panel_desc <- describe_panel(production, group = "firm", time = "year")
#' print(panel_desc)
#'
#' @seealso
#' [explore_panel()] for detailed panel analysis
#'
#' @export
describe_panel <- function(data, group, time) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
  }

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
    entities = n_groups,
    time_periods = n_periods,
    observations = n_obs,
    stringsAsFactors = FALSE
  )

  # Add class for potential future methods
  class(result) <- c("panel_description", "data.frame")

  return(result)
}
