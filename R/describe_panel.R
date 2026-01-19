#' Panel Data Structure Description
#'
#' This function provides a summary of panel data structure and validity.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#' @param time A character string specifying the name of the time variable in panel data.
#'
#' @return A data.frame with four columns:
#' \describe{
#'   \item{\code{entities}}{Number of unique groups/entities in the panel}
#'   \item{\code{time_periods}}{Number of unique time periods in the panel}
#'   \item{\code{observations}}{Total number of observations in the panel}
#'   \item{\code{status}}{Panel validity status: "valid" or "invalid"}
#' }
#'
#' @details
#' This function performs a simplified version of the exploration done by
#' \code{\link{explore_panel}}. It checks for:
#' \itemize{
#'   \item Basic data structure validity
#'   \item Presence of group and time variables
#'   \item Duplicate group-time combinations
#'   \item Missing values in group/time variables
#' }
#'
#' If the panel is marked as "invalid", a message is printed to the console
#' recommending the use of \code{\link{explore_panel}} for more detailed analysis.
#'
#' @examples
#' data(production)
#'
#' # Get panel description
#' panel_desc <- describe_panel(production, group = "firm", time = "year")
#' print(panel_desc)
#'
#' # If status is "invalid", use explore_panel() for detailed analysis
#' if (panel_desc$status == "invalid") {
#'   explore_panel(production, group = "firm", time = "year", detailed = TRUE)
#' }
#'
#' @seealso
#' [explore_panel()], [describe_balance()], [describe_periods()], [describe_participation()]
#'
#' @export
describe_panel <- function(data, group, time) {
  # Input validation (similar to explore_panel but without warning messages)
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

  # Check for validity issues
  validity_checks <- list(
    has_duplicates = any(duplicated(paste(
      data[[group]],
      data[[time]],
      sep = "|"
    ))),
    has_na_group = any(is.na(data[[group]])),
    has_na_time = any(is.na(data[[time]]))
  )

  # Determine if panel is valid
  is_valid <- !any(unlist(validity_checks))

  # Create status
  status <- ifelse(is_valid, "valid", "invalid")

  # Print note to console if panel is not valid
  if (!is_valid) {
    issues <- character()
    if (validity_checks$has_duplicates) {
      issues <- c(issues, "duplicate group-time pairs")
    }
    if (validity_checks$has_na_group) {
      issues <- c(issues, "missing values in group variable")
    }
    if (validity_checks$has_na_time) {
      issues <- c(issues, "missing values in time variable")
    }

    message(
      "Note: Panel has ",
      paste(issues, collapse = ", "),
      ". Use explore_panel() for detailed analysis."
    )
  }

  # Create result data.frame
  result <- data.frame(
    entities = n_groups,
    time_periods = n_periods,
    observations = n_obs,
    status = status,
    stringsAsFactors = FALSE
  )

  # Add class for potential future methods
  class(result) <- c("panel_description", "data.frame")

  return(result)
}
