#' Add Panel Attributes to Data
#'
#' Adds panel structure attributes to a data.frame, storing group and time variable names.
#' This allows panel functions to automatically detect panel structure.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable.
#' @param time A character string specifying the name of the time variable.
#'
#' @return The input data.frame with two additional attributes: panel_group and panel_time.
#'
#' @details
#' This function adds attributes to a data.frame to mark it as panel data.
#' The attributes store the names of group and time variables, enabling
#' panel-aware functions to automatically detect the panel structure without
#' requiring explicit specification of group and time variables in each function call.
#' The original data.frame structure is preserved.
#'
#' @seealso
#' [check_panel()], [get_attributes()], [describe_panel()], [explore_panel()]
#'
#' @examples
#' data(production)
#'
#' # Add panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#'
#' # Check the attributes
#' attr(panel_data, "panel_group")
#' attr(panel_data, "panel_time")
#'
#' # Use with describe_panel()
#' describe_panel(panel_data)
#'
#' @export
set_panel <- function(data, group, time) {
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

  # Add panel attributes to the data.frame
  attr(data, "panel_group") <- group
  attr(data, "panel_time") <- time

  return(data)
}

#' Check if Data Has Panel Attributes
#'
#' Helper function to check if a data.frame has panel attributes.
#'
#' @param data A data.frame to check.
#' @return Logical value indicating whether the data has panel attributes.
#'
#' @examples
#' data(production)
#' check_panel(production)  # FALSE
#'
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' check_panel(panel_data)  # TRUE
#'
#' @export
check_panel <- function(data) {
  !is.null(attr(data, "panel_group")) && !is.null(attr(data, "panel_time"))
}

#' Get Panel Attributes
#'
#' Helper function to extract panel attributes from a data.frame.
#'
#' @param data A data.frame, potentially with panel attributes.
#' @return A list with panel_group and panel_time if they exist, otherwise NULL.
#'
#' @examples
#' data(production)
#' get_attributes(production)  # NULL
#'
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' get_attributes(panel_data)  # list with group and time
#'
#' @export
get_attributes <- function(data) {
  if (is_panel(data)) {
    list(
      panel_group = attr(data, "panel_group"),
      panel_time = attr(data, "panel_time")
    )
  } else {
    NULL
  }
}
