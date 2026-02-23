#' Panel Data Dimensions Description
#'
#' This function provides basic dimension counts for panel data:
#' number of observations (rows with data), unique entities, unique time periods,
#' and substantive variables.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable in panel data.
#'        Not required if data has panel attributes.
#'
#' @return A data.frame with panel dimension counts. Contains two columns:
#'   \describe{
#'     \item{\code{dimension}}{Dimension name: "observations", "entities", "periods", "variables"}
#'     \item{\code{count}}{Corresponding count (integer).}
#'   }
#'
#' @details
#' The counts are defined as follows:
#' \itemize{
#'   \item{\bold{observations}}{ Number of rows that contain at least one non‑missing value
#'         in any substantive variable (i.e., columns other than the group and time identifiers).}
#'   \item{\bold{entities}}{ Number of distinct values in the group variable.}
#'   \item{\bold{periods}}{ Number of distinct values in the time variable.}
#'   \item{\bold{variables}}{ Number of substantive variables (all columns except group and time).}
#' }
#'
#' The returned data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing:
#'         \itemize{
#'           \item{\code{entities}: Vector of unique entity identifiers (original class).}
#'           \item{\code{periods}: Vector of unique time period identifiers (original class).}
#'           \item{\code{variables}: Character vector of substantive variable names.}
#'         }
#'   }
#' }
#'
#' @seealso
#' [check_panel()], [describe_balance()], [describe_periods()], [describe_patterns()], [set_panel()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' describe_dimensions(production, group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' describe_dimensions(panel_data)
#'
#' # Access detailed information
#' dims <- describe_dimensions(production, group = "firm", time = "year")
#' attr(dims, "details")$entities
#' attr(dims, "details")$variables
#'
#' @export
describe_dimensions <- function(data, group = NULL, time = NULL) {
  # Helper to sort unique values preserving original class
  sort_unique_preserve <- function(x) {
    ux <- unique(x)
    if (is.numeric(ux)) {
      sort(ux)
    } else if (inherits(ux, "Date") || inherits(ux, "POSIXt")) {
      sort(ux)
    } else if (is.factor(ux)) {
      char_lev <- as.character(ux)
      sorted_char <- sort(char_lev)
      factor(sorted_char, levels = sorted_char, ordered = is.ordered(ux))
    } else {
      sort(ux) # character, logical, etc.
    }
  }

  # Check for panel_data class and extract info from metadata
  if (inherits(data, "panel_data")) {
    metadata <- attr(data, "metadata")
    if (
      is.null(metadata) || is.null(metadata$group) || is.null(metadata$time)
    ) {
      stop(
        "Object has class 'panel_data' but missing or incomplete 'metadata' attribute."
      )
    }
    group <- metadata$group
    time <- metadata$time
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

  if (!group %in% names(data)) {
    stop('group variable "', group, '" not found in data')
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string, not ", class(time)[1])
  }

  if (!time %in% names(data)) {
    stop('time variable "', time, '" not found in data')
  }

  if (time == group) {
    stop("'time' and 'group' cannot be the same variable")
  }

  # Original vectors
  group_orig <- data[[group]]
  time_orig <- data[[time]]

  # Unique values, sorted, preserving original class (for details)
  entities_vals <- sort_unique_preserve(group_orig)
  periods_vals <- sort_unique_preserve(time_orig)

  # Substantive variables (all except group and time)
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Number of observations (rows with at least one non-NA in substantive variables)
  obs_count <- sum(apply(data[substantive_vars], 1, function(x) any(!is.na(x))))

  # Counts
  counts <- c(
    observations = obs_count,
    entities = length(entities_vals),
    periods = length(periods_vals),
    variables = length(substantive_vars)
  )

  # Result data.frame
  result <- data.frame(
    dimension = names(counts),
    count = as.integer(counts),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time
  )

  # Build details list with original class vectors and variable names
  details <- list(
    entities = entities_vals,
    periods = periods_vals,
    variables = substantive_vars
  )

  # Set attributes and class
  attr(result, "metadata") <- metadata
  attr(result, "details") <- details
  class(result) <- c("panel_description", "data.frame")

  return(result)
}
