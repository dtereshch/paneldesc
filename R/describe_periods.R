#' Time Periods Completeness Description
#'
#' This function calculates, for each time period, the number of entities (groups)
#' that have at least one non‑missing value in any substantive variable,
#' and the corresponding share of all entities.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#' @param digits An integer specifying the number of decimal places for rounding the share column.
#'        Default = 3.
#'
#' @return A data.frame with entities presence summary by time period.
#'
#' @details
#' The returned data.frame contains the following columns:
#' \describe{
#'   \item{\code{[time]}}{Time period identifier (name matches the input `time` argument)}
#'   \item{\code{count}}{Number of entities (groups) observed in that period,
#'         i.e., rows with at least one non‑NA value in substantive variables}
#'   \item{\code{share}}{Proportion of entities observed in that period,
#'         relative to the total number of unique entities in the data (0 to 1),
#'         rounded to `digits` decimal places.}
#' }
#'
#' Time periods are sorted naturally (numeric values as numbers, others alphabetically).
#'
#' The returned data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name, group, time, and digits.}
#'   \item{`details`}{List containing additional information: `entities`.
#'         This is a named list where names are time periods and values are vectors of
#'         entity identifiers that are observed (have at least one non‑NA) in that period.}
#' }
#'
#' @seealso
#' [plot_periods()], [describe_balance()], [describe_patterns()]
#'
#' @examples
#' data(production)
#' describe_periods(production, group = "firm", time = "year")
#'
#' # With custom rounding
#' describe_periods(production, group = "firm", time = "year", digits = 4)
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' describe_periods(panel_data)
#'
#' # Get entities observed in the 6th period
#' result <- describe_periods(production, group = "firm", time = "year")
#' attr(result, "details")$entities[["6"]]
#'
#' @export
describe_periods <- function(data, group = NULL, time = NULL, digits = 3) {
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
      sort(ux)
    }
  }

  # Helper function for rounding
  round_if_needed <- function(x, digits) {
    if (is.numeric(x) && !all(is.na(x))) {
      round(x, digits)
    } else {
      x
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

  # Harmonized digits validation
  if (!is.numeric(digits) || length(digits) != 1) {
    stop("'digits' must be a single non-negative integer")
  }
  if (digits < 0 || digits != round(digits)) {
    stop("'digits' must be a non-negative integer")
  }
  digits <- as.integer(digits)

  # Original vectors
  group_orig <- data[[group]]
  time_orig <- data[[time]]

  # Character versions for internal matching
  time_char <- as.character(time_orig)

  # Unique time periods in original class, sorted
  unique_times_orig <- sort_unique_preserve(time_orig)
  unique_times_char <- as.character(unique_times_orig)

  # Total number of unique entities (for share calculation)
  total_entities <- length(unique(group_orig))

  # Identify substantive variables (excluding group and time)
  substantive_vars <- setdiff(names(data), c(group, time))
  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Initialize result vectors and entity list
  period_counts <- integer(length(unique_times_orig))
  entities <- vector("list", length(unique_times_orig))
  names(entities) <- unique_times_char

  # Calculate observed statistics for each time period
  for (i in seq_along(unique_times_orig)) {
    current_time_char <- unique_times_char[i]
    time_indices <- which(time_char == current_time_char)

    if (length(time_indices) > 0) {
      period_data <- data[time_indices, substantive_vars, drop = FALSE]
      period_groups_orig <- group_orig[time_indices]

      # Observed: at least one non-NA value in substantive variables
      has_some_data <- apply(period_data, 1, function(x) any(!is.na(x)))
      period_counts[i] <- sum(has_some_data)

      if (any(has_some_data)) {
        entities[[i]] <- unique(period_groups_orig[has_some_data])
      } else {
        entities[[i]] <- vector(class(group_orig), 0)
      }
    } else {
      period_counts[i] <- 0
      entities[[i]] <- vector(class(group_orig), 0)
    }
  }

  # Compute share (proportion of total entities) and round
  share <- period_counts / total_entities
  share <- round_if_needed(share, digits)

  # Create result data.frame – first column uses original class time values
  result_df <- data.frame(
    time_period = unique_times_orig,
    count = period_counts,
    share = share,
    stringsAsFactors = FALSE
  )
  names(result_df)[1] <- time

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time,
    digits = digits
  )

  # Build details list
  details <- list(
    entities = entities
  )

  attr(result_df, "metadata") <- metadata
  attr(result_df, "details") <- details
  class(result_df) <- c("panel_description", "data.frame")

  return(result_df)
}
