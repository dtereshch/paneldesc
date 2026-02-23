#' Panel Data Structure Setting
#'
#' This function adds panel structure attributes to a data.frame, storing entity/group and time variable names,
#' and optionally checks the expected interval between time periods.
#' This allows panel functions to automatically detect panel structure.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param group A character string specifying the name of the entity/group variable.
#' @param time A character string specifying the name of the time variable.
#' @param interval An optional positive integer giving the expected interval between time periods.
#'
#' @return The input data.frame with additional attributes.
#'
#' @details
#' This function adds attributes to a data.frame to mark it as panel data.
#' The returned object has class `"panel_data"` (in addition to its original class).
#' It includes the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used
#'         (`group`, `time`, and `interval` if provided).}
#'   \item{`details`}{List with diagnostic vectors. By default it contains:
#'         \describe{
#'           \item{`entities`}{Unique values of the group variable.}
#'           \item{`periods`}{Sorted unique values of the time variable.}
#'         }
#'         If duplicates of the group-time combination are found, an additional vector is included:
#'         \describe{
#'           \item{`entity_time_duplicates`}{A data frame containing the distinct duplicate
#'                 combinations of group and time.}
#'         }
#'         If `interval` is supplied and missing periods are detected, two additional vectors are included:
#'         \describe{
#'           \item{`periods_restored`}{The full sequence of periods from `min(time)` to `max(time)` by `interval`.}
#'           \item{`periods_missing`}{The periods in `periods_restored` that are not present in the data.}
#'         }}
#' }
#'
#' The function checks for duplicate group-time combinations. In a properly structured panel dataset,
#' each entity (group) should have at most one observation per time period. If duplicates are found,
#' a one‑line message is printed with the number of distinct duplicate combinations and up to five examples.
#' The duplicate combinations are stored in `details$entity_time_duplicates` for further inspection.
#'
#' When `interval` is specified, the function first attempts to convert the time variable to numeric
#' (if it is not already). If conversion fails, an error is raised. It then checks whether the observed
#' time points are compatible with a regular spacing of that interval. If any observed difference is not
#' a multiple of the interval, an error is raised. If all differences are multiples of the interval but
#' there are gaps, a message is printed listing the missing periods, and the analysis continues.
#' In this case, the restored full sequence and the missing periods are stored in
#' `details$periods_restored` and `details$periods_missing`, respectively.
#'
#' @seealso
#' [check_panel()], [describe_dimensions()], [describe_balance()]
#'
#' @examples
#' data(production)
#'
#' # Add panel attributes without interval check
#' panel_data <- set_panel(production, group = "firm", time = "year")
#'
#' # Check the attributes
#' attr(panel_data, "metadata")
#' attr(panel_data, "details")
#'
#' # Use with describe_dimensions()
#' describe_dimensions(panel_data)
#'
#' # With interval specification (assuming yearly data)
#' panel_data2 <- set_panel(production, group = "firm", time = "year", interval = 1)
#'
#' @export
set_panel <- function(data, group, time, interval = NULL) {
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

  # --- Check for duplicate group-time combinations (before any time conversion) ---
  dup_combinations <- NULL
  dup_rows <- duplicated(data[c(group, time)]) |
    duplicated(data[c(group, time)], fromLast = TRUE)
  if (any(dup_rows)) {
    # Extract the duplicate combinations (unique ones) for reporting
    dup_combinations <- unique(data[dup_rows, c(group, time), drop = FALSE])
    n_dup <- nrow(dup_combinations)
    # Prepare up to five examples as a single string
    examples <- utils::head(dup_combinations, 5)
    example_strings <- paste0(examples[[group]], "-", examples[[time]])
    example_str <- paste(example_strings, collapse = ", ")
    message(
      n_dup,
      " duplicate group-time combinations found. Examples: ",
      example_str
    )
  }
  # --------------------------------------------------------------------------------

  # Validate interval if provided
  if (!is.null(interval)) {
    if (
      !is.numeric(interval) ||
        length(interval) != 1 ||
        interval <= 0 ||
        interval != round(interval)
    ) {
      stop("'interval' must be a positive integer")
    }

    # Attempt to coerce time variable to numeric
    time_vals_orig <- data[[time]]
    if (!is.numeric(time_vals_orig)) {
      time_numeric <- suppressWarnings(as.numeric(as.character(time_vals_orig)))
      if (anyNA(time_numeric)) {
        stop(
          "Cannot convert the time variable '",
          time,
          "' to numeric. ",
          "Please ensure it contains numbers or convert it manually."
        )
      }
      data[[time]] <- time_numeric
    }
  }

  # Build metadata (function call arguments)
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time,
    interval = interval
  )

  # Base details: entities and periods
  entities <- unique(data[[group]])
  periods <- sort(unique(data[[time]]))

  details <- list(
    entities = entities,
    periods = periods
  )

  # Add duplicate combinations if any were found
  if (!is.null(dup_combinations)) {
    details$entity_time_duplicates <- dup_combinations
  }

  # If interval is given, perform regularity checks and compute missing periods
  if (!is.null(interval)) {
    # Check consistency: all observed times must be equally spaced by multiples of interval
    time_diffs <- diff(periods)
    if (!all(time_diffs %% interval == 0)) {
      stop(
        "The observed time points are not evenly spaced by multiples of the specified interval (",
        interval,
        "). ",
        "For example, differences such as ",
        paste(unique(time_diffs[time_diffs %% interval != 0]), collapse = ", "),
        " are not multiples of ",
        interval,
        "."
      )
    }

    # Compute restored full sequence and missing periods
    full_seq <- seq(from = min(periods), to = max(periods), by = interval)
    missing <- setdiff(full_seq, periods)

    if (length(missing) > 0) {
      # Add extra diagnostic vectors only when gaps exist
      details$periods_restored <- full_seq
      details$periods_missing <- missing

      msg <- paste(
        "Irregular time intervals detected. Missing periods:",
        paste(missing, collapse = ", ")
      )
      message(msg)
    }
  }

  # Add attributes and class
  attr(data, "metadata") <- metadata
  attr(data, "details") <- details
  class(data) <- c("panel_data", class(data))

  return(data)
}
