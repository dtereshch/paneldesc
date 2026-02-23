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
#' @param interval An optional positive integer giving the expected interval between time periods.
#' @param digits An integer specifying the number of decimal places for rounding the share column.
#'        Default = 3.
#'
#' @return A data.frame with entities presence summary by time period.
#'
#' @details
#' The returned data.frame contains the following columns:
#' \describe{
#'   \item{\code{[time]}}{Time period identifier (name matches the input `time` argument)}
#'   \item{\code{count}}{Number of distinct entities (groups) observed in that period,
#'         i.e., entities with at least one row containing a non‑NA value in substantive variables}
#'   \item{\code{share}}{Proportion of entities observed in that period,
#'         relative to the total number of unique entities in the data (0 to 1),
#'         rounded to `digits` decimal places.}
#' }
#'
#' If `interval` is supplied, the time variable is coerced to numeric (if possible).
#' The function then checks that all observed time points are compatible with a regular spacing
#' of that interval. If gaps are detected, a message lists the missing periods (unless the interval
#' was inherited from panel attributes), and rows for those periods are added to the output with
#' `count = 0` and `share = 0`. The `entities` list in the `details` attribute includes empty vectors
#' for those missing periods.
#'
#' The function also checks for duplicate group-time combinations. In a properly structured panel dataset,
#' each entity (group) should have at most one observation per time period. If duplicates are found,
#' they are stored in `details$entity_time_duplicates`. A message is printed only when the group and time
#' variables were explicitly provided (i.e., not taken from `panel_data` attributes).
#'
#' Time periods are sorted naturally (numeric order).
#'
#' The returned data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name, group, time, interval, and digits.}
#'   \item{`details`}{List containing additional information: `entities` (a named list where
#'         names are time periods and values are vectors of entity identifiers observed in that period)
#'         and, if duplicates were found, `entity_time_duplicates`.}
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
#' # Specify interval to fill gaps (if any)
#' describe_periods(production, group = "firm", time = "year", interval = 1)
#'
#' # Get entities observed in the 6th period
#' result <- describe_periods(production, group = "firm", time = "year")
#' attr(result, "details")$entities[["6"]]
#'
#' @export
describe_periods <- function(
  data,
  group = NULL,
  time = NULL,
  interval = NULL,
  digits = 3
) {
  # Capture original interval argument to distinguish user-supplied vs inherited
  user_interval <- interval

  # Helper to sort unique values preserving original class (for non‑numeric)
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

  # Determine if group/time came from metadata
  group_time_from_metadata <- FALSE

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
    group_time_from_metadata <- TRUE
    # Use interval from metadata if not overridden by explicit argument
    if (is.null(interval) && !is.null(metadata$interval)) {
      interval <- metadata$interval
    }
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

  # Determine if interval came from metadata
  interval_from_metadata <- is.null(user_interval) && !is.null(interval)

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

  # --- Check for duplicate group-time combinations ---
  dup_combinations <- NULL
  dup_rows <- duplicated(data[c(group, time)]) |
    duplicated(data[c(group, time)], fromLast = TRUE)
  if (any(dup_rows)) {
    dup_combinations <- unique(data[dup_rows, c(group, time), drop = FALSE])
    n_dup <- nrow(dup_combinations)
    if (!group_time_from_metadata) {
      examples <- utils::head(dup_combinations, 5)
      example_strings <- paste0(examples[[group]], "-", examples[[time]])
      example_str <- paste(example_strings, collapse = ", ")
      message(
        n_dup,
        " duplicate group-time combinations found. Examples: ",
        example_str
      )
    }
  }
  # ----------------------------------------------------

  # --- Interval handling ---
  if (!is.null(interval)) {
    # Validate interval
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

    # Check consistency: observed time points must be equally spaced by multiples of interval
    obs_periods <- sort(unique(data[[time]]))
    time_diffs <- diff(obs_periods)
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

    # Compute full sequence and missing periods
    full_seq <- seq(
      from = min(obs_periods),
      to = max(obs_periods),
      by = interval
    )
    missing <- setdiff(full_seq, obs_periods)
    if (length(missing) > 0 && !interval_from_metadata) {
      message(
        "Irregular time intervals detected. Missing periods: ",
        paste(missing, collapse = ", ")
      )
    }
  }

  # Original vectors
  group_orig <- data[[group]]
  time_orig <- data[[time]]

  # Character versions for internal matching
  time_char <- as.character(time_orig)

  # Unique time periods (numeric if interval used, else original class)
  if (!is.null(interval)) {
    # Use numeric periods (including missing) for sorting and output
    unique_times <- sort(unique(data[[time]]))
    if (exists("missing") && length(missing) > 0) {
      # Add missing periods and re-sort
      unique_times <- sort(c(unique_times, missing))
    }
  } else {
    unique_times <- sort_unique_preserve(time_orig)
  }
  unique_times_char <- as.character(unique_times)

  # Total number of unique entities
  total_entities <- length(unique(group_orig))

  # Identify substantive variables (excluding group and time)
  substantive_vars <- setdiff(names(data), c(group, time))
  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Initialize result vectors and entity list
  period_counts <- integer(length(unique_times))
  entities <- vector("list", length(unique_times))
  names(entities) <- unique_times_char

  # Calculate observed statistics for each time period
  for (i in seq_along(unique_times)) {
    current_time <- unique_times[i]
    current_time_char <- unique_times_char[i]

    if (!is.null(interval) && exists("missing") && current_time %in% missing) {
      # Missing period: count = 0, empty entity vector
      period_counts[i] <- 0
      entities[[i]] <- vector(class(group_orig), 0)
    } else {
      # Indices of rows with this time value
      time_indices <- which(time_orig == current_time) # works because coerced if needed
      if (length(time_indices) > 0) {
        # Subset data for this period
        period_data <- data[time_indices, substantive_vars, drop = FALSE]
        period_groups_orig <- group_orig[time_indices]

        # Observed: at least one non-NA value in substantive variables
        has_some_data <- apply(period_data, 1, function(x) any(!is.na(x)))

        # Count distinct entities with data in this period
        if (any(has_some_data)) {
          period_counts[i] <- length(unique(period_groups_orig[has_some_data]))
          entities[[i]] <- unique(period_groups_orig[has_some_data])
        } else {
          period_counts[i] <- 0
          entities[[i]] <- vector(class(group_orig), 0)
        }
      } else {
        period_counts[i] <- 0
        entities[[i]] <- vector(class(group_orig), 0)
      }
    }
  }

  # Compute share (proportion of total entities) and round
  share <- period_counts / total_entities
  share <- round_if_needed(share, digits)

  # Create result data.frame – first column uses the computed time periods (numeric if interval used)
  result_df <- data.frame(
    time_period = unique_times,
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
    interval = interval,
    digits = digits
  )

  # Build details list
  details <- list(
    entities = entities
  )

  # Add duplicate combinations if any were found
  if (!is.null(dup_combinations)) {
    details$entity_time_duplicates <- dup_combinations
  }

  attr(result_df, "metadata") <- metadata
  attr(result_df, "details") <- details
  class(result_df) <- c("panel_description", "data.frame")

  return(result_df)
}
