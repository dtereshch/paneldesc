#' Entities Presence Patterns Description
#'
#' This function describes entities presence patterns in panel data over time.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#' @param interval An optional positive integer giving the expected interval between time periods.
#' @param format A character string specifying the output format: "wide" or "long". Default = "wide".
#' @param detailed A logical flag indicating whether to return detailed patterns. Default = TRUE.
#' @param digits An integer specifying the number of decimal places for rounding share column.
#'        Default = 3.
#'
#' @return A data.frame with presence patterns.
#'
#' @details
#' An entity/time combination is considered **present** if the corresponding row contains at least
#' one non-NA value in any substantive variable (i.e., all columns except the group and time identifiers).
#'
#' If `interval` is supplied, the time variable is coerced to numeric (if possible).
#' The function then checks that all observed time points are compatible with a regular spacing
#' of that interval. If gaps are detected, a message lists the missing periods (unless the interval
#' was inherited from panel attributes), and columns for those periods are added to the presence matrix
#' (all zeros) before computing patterns. The output data.frame will therefore include those missing
#' periods as additional columns (in wide format) or additional rows (in long format). The `details`
#' attribute is updated accordingly, but no new vectors are added beyond those already present.
#'
#' The output format depends on the `format` and `detailed` parameters:
#' \describe{
#'   \item{\code{format = "wide"}, \code{detailed = TRUE}}{Columns: pattern, [time_periods], count, share.}
#'   \item{\code{format = "long"}, \code{detailed = TRUE}}{Columns: pattern, time, presence, count, share.}
#'   \item{\code{detailed = FALSE}}{Only pattern and time period columns (or presence in long format).}
#' }
#'
#' Patterns are sorted by frequency (most common first).
#'
#' The returned data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing additional information: `count_patterns`, `presence_matrix`,
#'         `pattern_groups`. The `pattern_groups` element is a list where each element corresponds
#'         to a pattern identifier and contains the entity IDs that follow that pattern.}
#' }
#'
#' @seealso
#' [plot_patterns()], [describe_periods()], [describe_incomplete()], [describe_balance()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' describe_patterns(production, group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' describe_patterns(panel_data)
#'
#' # Specify interval to fill gaps
#' describe_patterns(production, group = "firm", time = "year", interval = 1)
#'
#' # Simplified version
#' describe_patterns(production, group = "firm", time = "year", detailed = FALSE)
#'
#' # Using pattern_groups to extract entities with specific patterns
#' patterns <- describe_patterns(production, group = "firm", time = "year")
#' pattern_groups <- attr(patterns, "details")$pattern_groups
#' most_common_pattern_entities <- pattern_groups[["1"]]
#'
#' @export
describe_patterns <- function(
  data,
  group = NULL,
  time = NULL,
  interval = NULL,
  format = "wide",
  detailed = TRUE,
  digits = 3
) {
  # Capture original interval argument
  user_interval <- interval

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
  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("'detailed' must be a single logical value, not ", class(detailed)[1])
  }
  if (!is.character(format) || length(format) != 1) {
    stop("'format' must be a single character string, not ", class(format)[1])
  }
  if (!format %in% c("wide", "long")) {
    stop('format must be either "wide" or "long", not "', format, '"')
  }
  if (!is.numeric(digits) || length(digits) != 1) {
    stop("'digits' must be a single non-negative integer")
  }
  if (digits < 0 || digits != round(digits)) {
    stop("'digits' must be a non-negative integer")
  }
  digits <- as.integer(digits)

  # --- Interval handling ---
  if (!is.null(interval)) {
    if (
      !is.numeric(interval) ||
        length(interval) != 1 ||
        interval <= 0 ||
        interval != round(interval)
    ) {
      stop("'interval' must be a positive integer")
    }
    # Coerce time to numeric if needed
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

    # Check consistency
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

  # Character versions for internal use
  group_char <- as.character(group_orig)
  time_char <- as.character(time_orig)

  # Sorted unique values in original class (for storage)
  unique_groups_orig <- sort_unique_preserve(group_orig)
  unique_periods_orig <- sort_unique_preserve(time_orig)

  # If interval used, we may need to expand periods to include missing ones.
  # We'll use numeric periods for matrix construction.
  if (!is.null(interval)) {
    # Use full_seq (which includes missing) as the time columns
    time_cols_numeric <- full_seq
    time_cols_char <- as.character(time_cols_numeric)
  } else {
    time_cols_char <- as.character(unique_periods_orig)
  }

  # Character versions for dimnames etc.
  unique_groups_char <- as.character(unique_groups_orig)

  # Identify data columns (excluding group and time)
  data_cols <- setdiff(names(data), c(group, time))
  if (length(data_cols) == 0) {
    stop("no data columns found (excluding group and time variables)")
  }

  # Filter data based on "observed" definition:
  # rows with at least one non-NA in substantive variables.
  if (nrow(data) > 0) {
    has_data <- apply(data[data_cols], 1, function(row) !all(is.na(row)))
    data_filtered <- data[has_data, ]
  } else {
    data_filtered <- data
  }

  # Character vectors for filtered data (if any)
  if (nrow(data_filtered) > 0) {
    group_filt_char <- as.character(data_filtered[[group]])
    time_filt_char <- as.character(data_filtered[[time]])
  } else {
    group_filt_char <- character(0)
    time_filt_char <- character(0)
  }

  # Create a matrix of all possible combinations (initialize with 0)
  presence_binary <- matrix(
    0,
    nrow = length(unique_groups_orig),
    ncol = length(time_cols_char),
    dimnames = list(unique_groups_char, time_cols_char)
  )

  # Fill the binary matrix: mark 1 for all rows in filtered data
  # If interval used, time_filt_char corresponds to original observed periods (character).
  # They should match some of the time_cols_char.
  for (i in seq_along(group_filt_char)) {
    if (time_filt_char[i] %in% time_cols_char) {
      presence_binary[group_filt_char[i], time_filt_char[i]] <- 1
    }
  }

  # Count patterns and create pattern groups (store original class group values)
  pattern_strings <- apply(presence_binary, 1, function(x) {
    paste(x, collapse = "")
  })
  pattern_counts <- table(pattern_strings)

  # Create pattern_groups list with original class group identifiers
  pattern_groups <- list()
  for (i in seq_along(unique_groups_orig)) {
    grp_orig <- unique_groups_orig[i]
    grp_char <- unique_groups_char[i]
    pat_str <- pattern_strings[i]
    if (!pat_str %in% names(pattern_groups)) {
      pattern_groups[[pat_str]] <- vector(class(grp_orig), 0)
    }
    pattern_groups[[pat_str]] <- c(pattern_groups[[pat_str]], grp_orig)
  }

  # Create result data frame
  result <- data.frame(
    pattern = seq_along(pattern_counts),
    stringsAsFactors = FALSE
  )

  # Add time period columns (preserve order: time_cols_char)
  for (t in time_cols_char) {
    result[[t]] <- as.numeric(substr(
      names(pattern_counts),
      which(time_cols_char == t),
      which(time_cols_char == t)
    ))
  }

  # Add count and share columns
  result$count <- as.numeric(pattern_counts)
  result$share <- round_if_needed(result$count / sum(result$count), digits)

  # Sort by count (descending)
  result <- result[order(-result$count), ]
  result$pattern <- seq_len(nrow(result))
  rownames(result) <- NULL

  # Reorder pattern_groups to match sorted patterns
  sorted_pattern_strings <- apply(result[time_cols_char], 1, function(x) {
    paste(x, collapse = "")
  })
  pattern_groups_sorted <- list()
  for (i in seq_along(sorted_pattern_strings)) {
    pat_str <- sorted_pattern_strings[i]
    pattern_groups_sorted[[as.character(i)]] <- pattern_groups[[pat_str]]
  }
  pattern_groups <- pattern_groups_sorted

  # Build base details list
  details_base <- list(
    count_patterns = nrow(result),
    presence_matrix = presence_binary,
    pattern_groups = pattern_groups
  )

  # Convert to long format if requested
  if (format == "long") {
    long_result <- data.frame()

    for (i in seq_len(nrow(result))) {
      pattern_row <- result[i, ]

      for (t in time_cols_char) {
        long_result <- rbind(
          long_result,
          data.frame(
            pattern = pattern_row$pattern,
            time = t,
            presence = as.integer(pattern_row[[t]]),
            count = pattern_row$count,
            share = pattern_row$share,
            stringsAsFactors = FALSE
          )
        )
      }
    }

    # Rename the "time" column to match original time variable name
    names(long_result)[names(long_result) == "time"] <- time

    if (!detailed) {
      simplified_result <- long_result[c("pattern", time, "presence")]
      attr(simplified_result, "metadata") <- list(
        function_name = as.character(match.call()[[1]]),
        group = group,
        time = time,
        interval = interval,
        format = format,
        detailed = detailed,
        digits = digits
      )
      attr(simplified_result, "details") <- details_base
      class(simplified_result) <- c("panel_description", "data.frame")
      return(simplified_result)
    }

    attr(long_result, "metadata") <- list(
      function_name = as.character(match.call()[[1]]),
      group = group,
      time = time,
      interval = interval,
      format = format,
      detailed = detailed,
      digits = digits
    )
    attr(long_result, "details") <- details_base
    class(long_result) <- c("panel_description", "data.frame")
    return(long_result)
  }

  # Wide format handling
  if (!detailed) {
    simplified_result <- result[c("pattern", time_cols_char)]
    attr(simplified_result, "metadata") <- list(
      function_name = as.character(match.call()[[1]]),
      group = group,
      time = time,
      interval = interval,
      format = format,
      detailed = detailed,
      digits = digits
    )
    attr(simplified_result, "details") <- details_base
    class(simplified_result) <- c("panel_description", "data.frame")
    return(simplified_result)
  }

  # Full wide result
  attr(result, "metadata") <- list(
    function_name = as.character(match.call()[[1]]),
    group = group,
    time = time,
    interval = interval,
    format = format,
    detailed = detailed,
    digits = digits
  )
  attr(result, "details") <- details_base
  class(result) <- c("panel_description", "data.frame")

  return(result)
}
