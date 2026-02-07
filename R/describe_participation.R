#' Panel Data Participation Patterns
#'
#' This function describes participation patterns of entities in panel data over time.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'              Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'             Not required if data has panel attributes.
#' @param type A character string specifying how to define entity presence: "observed", "balanced", or "complete". Default = "balanced".
#' @param format A character string specifying the output format: "wide" or "long". Default = "wide".
#' @param detailed A logical flag indicating whether to return detailed patterns. Default = TRUE.
#' @param digits An integer specifying the number of decimal places for rounding share and cumulative proportion columns.
#' Default = 3.
#'
#' @return A data.frame with participation patterns.
#'
#' @details
#' \strong{Type} parameter definitions:
#' \describe{
#'   \item{\code{"observed"}}{Entity is present if it has a row in the data (even with only panel ID variables)}
#'   \item{\code{"balanced"}}{Entity is present if it has at least one non-NA substantive variable (default)}
#'   \item{\code{"complete"}}{Entity is present only if it has no NA values in all substantive variables}
#' }
#'
#' The output format depends on the `format` and `detailed` parameters:
#'
#' \strong{When `format = "wide"` and `detailed = TRUE` (default):}
#' \describe{
#'   \item{\code{pattern}}{Pattern identifier (1, 2, 3, ...)}
#'   \item{\code{[time_period]}}{Columns for each time period showing participation
#'     (1 = present, 0 = missing). Column names match the time variable values.}
#'   \item{\code{count}}{Number of entities with this pattern}
#'   \item{\code{share}}{Proportion of entities with this pattern (0 to 1)}
#'   \item{\code{cumulative}}{Cumulative proportion of entities}
#' }
#'
#' \strong{When `format = "long"` and `detailed = TRUE`:}
#' \describe{
#'   \item{\code{pattern}}{Pattern identifier (1, 2, 3, ...)}
#'   \item{\code{[time]}}{The time period variable (named according to the `time` argument)}
#'   \item{\code{participation}}{0/1 values indicating absence/presence in the period}
#'   \item{\code{count}}{Number of entities with this pattern}
#'   \item{\code{share}}{Proportion of entities with this pattern}
#'   \item{\code{cumulative}}{Cumulative proportion of entities}
#' }
#'
#' \strong{When `detailed = FALSE`:}
#' Returns only the Pattern and time period columns (without count, share, or cumulative).
#'
#' Patterns are sorted by frequency (most common first).
#'
#' The data.frame has additional attributes:
#' \describe{
#'   \item{\code{panel_group}}{The grouping variable name}
#'   \item{\code{panel_time}}{The time variable name}
#'   \item{\code{panel_type}}{Presence type ("observed", "balanced", or "complete")}
#'   \item{\code{panel_format}}{Output format ("wide" or "long")}
#'   \item{\code{panel_detailed}}{Logical indicating detailed output}
#'   \item{\code{panel_digits}}{Number of decimal places used for rounding}
#'   \item{\code{panel_n_entities}}{Total number of unique entities/groups}
#'   \item{\code{panel_n_periods}}{Total number of unique time periods}
#'   \item{\code{panel_n_patterns}}{Number of distinct participation patterns}
#' }
#'
#' @seealso
#' [plot_participation()], [explore_participation()], [describe_periods()], [describe_incomplete()], [describe_balance()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' describe_participation(production, group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' describe_participation(panel_data)
#'
#' # Use different presence types
#' describe_participation(production, group = "firm", time = "year", type = "observed")
#' describe_participation(production, group = "firm", time = "year", type = "complete")
#'
#' # Simplified version
#' describe_participation(production, group = "firm", time = "year", detailed = FALSE)
#'
#' # Simplified version in long format
#' describe_participation(production, group = "firm", time = "year", detailed = FALSE, format = "long")
#'
#' # With custom rounding
#' describe_participation(production, group = "firm", time = "year", digits = 4)
#'
#' @export
describe_participation <- function(
  data,
  group = NULL,
  time = NULL,
  type = "balanced",
  format = "wide",
  detailed = TRUE,
  digits = 3
) {
  # Check if data has panel attributes
  has_panel_attrs <- !is.null(attr(data, "panel_group")) &&
    !is.null(attr(data, "panel_time"))

  if (has_panel_attrs) {
    # Extract group and time from attributes
    group <- attr(data, "panel_group")
    time <- attr(data, "panel_time")
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

  # Common validation for both cases
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string, not ", class(time)[1])
  }

  if (!is.character(type) || length(type) != 1) {
    stop("'type' must be a single character string, not ", class(type)[1])
  }

  if (!type %in% c("balanced", "observed", "complete")) {
    stop('type must be one of: "balanced", "observed", "complete"')
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

  if (!is.numeric(digits) || length(digits) != 1 || digits < 0) {
    stop(
      "'digits' must be a single non-negative integer, not ",
      class(digits)[1]
    )
  }

  digits <- as.integer(digits)

  # Convert data if needed
  data <- .check_and_convert_data_robust(data, arg_name = "data")

  # Identify data columns (excluding group and time)
  data_cols <- setdiff(names(data), c(group, time))

  if (length(data_cols) == 0) {
    stop("no data columns found (excluding group and time variables)")
  }

  # Get unique groups and periods for attributes
  unique_groups <- unique(as.character(data[[group]]))
  unique_periods <- unique(as.character(data[[time]]))

  # Sort time periods if they appear numeric
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", unique_periods))) {
    unique_periods <- as.character(sort(as.numeric(unique_periods)))
  } else {
    unique_periods <- sort(unique_periods)
  }

  # Filter data based on type
  if (type == "observed") {
    # Keep all rows (no filtering)
    data_filtered <- data
  } else if (type == "balanced") {
    # Keep rows where at least one data column is not NA
    if (nrow(data) > 0) {
      has_data <- apply(data[data_cols], 1, function(row) {
        !all(is.na(row))
      })
      data_filtered <- data[has_data, ]
    } else {
      data_filtered <- data
    }
  } else if (type == "complete") {
    # Keep rows where all data columns are not NA
    if (nrow(data) > 0) {
      complete_rows <- complete.cases(data[data_cols])
      data_filtered <- data[complete_rows, ]
    } else {
      data_filtered <- data
    }
  }

  # Convert group and time to character to handle different classes
  group_vec <- as.character(data_filtered[[group]])
  time_vec <- as.character(data_filtered[[time]])

  # For "complete" type, we need to handle differently to include all group-time combinations
  # even if some have NAs (they should be marked as 0)
  if (type == "complete") {
    # Get all unique groups and times from the complete data
    complete_combinations <- unique(data.frame(
      group = group_vec,
      time = time_vec
    ))

    # Get all possible groups and times from original data
    all_groups <- unique(as.character(data[[group]]))
    all_times <- sort(unique(as.character(data[[time]])))

    # Create a matrix of all possible combinations
    participation_binary <- matrix(
      0,
      nrow = length(all_groups),
      ncol = length(all_times),
      dimnames = list(all_groups, all_times)
    )

    # Mark 1 for complete cases
    for (i in seq_len(nrow(complete_combinations))) {
      row_group <- as.character(complete_combinations$group[i])
      row_time <- as.character(complete_combinations$time[i])
      if (
        row_group %in%
          rownames(participation_binary) &&
          row_time %in% colnames(participation_binary)
      ) {
        participation_binary[row_group, row_time] <- 1
      }
    }

    # Convert to data frame for pattern analysis
    participation_df <- as.data.frame(participation_binary)
    participation_df$group <- rownames(participation_df)
    time_cols <- all_times
  } else {
    # For "observed" and "balanced" types, use the original logic
    # Create unique combinations of group and time
    unique_combinations <- unique(data.frame(
      group = group_vec,
      time = time_vec
    ))

    # Get all unique time periods and sort them
    all_times <- sort(unique(time_vec))

    # Create participation matrix
    participation <- table(unique_combinations$group, unique_combinations$time)

    # Convert to binary matrix (1 = present, 0 = missing)
    participation_binary <- ifelse(participation > 0, 1, 0)

    # Convert to data frame for pattern analysis
    participation_df <- as.data.frame(participation_binary)
    participation_df$group <- rownames(participation_df)

    # Ensure all time periods are present as columns
    for (t in all_times) {
      if (!t %in% names(participation_df)) {
        participation_df[[t]] <- 0
      }
    }

    # Reorder columns to have time periods in order
    time_cols <- as.character(sort(all_times))
    participation_df <- participation_df[c("group", time_cols)]
  }

  # Count patterns
  pattern_cols <- setdiff(names(participation_df), "group")
  pattern_strings <- apply(participation_df[pattern_cols], 1, function(x) {
    paste(x, collapse = "")
  })

  pattern_counts <- table(pattern_strings)

  # Create result data frame
  result <- data.frame(
    pattern = seq_along(pattern_counts),
    stringsAsFactors = FALSE
  )

  # Add time period columns
  for (i in seq_along(time_cols)) {
    result[[time_cols[i]]] <- as.numeric(substr(names(pattern_counts), i, i))
  }

  # Add count and share columns
  result$count <- as.numeric(pattern_counts)
  result$share <- round(result$count / sum(result$count), digits)

  # Sort by count (descending) FIRST, then calculate cumulative sum
  result <- result[order(-result$count), ]

  # Now calculate cumulative sum on the sorted data
  result$cumulative <- round(cumsum(result$share), digits)

  # Reset pattern numbers to follow the new sorted order
  result$pattern <- seq_len(nrow(result))
  rownames(result) <- NULL

  # Convert to long format if requested
  if (format == "long") {
    # Reshape from wide to long format
    long_result <- data.frame()

    for (i in seq_len(nrow(result))) {
      pattern_row <- result[i, ]

      for (t in time_cols) {
        long_result <- rbind(
          long_result,
          data.frame(
            pattern = pattern_row$pattern,
            time = t,
            participation = as.integer(pattern_row[[t]]),
            count = pattern_row$count,
            share = pattern_row$share,
            cumulative = pattern_row$cumulative,
            stringsAsFactors = FALSE
          )
        )
      }
    }

    # Rename the "time" column to match the original time variable name
    names(long_result)[names(long_result) == "time"] <- time

    if (!detailed) {
      # Return simplified version with only pattern, time, and participation columns
      simplified_result <- long_result[c("pattern", time, "participation")]

      # Add standardized attributes to simplified result
      attr(simplified_result, "panel_group") <- group
      attr(simplified_result, "panel_time") <- time
      attr(simplified_result, "panel_type") <- type
      attr(simplified_result, "panel_format") <- format
      attr(simplified_result, "panel_detailed") <- detailed
      attr(simplified_result, "panel_digits") <- digits
      attr(simplified_result, "panel_n_entities") <- length(unique_groups)
      attr(simplified_result, "panel_n_periods") <- length(unique_periods)
      attr(simplified_result, "panel_n_patterns") <- nrow(result)

      return(simplified_result)
    }

    # Add standardized attributes to long result
    attr(long_result, "panel_group") <- group
    attr(long_result, "panel_time") <- time
    attr(long_result, "panel_type") <- type
    attr(long_result, "panel_format") <- format
    attr(long_result, "panel_detailed") <- detailed
    attr(long_result, "panel_digits") <- digits
    attr(long_result, "panel_n_entities") <- length(unique_groups)
    attr(long_result, "panel_n_periods") <- length(unique_periods)
    attr(long_result, "panel_n_patterns") <- nrow(result)

    return(long_result)
  }

  # Wide format handling (original behavior)
  if (!detailed) {
    # Return simplified version with only pattern and time period columns
    simplified_result <- result[c("pattern", time_cols)]

    # Add standardized attributes to simplified result
    attr(simplified_result, "panel_group") <- group
    attr(simplified_result, "panel_time") <- time
    attr(simplified_result, "panel_type") <- type
    attr(simplified_result, "panel_format") <- format
    attr(simplified_result, "panel_detailed") <- detailed
    attr(simplified_result, "panel_digits") <- digits
    attr(simplified_result, "panel_n_entities") <- length(unique_groups)
    attr(simplified_result, "panel_n_periods") <- length(unique_periods)
    attr(simplified_result, "panel_n_patterns") <- nrow(result)

    return(simplified_result)
  }

  # Add standardized attributes to result
  attr(result, "panel_group") <- group
  attr(result, "panel_time") <- time
  attr(result, "panel_type") <- type
  attr(result, "panel_format") <- format
  attr(result, "panel_detailed") <- detailed
  attr(result, "panel_digits") <- digits
  attr(result, "panel_n_entities") <- length(unique_groups)
  attr(result, "panel_n_periods") <- length(unique_periods)
  attr(result, "panel_n_patterns") <- nrow(result)

  return(result)
}
