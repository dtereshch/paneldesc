#' Entities Presence Patterns Description
#'
#' This function describes entities presence patterns in panel data over time.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#' @param presence A character string specifying how to define entity presence: "nominal", "observed", or "complete".
#'        Default = "observed".
#' @param format A character string specifying the output format: "wide" or "long". Default = "wide".
#' @param detailed A logical flag indicating whether to return detailed patterns. Default = TRUE.
#' @param digits An integer specifying the number of decimal places for rounding share column.
#'        Default = 3.
#'
#' @return A data.frame with presence patterns.
#'
#' @details
#' \strong{Presence} parameter definitions:
#' \describe{
#'   \item{\code{"nominal"}}{Entity is present if it has a row in the data (even with only panel ID variables)}
#'   \item{\code{"observed"}}{Entity is present if it has at least one non-NA substantive variable (default)}
#'   \item{\code{"complete"}}{Entity is present only if it has no NA values in all substantive variables}
#' }
#'
#' The output format depends on the `format` and `detailed` parameters:
#'
#' \strong{When `format = "wide"` and `detailed = TRUE` (default):}
#' \describe{
#'   \item{\code{rank}}{Pattern rank (1, 2, 3, ...) ordered by frequency}
#'   \item{\code{[time_period]}}{Columns for each time period showing presence
#'     (1 = present, 0 = missing). Column names match the time variable values.}
#'   \item{\code{count}}{Number of entities with this pattern}
#'   \item{\code{share}}{Proportion of entities with this pattern (0 to 1)}
#' }
#'
#' \strong{When `format = "long"` and `detailed = TRUE`:}
#' \describe{
#'   \item{\code{rank}}{Pattern rank (1, 2, 3, ...) ordered by frequency}
#'   \item{\code{[time]}}{The time period variable (named according to the `time` argument)}
#'   \item{\code{presence}}{0/1 values indicating absence/presence in the period}
#'   \item{\code{count}}{Number of entities with this pattern}
#'   \item{\code{share}}{Proportion of entities with this pattern}
#' }
#'
#' \strong{When `detailed = FALSE`:}
#' Returns only the rank and time period columns (without count or share).
#'
#' Patterns are sorted by frequency (most common first).
#'
#' The returned data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing additional information: `n_entities`, `n_periods`, `n_patterns`,
#'         `matrix`, `pattern_groups`.}
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
#' # Use different presence types
#' describe_patterns(production, group = "firm", time = "year", presence = "nominal")
#' describe_patterns(production, group = "firm", time = "year", presence = "complete")
#'
#' # Simplified version
#' describe_patterns(production, group = "firm", time = "year", detailed = FALSE)
#'
#' # Simplified version in long format
#' describe_patterns(production, group = "firm", time = "year", detailed = FALSE, format = "long")
#'
#' # With custom rounding
#' describe_patterns(production, group = "firm", time = "year", digits = 4)
#'
#' # Effectively no rounding (use large digit value)
#' describe_patterns(production, group = "firm", time = "year", digits = 999999)
#'
#' @export
describe_patterns <- function(
  data,
  group = NULL,
  time = NULL,
  presence = "observed",
  format = "wide",
  detailed = TRUE,
  digits = 3
) {
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

  # Common validation for both cases
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string, not ", class(time)[1])
  }

  if (!is.character(presence) || length(presence) != 1) {
    stop(
      "'presence' must be a single character string, not ",
      class(presence)[1]
    )
  }

  if (!presence %in% c("observed", "nominal", "complete")) {
    stop('presence must be one of: "observed", "nominal", "complete"')
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

  # Harmonized digits validation
  if (!is.numeric(digits) || length(digits) != 1) {
    stop("'digits' must be a single non-negative integer")
  }
  if (digits < 0 || digits != round(digits)) {
    stop("'digits' must be a non-negative integer")
  }
  digits <- as.integer(digits)

  # Helper function for rounding
  round_if_needed <- function(x, digits) {
    if (is.numeric(x) && !all(is.na(x))) {
      round(x, digits)
    } else {
      x
    }
  }

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

  # Filter data based on presence
  if (presence == "nominal") {
    # Keep all rows (no filtering)
    data_filtered <- data
  } else if (presence == "observed") {
    # Keep rows where at least one data column is not NA
    if (nrow(data) > 0) {
      has_data <- apply(data[data_cols], 1, function(row) {
        !all(is.na(row))
      })
      data_filtered <- data[has_data, ]
    } else {
      data_filtered <- data
    }
  } else if (presence == "complete") {
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

  # Get all entities and time periods
  all_groups <- unique(as.character(data[[group]]))
  all_times <- sort(unique_periods)

  # Create a matrix of all possible combinations (initialize with 0)
  presence_binary <- matrix(
    0,
    nrow = length(all_groups),
    ncol = length(all_times),
    dimnames = list(all_groups, all_times)
  )

  time_cols <- all_times

  # Fill the binary matrix based on presence
  if (presence == "nominal") {
    # For nominal type, mark 1 for all rows in filtered data
    for (i in seq_along(group_vec)) {
      row_group <- as.character(group_vec[i])
      row_time <- as.character(time_vec[i])
      presence_binary[row_group, row_time] <- 1
    }
  } else if (presence == "observed") {
    # For observed type, mark 1 for rows with at least one non-NA
    # The data_filtered already contains only rows with at least one non-NA
    for (i in seq_along(group_vec)) {
      row_group <- as.character(group_vec[i])
      row_time <- as.character(time_vec[i])
      presence_binary[row_group, row_time] <- 1
    }
  } else if (presence == "complete") {
    # For complete type, mark 1 for complete rows only
    # Need to check all rows in original data
    complete_rows <- complete.cases(data[data_cols])
    all_group_vec <- as.character(data[[group]])
    all_time_vec <- as.character(data[[time]])

    for (i in seq_along(all_group_vec)) {
      if (complete_rows[i]) {
        row_group <- as.character(all_group_vec[i])
        row_time <- as.character(all_time_vec[i])
        if (row_time %in% time_cols) {
          presence_binary[row_group, row_time] <- 1
        }
      }
    }
  }

  # Convert to data frame for pattern analysis
  presence_df <- as.data.frame(presence_binary)
  presence_df$group <- rownames(presence_df)
  presence_df <- presence_df[c("group", time_cols)]

  # Count patterns and create pattern groups
  pattern_cols <- setdiff(names(presence_df), "group")
  pattern_strings <- apply(presence_df[pattern_cols], 1, function(x) {
    paste(x, collapse = "")
  })

  pattern_counts <- table(pattern_strings)

  # Create pattern_groups list
  pattern_groups <- list()

  # Create pattern groups from the binary matrix
  for (entity in all_groups) {
    pattern_vec <- presence_binary[entity, ]
    pattern_string <- paste(pattern_vec, collapse = "")

    if (!pattern_string %in% names(pattern_groups)) {
      pattern_groups[[pattern_string]] <- character(0)
    }
    pattern_groups[[pattern_string]] <- c(
      pattern_groups[[pattern_string]],
      entity
    )
  }

  # Create result data frame
  result <- data.frame(
    rank = seq_along(pattern_counts),
    stringsAsFactors = FALSE
  )

  # Add time period columns
  for (i in seq_along(time_cols)) {
    result[[time_cols[i]]] <- as.numeric(substr(names(pattern_counts), i, i))
  }

  # Add count and share columns
  result$count <- as.numeric(pattern_counts)
  result$share <- round_if_needed(result$count / sum(result$count), digits)

  # Sort by count (descending) FIRST
  result <- result[order(-result$count), ]

  # Reset rank numbers to follow the new sorted order
  result$rank <- seq_len(nrow(result))
  rownames(result) <- NULL

  # Reorder pattern_groups to match the sorted patterns in result
  # First, get the pattern strings in the sorted order
  sorted_pattern_strings <- apply(result[time_cols], 1, function(x) {
    paste(x, collapse = "")
  })

  # Create a reordered pattern_groups list
  pattern_groups_sorted <- list()
  for (i in seq_along(sorted_pattern_strings)) {
    pattern_string <- sorted_pattern_strings[i]
    pattern_groups_sorted[[as.character(i)]] <- pattern_groups[[pattern_string]]
  }
  pattern_groups <- pattern_groups_sorted

  # Build base details list (will be extended depending on format)
  details_base <- list(
    n_entities = length(unique_groups),
    n_periods = length(unique_periods),
    n_patterns = nrow(result),
    matrix = presence_binary,
    pattern_groups = pattern_groups
  )

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
            rank = pattern_row$rank,
            time = t,
            presence = as.integer(pattern_row[[t]]),
            count = pattern_row$count,
            share = pattern_row$share,
            stringsAsFactors = FALSE
          )
        )
      }
    }

    # Rename the "time" column to match the original time variable name
    names(long_result)[names(long_result) == "time"] <- time

    if (!detailed) {
      # Return simplified version with only rank, time, and presence columns
      simplified_result <- long_result[c("rank", time, "presence")]

      # Build metadata
      call <- match.call()
      metadata <- list(
        function_name = as.character(call[[1]]),
        group = group,
        time = time,
        presence = presence,
        format = format,
        detailed = detailed,
        digits = digits
      )

      # Set attributes
      attr(simplified_result, "metadata") <- metadata
      attr(simplified_result, "details") <- details_base

      class(simplified_result) <- c("panel_description", "data.frame")
      return(simplified_result)
    }

    # Build metadata for long result
    call <- match.call()
    metadata <- list(
      function_name = as.character(call[[1]]),
      group = group,
      time = time,
      presence = presence,
      format = format,
      detailed = detailed,
      digits = digits
    )

    # Set attributes
    attr(long_result, "metadata") <- metadata
    attr(long_result, "details") <- details_base

    class(long_result) <- c("panel_description", "data.frame")
    return(long_result)
  }

  # Wide format handling
  if (!detailed) {
    # Return simplified version with only rank and time period columns
    simplified_result <- result[c("rank", time_cols)]

    # Build metadata
    call <- match.call()
    metadata <- list(
      function_name = as.character(call[[1]]),
      group = group,
      time = time,
      presence = presence,
      format = format,
      detailed = detailed,
      digits = digits
    )

    # Set attributes
    attr(simplified_result, "metadata") <- metadata
    attr(simplified_result, "details") <- details_base

    class(simplified_result) <- c("panel_description", "data.frame")
    return(simplified_result)
  }

  # Build metadata for full wide result
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time,
    presence = presence,
    format = format,
    detailed = detailed,
    digits = digits
  )

  # Set attributes
  attr(result, "metadata") <- metadata
  attr(result, "details") <- details_base

  # Set class
  class(result) <- c("panel_description", "data.frame")

  return(result)
}
