#' Panel Data Participation Patterns
#'
#' This function describes participation patterns of entities in panel data over time.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#' @param time A character string specifying the name of the time variable.
#' @param detailed A logical flag indicating whether to return detailed patterns. Default = TRUE.
#' @param format A character string specifying the output format: "wide" (default) or "long".
#'
#' @return A data.frame with participation patterns. For `format = "wide"`:
#' \itemize{
#'   \item Pattern: Pattern identifier (1, 2, 3, ...)
#'   \item Columns for each time period showing participation (1 = present, 0 = missing)
#'   \item Count: Number of entities with this pattern
#'   \item Share: Proportion of entities with this pattern
#'   \item Cumul.: Cumulative proportion of entities
#' }
#' For `format = "long"`:
#' \itemize{
#'   \item Pattern: Pattern identifier (1, 2, 3, ...)
#'   \item [time]: The time period variable (named according to the `time` argument)
#'   \item Participation: 0/1 values indicating absence/presence in the period
#'   \item Count: Number of entities with this pattern
#'   \item Share: Proportion of entities with this pattern
#'   \item Cumul.: Cumulative proportion of entities
#' }
#'
#' @seealso
#' [plot_participation()], [explore_participation()], [find_incomplete()], [explore_incomplete()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' describe_participation(production, group = "firm", time = "year")
#'
#' # Simplified version
#' describe_participation(production, group = "firm", time = "year", detailed = FALSE)
#'
#' # Simplified version in long format
#' describe_participation(production, group = "firm", time = "year", detailed = FALSE, format = "long")
#'
#' @export
describe_participation <- function(
  data,
  group,
  time,
  detailed = TRUE,
  format = "wide"
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
  }

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

  data <- .check_and_convert_data_robust(data, arg_name = "data")

  # Identify data columns (excluding group and time)
  data_cols <- setdiff(names(data), c(group, time))

  if (length(data_cols) == 0) {
    stop("no data columns found (excluding group and time variables)")
  }

  # Filter data: remove rows where ALL data columns are NA
  # This preserves the group-time structure while removing completely empty observations
  data_filtered <- data
  if (nrow(data_filtered) > 0) {
    # Create a logical vector indicating rows where at least one data column is not NA
    has_data <- apply(data_filtered[data_cols], 1, function(row) {
      !all(is.na(row))
    })
    data_filtered <- data_filtered[has_data, ]
  }

  # Convert group and time to character to handle different classes
  group_vec <- as.character(data_filtered[[group]])
  time_vec <- as.character(data_filtered[[time]])

  # Create unique combinations of group and time
  unique_combinations <- unique(data.frame(group = group_vec, time = time_vec))

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

  # Count patterns
  pattern_cols <- setdiff(names(participation_df), "group")
  pattern_strings <- apply(participation_df[pattern_cols], 1, function(x) {
    paste(x, collapse = "")
  })

  pattern_counts <- table(pattern_strings)

  # Create result data frame
  result <- data.frame(
    Pattern = seq_along(pattern_counts),
    stringsAsFactors = FALSE
  )

  # Add time period columns
  for (i in seq_along(time_cols)) {
    result[[time_cols[i]]] <- as.numeric(substr(names(pattern_counts), i, i))
  }

  # Add count and share columns
  result$Count <- as.numeric(pattern_counts)
  result$Share <- round(result$Count / sum(result$Count), 2)

  # FIX: Sort by count (descending) FIRST, then calculate cumulative sum
  result <- result[order(-result$Count), ]

  # Now calculate cumulative sum on the sorted data
  result$Cumul. <- round(cumsum(result$Share), 2)

  # Reset pattern numbers to follow the new sorted order
  result$Pattern <- seq_len(nrow(result))
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
            Pattern = pattern_row$Pattern,
            Time = t,
            Participation = as.integer(pattern_row[[t]]),
            Count = pattern_row$Count,
            Share = pattern_row$Share,
            Cumul. = pattern_row$Cumul.,
            stringsAsFactors = FALSE
          )
        )
      }
    }

    # Rename the "Time" column to match the original time variable name
    names(long_result)[names(long_result) == "Time"] <- time

    if (!detailed) {
      # Return simplified version with only Pattern, time, and Participation columns
      simplified_result <- long_result[c("Pattern", time, "Participation")]
      return(simplified_result)
    }

    return(long_result)
  }

  # Wide format handling (original behavior)
  if (!detailed) {
    # Return simplified version with only Pattern and time period columns
    simplified_result <- result[c("Pattern", time_cols)]
    return(simplified_result)
  }

  return(result)
}
