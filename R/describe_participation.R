#' Describe panel data participation patterns
#'
#' This function replicates Stata's xtdes command, showing the participation patterns
#' of entities in panel data over time.
#'
#' @param data A data frame with panel data structure
#' @param group Character string specifying the entity/group variable name
#' @param time Character string specifying the time variable name
#' @param detailed Logical indicating whether to return detailed patterns (default: TRUE)
#'
#' @return A data frame with participation patterns containing:
#' \itemize{
#'   \item Pattern: Pattern identifier (1, 2, 3, ...)
#'   \item Columns for each time period showing participation (1 = present, 0 = missing)
#'   \item Count: Number of entities with this pattern
#'   \item Share: Proportion of entities with this pattern
#'   \item Cumul.: Cumulative proportion of entities
#' }
#'
#' @examples
#' # Load the production dataset
#' data(production)
#'
#' # Basic usage with all patterns shown
#' describe_participation(production, group = "firm", time = "year")
#'
#' # Show simplified version without time period columns
#' describe_participation(production, group = "firm", time = "year", detailed = FALSE)
#'
#' @export
describe_participation <- function(
  data,
  group = NULL,
  time = NULL,
  detailed = TRUE
) {
  # Input validation
  if (missing(data)) {
    stop("Argument 'data' is required")
  }

  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data frame")
  }

  # Check if group and time are specified for regular data frames
  if (is.null(group) || is.null(time)) {
    stop(
      "Arguments 'group' and 'time' must be specified for regular data frames"
    )
  }

  # Validate that group and time variables exist in data
  if (!group %in% names(data)) {
    stop("Group variable '", group, "' not found in data")
  }

  if (!time %in% names(data)) {
    stop("Time variable '", time, "' not found in data")
  }

  # Identify data columns (excluding group and time)
  data_cols <- setdiff(names(data), c(group, time))

  if (length(data_cols) == 0) {
    stop("No data columns found (excluding group and time variables)")
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

  # Add count, share and cumulative columns with rounding
  result$Count <- as.numeric(pattern_counts)
  result$Share <- round(result$Count / sum(result$Count), 2)
  result$Cumul. <- round(cumsum(result$Share), 2)

  # Sort by count (descending)
  result <- result[order(-result$Count), ]
  result$Pattern <- seq_len(nrow(result))
  rownames(result) <- NULL

  if (!detailed) {
    # Return simplified version without individual time period columns
    simplified_result <- data.frame(
      Pattern = result$Pattern,
      Count = result$Count,
      Share = result$Share,
      Cumul. = result$Cumul.
    )
    return(simplified_result)
  }

  return(result)
}
