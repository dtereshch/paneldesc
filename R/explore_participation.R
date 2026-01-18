#' Detailed Panel Data Participation Patterns
#'
#' Provides detailed analysis of participation patterns in panel data.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#' @param time A character string specifying the name of the time variable.
#' @param max_patterns An integer specifying the maximum number of patterns to display in detail. Default = 10.
#'
#' @return A list containing detailed participation pattern statistics.
#' The list includes:
#'   \item{pattern_matrix}{Matrix showing presence patterns}
#'   \item{pattern_groups}{List of entities belonging to each pattern}
#'   \item{pattern_stats}{Data.frame with pattern statistics}
#'   \item{entities_by_pattern}{Vector with the number of entities in each pattern}
#' The result is returned invisibly, and will be printed automatically unless assigned to a variable.
#'
#' @examples
#' data(production)
#'
#' # Basic usage - will print automatically
#' explore_participation(production, group = "firm", time = "year")
#'
#' # Show only top 5 patterns - will print automatically
#' explore_participation(production, group = "firm", time = "year", max_patterns = 5)
#'
#' # Access the number of entities per pattern - won't print
#' result <- explore_participation(production, group = "firm", time = "year")
#' entities_per_pattern <- result$entities_by_pattern
#' print(entities_per_pattern)
#'
#' @export
explore_participation <- function(data, group, time, max_patterns = 10) {
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

  if (
    !is.numeric(max_patterns) || length(max_patterns) != 1 || max_patterns < 1
  ) {
    stop(
      "'max_patterns' must be a single positive integer, not ",
      class(max_patterns)[1]
    )
  }

  # Filter data for analysis
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  has_data <- apply(data[substantive_vars], 1, function(x) any(!is.na(x)))
  filtered_data <- data[has_data, ]

  # Extract variables
  group_var <- as.character(filtered_data[[group]])
  time_var <- as.character(filtered_data[[time]])

  # Get unique values
  unique_groups <- unique(group_var)
  unique_times <- unique(time_var)

  # Order time periods
  if (all(grepl("^-?\\d+\\.?\\d*$", unique_times))) {
    time_order <- order(as.numeric(unique_times))
  } else {
    time_order <- order(unique_times)
  }
  ordered_times <- unique_times[time_order]

  # Create presence matrix
  presence_matrix <- matrix(
    0,
    nrow = length(unique_groups),
    ncol = length(ordered_times),
    dimnames = list(unique_groups, ordered_times)
  )

  for (i in seq_along(group_var)) {
    row_idx <- which(unique_groups == group_var[i])
    col_idx <- which(ordered_times == time_var[i])
    if (length(col_idx) > 0) {
      presence_matrix[row_idx, col_idx] <- 1
    }
  }

  # Group entities by missing value patterns
  pattern_strings <- apply(presence_matrix, 1, function(x) {
    paste(x, collapse = "")
  })
  pattern_groups <- split(rownames(presence_matrix), pattern_strings)

  # Create pattern matrix
  if (length(pattern_groups) == 1) {
    pattern_matrix <- matrix(
      as.numeric(strsplit(names(pattern_groups)[1], "")[[1]]),
      nrow = 1,
      ncol = ncol(presence_matrix)
    )
  } else {
    pattern_matrix <- matrix(
      0,
      nrow = length(pattern_groups),
      ncol = ncol(presence_matrix)
    )
    for (i in seq_along(pattern_groups)) {
      pattern_matrix[i, ] <- as.numeric(strsplit(names(pattern_groups)[i], "")[[
        1
      ]])
    }
  }

  rownames(pattern_matrix) <- paste0("Pattern ", seq_len(nrow(pattern_matrix)))
  colnames(pattern_matrix) <- ordered_times

  # Count entities per pattern
  pattern_counts <- numeric(length(pattern_groups))
  for (i in seq_along(pattern_groups)) {
    pattern_counts[i] <- length(pattern_groups[[i]])
  }

  # Order patterns by frequency (most common first)
  pattern_order <- order(pattern_counts, decreasing = TRUE)
  pattern_matrix <- pattern_matrix[pattern_order, , drop = FALSE]
  pattern_counts <- pattern_counts[pattern_order]
  pattern_groups <- pattern_groups[pattern_order]

  # Calculate percentages
  total_entities <- length(unique_groups)
  pattern_pcts <- pattern_counts / total_entities * 100

  # Create vector with entities number for each pattern
  entities_by_pattern <- pattern_counts
  names(entities_by_pattern) <- rownames(pattern_matrix)

  # Check if we're at the top level (called directly, not assigned)
  # Compare the current call to the first call in the call stack
  called_from_top <- length(sys.calls()) == 1

  # Print if called directly (not assigned)
  if (called_from_top) {
    # Print formatted output
    n_to_display <- min(length(pattern_groups), max_patterns)
    cat(sprintf(
      "Information on Top %d Participation Patterns\n\n",
      n_to_display
    ))

    # Calculate maximum widths for alignment
    max_pattern_width <- nchar(as.character(n_to_display))
    max_count_width <- max(nchar(as.character(pattern_counts[1:n_to_display])))

    for (i in seq_len(n_to_display)) {
      pattern <- pattern_matrix[i, ]
      pattern_visual <- ifelse(pattern == 1, "1", "0")

      # Format the pattern line with aligned separators
      pattern_label <- sprintf("Pattern %*d", max_pattern_width, i)
      count_label <- sprintf("n=%*d", max_count_width, pattern_counts[i])
      pct_label <- sprintf("%5.1f%%", pattern_pcts[i])

      # Show entities for this pattern
      entities_in_pattern <- pattern_groups[[i]]
      if (length(entities_in_pattern) <= 5) {
        entities_label <- paste(entities_in_pattern, collapse = ", ")
      } else {
        entities_label <- paste(
          paste(entities_in_pattern[1:3], collapse = ", "),
          ", ... (",
          length(entities_in_pattern) - 3,
          " more)",
          sep = ""
        )
      }

      cat(sprintf(
        "%s (%s, %s): [%s], entities: %s%s\n",
        pattern_label,
        count_label,
        pct_label,
        paste(pattern_visual, collapse = ""),
        entities_label,
        ifelse(i == 1, " (Most Common)", "")
      ))
    }

    if (length(pattern_groups) > max_patterns) {
      cat(sprintf(
        "\n... and %d more patterns (use max_patterns = %d to see all)\n",
        length(pattern_groups) - max_patterns,
        length(pattern_groups)
      ))
    }
    cat("\n")

    # Create pattern stats data.frame
    pattern_stats <- data.frame(
      pattern_id = seq_len(length(pattern_counts)),
      pattern_string = names(pattern_groups),
      n_entities = pattern_counts,
      percent_entities = pattern_pcts,
      entities = I(pattern_groups),
      stringsAsFactors = FALSE
    )

    # Return visibly so it gets printed by the REPL
    return(list(
      pattern_matrix = pattern_matrix,
      pattern_groups = pattern_groups,
      pattern_stats = pattern_stats,
      entities_by_pattern = entities_by_pattern,
      presence_matrix = presence_matrix,
      group_var = group,
      time_var = time,
      filtered_data = filtered_data
    ))
  } else {
    # Create pattern stats data.frame
    pattern_stats <- data.frame(
      pattern_id = seq_len(length(pattern_counts)),
      pattern_string = names(pattern_groups),
      n_entities = pattern_counts,
      percent_entities = pattern_pcts,
      entities = I(pattern_groups),
      stringsAsFactors = FALSE
    )

    # Return invisibly if not at top level (likely assigned)
    invisible(list(
      pattern_matrix = pattern_matrix,
      pattern_groups = pattern_groups,
      pattern_stats = pattern_stats,
      entities_by_pattern = entities_by_pattern,
      presence_matrix = presence_matrix,
      group_var = group,
      time_var = time,
      filtered_data = filtered_data
    ))
  }
}
