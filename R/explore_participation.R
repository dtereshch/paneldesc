#' Panel Data Participation Analysis
#'
#' This function provides comprehensive summary statistics and pattern analysis of panel data structure.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#' @param time A character string specifying the name of the time variable.
#' @param detailed A logical flag indicating whether to show detailed pattern information. Default = TRUE.
#' @param max_patterns An integer specifying the maximum number of patterns to display in detail. Default = 10.
#'
#' @return Invisible list containing detailed participation pattern statistics.
#'
#' @references
#' For Stata users: This corresponds to the `xtdes` command.
#'
#' @seealso
#' [decompose_variation()], [describe_transition()], [describe_participation()], [plot_participation()]
#'
#' @examples
#' # Load the production dataset
#' data(production)
#'
#' # Analyze participation patterns
#' explore_participation(production, group = "firm", time = "year")
#'
#' # Show only summary without detailed patterns
#' explore_participation(production, group = "firm", time = "year", detailed = FALSE)
#'
#' # Increase maximum patterns to display
#' explore_participation(production, group = "firm", time = "year", max_patterns = 15)
#'
#' @export
explore_participation <- function(
  data,
  group = NULL,
  time = NULL,
  detailed = TRUE,
  max_patterns = 10
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop(
      "explore_participation: 'data' must be a data.frame, not ",
      class(data)[1]
    )
  }

  if (!is.character(group) || length(group) != 1) {
    stop(
      "explore_participation: 'group' must be a single character string, not ",
      class(group)[1]
    )
  }

  if (!is.character(time) || length(time) != 1) {
    stop(
      "explore_participation: 'time' must be a single character string, not ",
      class(time)[1]
    )
  }

  if (!group %in% names(data)) {
    stop('explore_participation: variable "', group, '" not found in data')
  }

  if (!time %in% names(data)) {
    stop('explore_participation: variable "', time, '" not found in data')
  }

  if (!is.logical(detailed) || length(detailed) != 1) {
    stop(
      "explore_participation: 'detailed' must be a single logical value, not ",
      class(detailed)[1]
    )
  }

  if (
    !is.numeric(max_patterns) || length(max_patterns) != 1 || max_patterns < 1
  ) {
    stop(
      "explore_participation: 'max_patterns' must be a single positive integer, not ",
      class(max_patterns)[1]
    )
  }

  data_df <- .check_and_convert_data_robust(data, arg_name = "data")

  # Regular data frame - require group and time arguments as character strings
  if (is.null(group) || is.null(time)) {
    stop(
      "explore_participation: for regular data frames, both 'group' and 'time' arguments are required as character strings"
    )
  }

  if (!is.character(group) || !is.character(time)) {
    stop(
      "explore_participation: for regular data frames, 'group' and 'time' arguments must be character strings"
    )
  }

  if (length(group) != 1 || length(time) != 1) {
    stop(
      "explore_participation: 'group' and 'time' must be single character strings"
    )
  }

  group_name <- group
  time_name <- time

  # Extract variables
  group_var <- data_df[[group_name]]
  time_var <- data_df[[time_name]]

  if (is.null(group_var)) {
    stop(
      "explore_participation: variable '",
      group_name,
      "' not found in data. Available variables: ",
      paste(names(data_df), collapse = ", ")
    )
  }
  if (is.null(time_var)) {
    stop(
      "explore_participation: variable '",
      time_name,
      "' not found in data. Available variables: ",
      paste(names(data_df), collapse = ", ")
    )
  }

  # Filter out rows with NAs in all substantive variables (excluding group and time)
  substantive_vars <- setdiff(names(data_df), c(group_name, time_name))

  if (length(substantive_vars) == 0) {
    stop(
      "explore_participation: no substantive variables found (besides group and time variables)"
    )
  }

  # Identify rows that have at least one non-NA value in substantive variables
  has_data <- apply(data_df[substantive_vars], 1, function(x) any(!is.na(x)))
  filtered_data <- data_df[has_data, ]

  # Update variables after filtering
  group_var <- filtered_data[[group_name]]
  time_var <- filtered_data[[time_name]]

  # Convert to character to handle different classes uniformly
  group_var <- as.character(group_var)
  time_var <- as.character(time_var)

  # Create cross-tabulation
  unique_groups <- unique(group_var)
  unique_times <- unique(time_var)

  # Create presence matrix (1 = present, 0 = missing)
  presence_matrix <- matrix(
    0,
    nrow = length(unique_groups),
    ncol = length(unique_times),
    dimnames = list(unique_groups, unique_times)
  )

  # Fill matrix with 1 for present observations
  for (i in seq_along(group_var)) {
    row_idx <- which(unique_groups == group_var[i])
    col_idx <- which(unique_times == time_var[i])
    presence_matrix[row_idx, col_idx] <- 1
  }

  # Group entities by missing value patterns
  pattern_strings <- apply(presence_matrix, 1, function(x) {
    paste(x, collapse = "")
  })
  pattern_groups <- split(rownames(presence_matrix), pattern_strings)

  # Create pattern matrix with one row per pattern
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
  }

  rownames(pattern_matrix) <- paste0(
    "Pattern ",
    seq_len(length(pattern_groups))
  )
  colnames(pattern_matrix) <- colnames(presence_matrix)

  # Fill pattern matrix and count entities per pattern
  pattern_counts <- numeric(length(pattern_groups))
  pattern_pcts <- numeric(length(pattern_groups))
  total_entities <- length(unique_groups)

  for (i in seq_along(pattern_groups)) {
    if (length(pattern_groups) == 1) {
      # Already filled above for single pattern case
      pattern <- pattern_matrix[i, ]
    } else {
      pattern <- as.numeric(strsplit(names(pattern_groups)[i], "")[[1]])
      pattern_matrix[i, ] <- pattern
    }
    pattern_counts[i] <- length(pattern_groups[[i]])
    pattern_pcts[i] <- pattern_counts[i] / total_entities * 100
  }

  # Order patterns by frequency (most common first)
  pattern_order <- order(pattern_counts, decreasing = TRUE)
  pattern_matrix <- pattern_matrix[pattern_order, , drop = FALSE]
  pattern_counts <- pattern_counts[pattern_order]
  pattern_pcts <- pattern_pcts[pattern_order]
  pattern_groups <- pattern_groups[pattern_order]

  # Calculate summary statistics
  stats <- list(
    n_entities = total_entities,
    n_periods = ncol(presence_matrix),
    total_obs = sum(presence_matrix),
    balanced_obs = nrow(presence_matrix) * ncol(presence_matrix),
    missing_obs = nrow(presence_matrix) *
      ncol(presence_matrix) -
      sum(presence_matrix),
    pct_missing = (1 -
      sum(presence_matrix) / (nrow(presence_matrix) * ncol(presence_matrix))) *
      100,
    entities_with_gaps = sum(rowSums(presence_matrix) < ncol(presence_matrix)),
    pct_entities_with_gaps = (sum(
      rowSums(presence_matrix) < ncol(presence_matrix)
    ) /
      nrow(presence_matrix)) *
      100,
    n_patterns = length(pattern_groups),
    filtered_obs = nrow(data_df) - nrow(filtered_data)
  )

  # Order time periods
  if (all(grepl("^-?\\d+\\.?\\d*$", unique_times))) {
    col_order <- order(as.numeric(unique_times))
  } else {
    col_order <- order(unique_times)
  }

  ordered_matrix <- pattern_matrix[, col_order, drop = FALSE]

  # Print summary statistics to console
  cat("=== Panel Data Participation Patterns ===\n\n")

  cat("Basic Statistics:\n")
  cat(sprintf("  Number of entities: %d\n", stats$n_entities))
  cat(sprintf("  Number of time periods: %d\n", stats$n_periods))
  cat(sprintf("  Total observations: %d\n", stats$total_obs))
  cat(sprintf("  Potential observations (balanced): %d\n", stats$balanced_obs))
  cat(sprintf(
    "  Missing observations: %d (%.1f%%)\n",
    stats$missing_obs,
    stats$pct_missing
  ))
  cat(sprintf(
    "  Entities with gaps: %d (%.1f%%)\n",
    stats$entities_with_gaps,
    stats$pct_entities_with_gaps
  ))
  cat(sprintf("  Number of participation patterns: %d\n", stats$n_patterns))
  if (stats$filtered_obs > 0) {
    cat(sprintf("  Rows filtered (all NAs): %d\n", stats$filtered_obs))
  }
  cat("\n")

  # Print time period coverage FIRST (as requested)
  period_coverage <- colSums(presence_matrix)
  period_coverage_pct <- period_coverage / stats$n_entities * 100

  cat("Time Period Coverage:\n")
  for (j in seq_along(period_coverage)) {
    cat(sprintf(
      "  %s: %d entities (%.1f%%)\n",
      colnames(ordered_matrix)[j],
      period_coverage[j],
      period_coverage_pct[j]
    ))
  }
  cat("\n")

  # Print pattern coverage statistics with aligned separators
  cat("Pattern Coverage:\n")
  cumulative_pct <- 0
  n_coverage_lines <- min(length(pattern_counts), 5)

  # Calculate maximum width for alignment
  max_width <- nchar(as.character(n_coverage_lines))

  for (i in seq_len(n_coverage_lines)) {
    cumulative_pct <- cumulative_pct + pattern_pcts[i]
    cat(sprintf(
      "  Top %*d patterns cover: %.1f%% of entities\n",
      max_width,
      i,
      cumulative_pct
    ))
  }
  cat("\n")

  if (detailed) {
    # Print detailed pattern information with aligned separators
    cat("Detailed Pattern Information:\n")
    n_to_display <- min(length(pattern_groups), max_patterns)

    # Calculate maximum widths for alignment
    max_pattern_width <- nchar(as.character(n_to_display))
    max_count_width <- max(nchar(as.character(pattern_counts[1:n_to_display])))
    max_pct_width <- 5 # "XX.X%" format

    for (i in seq_len(n_to_display)) {
      pattern <- ordered_matrix[i, ]
      pattern_visual <- ifelse(pattern == 1, "1", "0")

      # Format the pattern line with aligned separators
      pattern_label <- sprintf("Pattern %*d", max_pattern_width, i)
      count_label <- sprintf("n=%*d", max_count_width, pattern_counts[i])
      pct_label <- sprintf("%*.1f%%", max_pct_width - 1, pattern_pcts[i])

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
  }

  # Return invisible results with detailed pattern information
  invisible(list(
    presence_matrix = presence_matrix,
    pattern_matrix = ordered_matrix,
    pattern_groups = pattern_groups,
    pattern_stats = data.frame(
      pattern_id = seq_len(length(pattern_counts)),
      pattern_string = names(pattern_groups),
      n_entities = pattern_counts,
      percent_entities = pattern_pcts,
      entities = I(pattern_groups)
    ),
    statistics = stats,
    period_coverage = data.frame(
      time_period = colnames(ordered_matrix),
      n_entities = period_coverage,
      percent_coverage = period_coverage_pct
    ),
    group_var = group_name,
    time_var = time_name,
    filtered_data = filtered_data
  ))
}
