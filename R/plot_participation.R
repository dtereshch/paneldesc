#' Participation Patterns Visualization
#'
#' This function creates a heatmap visualization of participation patterns
#' of entities in panel data over time.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#' @param time A character string specifying the name of the time variable.
#' @param max_patterns An integer specifying the maximum number of patterns to display.
#'        Default = 10.
#' @param colors A character vector of two colors for present and missing observations.
#'        Default = c("#0072B2", "#D55E00").
#'
#' @return Invisibly returns a list with summary statistics. Creates a plot showing participation patterns.
#'
#' @seealso
#' [describe_participation()], [explore_participation()], [plot_heterogeneity()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage with top 10 patterns shown
#' plot_participation(production, group = "firm", time = "year")
#'
#' # Show only top 5 patterns
#' plot_participation(production, group = "firm", time = "year", max_patterns = 5)
#'
#' # Show all patterns
#' plot_participation(production, group = "firm", time = "year", max_patterns = 999999)
#'
#' # Custom colors
#' plot_participation(production, group = "firm", time = "year", colors = c("black", "white"))
#'
#' @export
plot_participation <- function(
  data,
  group,
  time,
  max_patterns = 10,
  colors = c("#0072B2", "#D55E00")
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

  if (
    !is.numeric(max_patterns) || length(max_patterns) != 1 || max_patterns < 1
  ) {
    stop(
      "'max_patterns' must be a single positive integer, not ",
      class(max_patterns)[1]
    )
  }

  if (!is.character(colors) || length(colors) != 2) {
    stop(
      "'colors' must be a character vector of length 2, not ",
      class(colors)[1]
    )
  }

  data <- .check_and_convert_data_robust(data, arg_name = "data")

  # Validate that group and time variables exist in data
  if (!group %in% names(data)) {
    stop("variable '", group, "' not found in data")
  }

  if (!time %in% names(data)) {
    stop("variable '", time, "' not found in data")
  }

  # Identify data columns (excluding group and time)
  data_cols <- setdiff(names(data), c(group, time))

  if (length(data_cols) == 0) {
    stop("no data columns found (excluding group and time variables)")
  }

  # Filter data: remove rows where ALL data columns are NA
  data_filtered <- data
  if (nrow(data_filtered) > 0) {
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

  # Count patterns and create pattern matrix
  pattern_cols <- setdiff(names(participation_df), "group")
  pattern_strings <- apply(participation_df[pattern_cols], 1, function(x) {
    paste(x, collapse = "")
  })

  pattern_counts <- table(pattern_strings)

  # Handle single pattern case
  n_patterns <- length(pattern_counts)

  if (n_patterns == 0) {
    stop("No participation patterns found in the data")
  }

  # Create pattern matrix for heatmap - FIX: Handle single row matrix properly
  if (n_patterns == 1) {
    # Single pattern case
    pattern_matrix <- matrix(
      as.numeric(strsplit(names(pattern_counts)[1], "")[[1]]),
      nrow = 1,
      ncol = length(time_cols)
    )
  } else {
    # Multiple patterns case
    pattern_matrix <- matrix(
      0,
      nrow = n_patterns,
      ncol = length(time_cols)
    )

    for (i in seq_along(pattern_counts)) {
      pattern_matrix[i, ] <- as.numeric(strsplit(names(pattern_counts)[i], "")[[
        1
      ]])
    }
  }

  colnames(pattern_matrix) <- time_cols

  # Sort by count (descending) and create labels
  counts <- as.numeric(pattern_counts)
  sorted_order <- order(-counts)
  pattern_matrix <- pattern_matrix[sorted_order, , drop = FALSE]
  counts <- counts[sorted_order]

  # Apply max_patterns limit - FIX: Handle single pattern case
  n_patterns_to_display <- min(nrow(pattern_matrix), max_patterns)

  if (n_patterns_to_display > 0) {
    pattern_matrix <- pattern_matrix[1:n_patterns_to_display, , drop = FALSE]
    counts <- counts[1:n_patterns_to_display]
  } else {
    stop("No patterns to display after applying max_patterns filter")
  }

  # Reverse the matrix to put most common pattern on top
  pattern_matrix <- pattern_matrix[
    rev(seq_len(nrow(pattern_matrix))),
    ,
    drop = FALSE
  ]
  counts <- counts[rev(seq_len(length(counts)))]

  # Create y-axis labels
  y_labels <- paste0("Pattern ", rev(seq_along(counts)), " (n = ", counts, ")")

  # Reset graphical parameters on exit
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Reduce bottom margin to eliminate space for x-axis label
  # Top margin for legend, right margin for y-labels, reduced bottom margin
  par(mar = c(3, 1, 4, 8) + 0.1)

  # Create a reversed version of the pattern matrix
  pattern_matrix_rev <- 1 - pattern_matrix # 1->0, 0->1

  # Create heatmap using image function
  image(
    x = seq_len(ncol(pattern_matrix_rev)),
    y = seq_len(nrow(pattern_matrix_rev)),
    z = t(pattern_matrix_rev),
    col = colors,
    xlab = "", # Empty x-axis label
    ylab = "",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i"
  )

  # Add x-axis with vertical labels
  axis(
    1,
    at = seq_len(ncol(pattern_matrix_rev)),
    labels = time_cols,
    tick = FALSE,
    las = 2 # Vertical text
  )

  # Add y-axis labels on the right
  axis(
    4,
    at = seq_len(nrow(pattern_matrix_rev)),
    labels = y_labels,
    las = 2,
    tick = FALSE
  )

  # Add grid lines
  abline(h = seq(0.5, nrow(pattern_matrix_rev) + 0.5, 1), col = "gray", lty = 3)
  abline(v = seq(0.5, ncol(pattern_matrix_rev) + 0.5, 1), col = "gray", lty = 3)

  # Add legend at the top
  legend(
    "top",
    legend = c("Present", "Missing"),
    fill = colors,
    bg = "white",
    horiz = TRUE,
    xpd = TRUE,
    bty = "n",
    inset = c(0, -0.1),
    cex = 0.9
  )

  # Return the pattern matrix invisibly for further use
  invisible(list(
    pattern_matrix = pattern_matrix,
    pattern_matrix_reversed = pattern_matrix_rev,
    time_periods = time_cols,
    pattern_labels = y_labels,
    counts = counts,
    max_patterns = max_patterns,
    n_patterns_displayed = n_patterns_to_display,
    total_patterns = length(pattern_counts)
  ))
}
