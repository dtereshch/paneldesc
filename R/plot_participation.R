#' Visualize panel data participation patterns as a heatmap
#'
#' This function creates a heatmap visualization of participation patterns
#' of entities in panel data over time, similar to Stata's xtdes command.
#'
#' @param data A data frame with panel data structure
#' @param group Character string specifying the entity/group variable name
#' @param time Character string specifying the time variable name
#' @param colors Vector of two colors for present and missing observations
#' (default: c("#0072B2", "#D55E00") - colorblind-friendly)
#' @param xlab X-axis label (default: "Time Period")
#'
#' @return A heatmap plot showing participation patterns
#'
#' @seealso [describe_participation()], [explore_participation()], [find_incomplete()], [explore_incomplete()]
#'
#' @examples
#' # Load the production dataset
#' data(production)
#'
#' # Basic usage with all patterns shown
#' plot_participation(production, group = "firm", time = "year")
#'
#' # Custom colors
#' plot_participation(production, group = "firm", time = "year",
#'                        colors = c("blue", "red"))
#'
#' @export
plot_participation <- function(
  data,
  group = NULL,
  time = NULL,
  colors = c("#0072B2", "#D55E00"),
  xlab = "Time Period"
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

  # Create pattern matrix for heatmap
  pattern_matrix <- matrix(
    0,
    nrow = length(pattern_counts),
    ncol = length(time_cols)
  )
  colnames(pattern_matrix) <- time_cols

  for (i in seq_along(pattern_counts)) {
    pattern_matrix[i, ] <- as.numeric(strsplit(names(pattern_counts)[i], "")[[
      1
    ]])
  }

  # Sort by count (descending) and create labels
  counts <- as.numeric(pattern_counts)
  sorted_order <- order(-counts)
  pattern_matrix <- pattern_matrix[sorted_order, ]
  counts <- counts[sorted_order]

  # Reverse the matrix to put most common pattern on top
  pattern_matrix <- pattern_matrix[rev(seq_len(nrow(pattern_matrix))), ]
  counts <- counts[rev(seq_len(length(counts)))]

  # Create y-axis labels
  y_labels <- paste0("Pattern ", rev(seq_along(counts)), " (n = ", counts, ")")

  # Reset graphical parameters on exit
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Set up plot margins to accommodate legend at top and y-labels on right
  par(mar = c(5, 1, 4, 8) + 0.1) # Top margin for legend, right margin for y-labels

  # Create heatmap using image function with reversed colors
  image(
    x = seq_len(ncol(pattern_matrix)),
    y = seq_len(nrow(pattern_matrix)),
    z = t(pattern_matrix),
    col = rev(colors), # Reverse colors to match the data correctly
    xlab = xlab,
    ylab = "",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i"
  )

  # Add axes without ticks
  axis(1, at = seq_len(ncol(pattern_matrix)), labels = time_cols, tick = FALSE)
  axis(
    4,
    at = seq_len(nrow(pattern_matrix)),
    labels = y_labels,
    las = 2,
    tick = FALSE
  )

  # Add grid lines
  abline(h = seq(0.5, nrow(pattern_matrix) + 0.5, 1), col = "gray", lty = 3)
  abline(v = seq(0.5, ncol(pattern_matrix) + 0.5, 1), col = "gray", lty = 3)

  # Add legend at the top
  legend(
    "top",
    legend = c("Present", "Missing"),
    fill = colors,
    bg = "white",
    horiz = TRUE,
    xpd = TRUE,
    bty = "n",
    inset = c(0, -0.1), # Adjust position above plot
    cex = 0.9
  )

  # Return the pattern matrix invisibly for further use
  invisible(list(
    pattern_matrix = pattern_matrix,
    time_periods = time_cols,
    pattern_labels = y_labels,
    counts = counts
  ))
}
