#' Entities Presence Patterns Visualization
#'
#' This function creates a heatmap visualization of entities presence patterns in panel data over time.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#' @param presence A character string specifying how to define entity presence: "nominal", "observed", or "complete".
#'        Default = "observed".
#' @param max_patterns An integer specifying the maximum number of patterns to display.
#'        Default = 10.
#' @param colors A character vector of two colors for present and missing observations.
#'        Default = c("#0072B2", "#D55E00").
#'
#' @return Invisibly returns a list with summary statistics. Creates a plot showing presence patterns.
#'
#' @details
#' \strong{Presence} parameter definitions:
#' \describe{
#'   \item{\code{"nominal"}}{Entity is present if it has a row in the data (even with only panel ID variables)}
#'   \item{\code{"observed"}}{Entity is present if it has at least one non-NA substantive variable (default)}
#'   \item{\code{"complete"}}{Entity is present only if it has no NA values in all substantive variables}
#' }
#'
#' The heatmap shows presence patterns where:
#' \itemize{
#'   \item \strong{Present}: Entity is present in the time period (based on the specified presence type)
#'   \item \strong{Missing}: Entity is absent in the time period
#' }
#'
#' Patterns are sorted by frequency (most common first) and the most common pattern appears at the top of the plot.
#'
#' The returned list contains the following components:
#' \describe{
#'   \item{\code{summary}}{List with summary statistics including:
#'     \itemize{
#'       \item \code{n_patterns_displayed}: Number of patterns displayed in the plot
#'       \item \code{total_patterns}: Total number of patterns found
#'       \item \code{presence}: Presence type used for analysis
#'       \item \code{max_patterns}: Maximum number of patterns to display
#'     }
#'   }
#'   \item{\code{patterns}}{List with pattern information including:
#'     \itemize{
#'       \item \code{pattern_matrix}: Matrix showing presence patterns
#'       \item \code{pattern_matrix_reversed}: Matrix with reversed values (for plotting)
#'       \item \code{pattern_labels}: Labels for each pattern
#'       \item \code{counts}: Number of entities in each pattern
#'     }
#'   }
#'   \item{\code{metadata}}{List with analysis parameters including:
#'     \itemize{
#'       \item \code{group_var}: The group variable name
#'       \item \code{time_var}: The time variable name
#'       \item \code{presence}: The presence type used for analysis
#'       \item \code{max_patterns}: Maximum number of patterns to display
#'       \item \code{colors}: Colors used for plotting
#'     }
#'   }
#' }
#'
#' @seealso
#' [describe_patterns()], [explore_presence()], [plot_heterogeneity()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage with top 10 patterns shown
#' plot_patterns(production, group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' plot_patterns(panel_data)
#'
#' # Use different presence types
#' plot_patterns(production, group = "firm", time = "year", presence = "nominal")
#' plot_patterns(production, group = "firm", time = "year", presence = "complete")
#'
#' # Show only top 5 patterns
#' plot_patterns(production, group = "firm", time = "year", max_patterns = 5)
#'
#' # Show all patterns
#' plot_patterns(production, group = "firm", time = "year", max_patterns = 999999)
#'
#' # Custom colors
#' plot_patterns(production, group = "firm", time = "year", colors = c("black", "white"))
#'
#' @export
plot_patterns <- function(
  data,
  group = NULL,
  time = NULL,
  presence = "observed",
  max_patterns = 10,
  colors = c("#0072B2", "#D55E00")
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

  # Common validation
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

  # Identify data columns (excluding group and time)
  data_cols <- setdiff(names(data), c(group, time))

  if (length(data_cols) == 0) {
    stop("no data columns found (excluding group and time variables)")
  }

  # Get all entities and time periods
  all_groups <- unique(as.character(data[[group]]))
  all_times <- unique(as.character(data[[time]]))

  # Sort time periods if they appear numeric
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", all_times))) {
    all_times <- as.character(sort(as.numeric(all_times)))
  } else {
    all_times <- sort(all_times)
  }

  time_cols <- all_times

  # Create a matrix of all possible combinations (initialize with 0)
  presence_binary <- matrix(
    0,
    nrow = length(all_groups),
    ncol = length(all_times),
    dimnames = list(all_groups, all_times)
  )

  # Convert to character for consistent handling
  group_vec <- as.character(data[[group]])
  time_vec <- as.character(data[[time]])

  # Fill the binary matrix based on presence
  if (presence == "nominal") {
    # For nominal type, mark 1 for all rows
    for (i in seq_along(group_vec)) {
      row_group <- as.character(group_vec[i])
      row_time <- as.character(time_vec[i])
      presence_binary[row_group, row_time] <- 1
    }
  } else if (presence == "observed") {
    # For observed type, mark 1 for rows with at least one non-NA
    has_at_least_one_non_na <- apply(
      data[data_cols],
      1,
      function(row) {
        !all(is.na(row))
      }
    )

    for (i in seq_along(group_vec)) {
      if (has_at_least_one_non_na[i]) {
        row_group <- as.character(group_vec[i])
        row_time <- as.character(time_vec[i])
        presence_binary[row_group, row_time] <- 1
      }
    }
  } else if (presence == "complete") {
    # For complete type, mark 1 for complete rows only
    complete_rows <- complete.cases(data[data_cols])

    for (i in seq_along(group_vec)) {
      if (complete_rows[i]) {
        row_group <- as.character(group_vec[i])
        row_time <- as.character(time_vec[i])
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

  # Count patterns and create pattern matrix
  pattern_cols <- setdiff(names(presence_df), "group")
  pattern_strings <- apply(presence_df[pattern_cols], 1, function(x) {
    paste(x, collapse = "")
  })

  pattern_counts <- table(pattern_strings)

  # Handle single pattern case
  n_patterns <- length(pattern_counts)

  if (n_patterns == 0) {
    stop("No presence patterns found in the data")
  }

  # Create pattern matrix for heatmap
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

  # Apply max_patterns limit
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

  # Reduced top margin since we're not showing a title
  par(mar = c(3, 1, 3, 8) + 0.1)

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

  # Create unified return object
  result <- list(
    summary = list(
      n_patterns_displayed = n_patterns_to_display,
      total_patterns = length(pattern_counts),
      presence = presence,
      max_patterns = max_patterns
    ),
    patterns = list(
      pattern_matrix = pattern_matrix,
      pattern_matrix_reversed = pattern_matrix_rev,
      pattern_labels = y_labels,
      counts = counts,
      time_periods = time_cols,
      presence_matrix = presence_binary
    ),
    metadata = list(
      group_var = group,
      time_var = time,
      presence = presence,
      max_patterns = max_patterns,
      colors = colors
    )
  )

  # Return the pattern matrix invisibly for further use
  invisible(result)
}
