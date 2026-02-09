#' Time Coverage Distribution Visualization
#'
#' This function creates a histogram showing the distribution of time periods
#' covered by each entity in panel data, based on the specified presence definition.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'              Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'             Not required if data has panel attributes.
#' @param type A character string specifying how to define entity presence: "balanced", "observed", or "complete".
#'        Default = "balanced".
#' @param color A character string specifying the fill color for the histogram.
#'        Default = "#0072B2".
#' @param detailed A logical flag indicating whether to display detailed summary statistics.
#'        Default = TRUE.
#'
#' @return Invisibly returns a list with summary statistics for the specified type.
#'         Creates a plot showing time coverage distribution.
#'
#' @details
#' \strong{Type} parameter definitions:
#' \describe{
#'   \item{\code{"observed"}}{Entity is present if it has a row in the data (even with only panel ID variables)}
#'   \item{\code{"balanced"}}{Entity is present if it has at least one non-NA substantive variable (default)}
#'   \item{\code{"complete"}}{Entity is present only if it has no NA values in all substantive variables}
#' }
#'
#' The x-axis shows the number of time periods covered by each entity, and the
#' y-axis shows the count/frequency of entities with that coverage.
#'
#' The returned list contains the following components:
#' \describe{
#'   \item{\code{summary}}{List with overall summary statistics including:
#'     \itemize{
#'       \item \code{n_entities}: Total number of unique entities
#'       \item \code{n_time_periods}: Total number of unique time periods
#'       \item \code{type}: Presence type used for analysis
#'       \item \code{group_var}: The group variable name
#'       \item \code{time_var}: The time variable name
#'     }
#'   }
#'   \item{\code{coverage}}{List with coverage statistics including:
#'     \itemize{
#'       \item \code{coverage_by_entity}: Named vector with number of time periods covered for each entity
#'       \item \code{summary_stats}: Summary statistics (min, Q1, median, mean, Q3, max)
#'       \item \code{histogram_data}: Data used for histogram plotting
#'     }
#'   }
#'   \item{\code{metadata}}{List with analysis parameters including:
#'     \itemize{
#'       \item \code{group_var}: The group variable name
#'       \item \code{time_var}: The time variable name
#'       \item \code{type}: The presence type used for analysis
#'       \item \code{color}: Color used for plotting
#'       \item \code{detailed}: Whether detailed statistics were shown
#'     }
#'   }
#' }
#'
#' @seealso
#' [plot_participation()], [explore_participation()], [plot_heterogeneity()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage with regular data.frame
#' plot_periods(production, group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' plot_periods(panel_data)
#'
#' # Use different presence types
#' plot_periods(production, group = "firm", time = "year", type = "observed")
#' plot_periods(production, group = "firm", time = "year", type = "complete")
#'
#' # Custom color
#' plot_periods(production, group = "firm", time = "year", color = "darkred")
#'
#' # Show plot without summary statistics
#' plot_periods(production, group = "firm", time = "year", detailed = FALSE)
#'
#' @export
plot_periods <- function(
  data,
  group = NULL,
  time = NULL,
  type = "balanced",
  color = "#0072B2",
  detailed = TRUE
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

  if (!is.character(type) || length(type) != 1) {
    stop("'type' must be a single character string, not ", class(type)[1])
  }

  if (!type %in% c("balanced", "observed", "complete")) {
    stop('type must be one of: "balanced", "observed", "complete"')
  }

  if (!is.character(color) || length(color) != 1) {
    stop(
      "'color' must be a single character string, not ",
      class(color)[1]
    )
  }

  if (!is.logical(detailed) || length(detailed) != 1) {
    stop(
      "'detailed' must be a single logical value, not ",
      class(detailed)[1]
    )
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  if (!time %in% names(data)) {
    stop('variable "', time, '" not found in data')
  }

  # Get substantive variables
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Helper function to calculate coverage for a given type
  calculate_coverage <- function(type) {
    if (type == "observed") {
      # Use all rows (no filtering)
      filtered_data <- data

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

      # Count all rows as presence
      for (i in seq_along(group_var)) {
        row_idx <- which(unique_groups == group_var[i])
        col_idx <- which(ordered_times == time_var[i])
        if (length(col_idx) > 0) {
          presence_matrix[row_idx, col_idx] <- 1
        }
      }
    } else if (type == "balanced") {
      # Keep rows with at least one non-NA substantive variable
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

      # Fill presence matrix
      for (i in seq_along(group_var)) {
        row_idx <- which(unique_groups == group_var[i])
        col_idx <- which(ordered_times == time_var[i])
        if (length(col_idx) > 0) {
          presence_matrix[row_idx, col_idx] <- 1
        }
      }
    } else if (type == "complete") {
      # Keep rows with no NAs in substantive variables
      complete_cases <- complete.cases(data[substantive_vars])
      filtered_data <- data[complete_cases, ]

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

      # For complete type, need to check each (group, time) combination
      # Get original data to check completeness for all combinations
      all_groups <- as.character(data[[group]])
      all_times <- as.character(data[[time]])

      # Create a completeness indicator for each row
      complete_rows <- complete.cases(data[substantive_vars])

      # Fill presence matrix with 1 only for complete rows
      for (i in seq_along(all_groups)) {
        if (complete_rows[i]) {
          row_idx <- which(unique_groups == all_groups[i])
          col_idx <- which(ordered_times == all_times[i])
          if (length(row_idx) > 0 && length(col_idx) > 0) {
            presence_matrix[row_idx, col_idx] <- 1
          }
        }
      }
    }

    # Calculate time coverage for each entity
    time_coverage_by_entity <- rowSums(presence_matrix)
    names(time_coverage_by_entity) <- rownames(presence_matrix)

    # Calculate full summary statistics (same as explore_participation)
    summary_stats <- c(
      min = min(time_coverage_by_entity),
      `5%` = as.numeric(quantile(time_coverage_by_entity, 0.05)),
      `25%` = as.numeric(quantile(time_coverage_by_entity, 0.25)),
      `50%` = as.numeric(quantile(time_coverage_by_entity, 0.50)),
      `75%` = as.numeric(quantile(time_coverage_by_entity, 0.75)),
      `95%` = as.numeric(quantile(time_coverage_by_entity, 0.95)),
      max = max(time_coverage_by_entity)
    )

    # Round statistics to nearest whole number (since they represent counts)
    summary_stats <- round(summary_stats)

    # Prepare histogram data
    hist_data <- hist(time_coverage_by_entity, plot = FALSE)

    return(list(
      coverage_by_entity = time_coverage_by_entity,
      summary_stats = summary_stats,
      histogram_data = hist_data
    ))
  }

  # Calculate coverage for the specified type
  coverage_result <- calculate_coverage(type)

  # Get overall information
  unique_groups <- unique(as.character(data[[group]]))
  unique_times <- unique(as.character(data[[time]]))

  # Reset graphical parameters on exit
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Set up plot layout - adjust margins based on detailed parameter
  if (detailed) {
    # More space at bottom for statistics
    par(mar = c(7, 4, 3, 2) + 0.1, las = 1)
  } else {
    # Normal margins without statistics
    par(mar = c(5, 4, 3, 2) + 0.1, las = 1)
  }

  # Create histogram data
  hist_data <- coverage_result$histogram_data
  summary_stats <- coverage_result$summary_stats

  # Create x-axis label (without type in parentheses)
  x_label <- paste("Time coverage by", group)

  # Create empty plot frame
  plot(
    NA,
    xlim = range(hist_data$breaks),
    ylim = c(0, max(hist_data$counts) * 1.05),
    xlab = x_label,
    ylab = "Count",
    main = "", # No main title
    frame.plot = FALSE,
    xaxs = "i",
    yaxs = "i"
  )

  # Add histogram bars WITH FILL
  for (i in seq_along(hist_data$counts)) {
    if (hist_data$counts[i] > 0) {
      rect(
        xleft = hist_data$breaks[i],
        ybottom = 0,
        xright = hist_data$breaks[i + 1],
        ytop = hist_data$counts[i],
        col = color, # FILL with color
        border = "black", # Black border for definition
        lwd = 1
      )
    }
  }

  # Add grid behind the histogram
  grid()

  # Redraw histogram bars on top of grid (to ensure they're visible)
  for (i in seq_along(hist_data$counts)) {
    if (hist_data$counts[i] > 0) {
      rect(
        xleft = hist_data$breaks[i],
        ybottom = 0,
        xright = hist_data$breaks[i + 1],
        ytop = hist_data$counts[i],
        col = color,
        border = "black",
        lwd = 1
      )
    }
  }

  # Add summary statistics below the plot if detailed = TRUE
  if (detailed) {
    # Format statistics for display
    stats_labels <- names(summary_stats)
    stats_values <- as.character(summary_stats)

    # Determine spacing
    n_stats <- length(stats_labels)

    # Calculate positions for centered horizontal display
    plot_width <- par("usr")[2] - par("usr")[1]
    x_positions <- seq(
      from = par("usr")[1] + plot_width * 0.05,
      to = par("usr")[2] - plot_width * 0.05,
      length.out = n_stats
    )

    # Add statistics below the x-axis label
    for (i in seq_len(n_stats)) {
      # Add statistic label
      mtext(
        text = stats_labels[i],
        side = 1,
        line = 4.0, # Positioned below x-axis label
        at = x_positions[i],
        cex = 0.8
      )

      # Add statistic value
      mtext(
        text = stats_values[i],
        side = 1,
        line = 5.2, # Positioned below the labels
        at = x_positions[i],
        cex = 0.9,
        font = 2
      )
    }
  }

  # Create unified return object
  result <- list(
    summary = list(
      n_entities = length(unique_groups),
      n_time_periods = length(unique_times),
      type = type,
      group_var = group,
      time_var = time
    ),
    coverage = list(
      coverage_by_entity = coverage_result$coverage_by_entity,
      summary_stats = coverage_result$summary_stats,
      histogram_data = coverage_result$histogram_data
    ),
    metadata = list(
      group_var = group,
      time_var = time,
      type = type,
      color = color,
      detailed = detailed
    )
  )

  # Return the result invisibly for further use
  invisible(result)
}
