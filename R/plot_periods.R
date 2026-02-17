#' Time Coverage Distribution Visualization
#'
#' This function calculates summary statistics and creates a histogram showing
#' the distribution of time periods covered by each entity in panel data,
#' based on the specified presence definition.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#' @param presence A character string specifying how to define entity presence: "observed", "nominal", or "complete".
#'        Default = "observed".
#' @param colors A character vector of length 2 specifying the line color and fill color for the histogram.
#'        Default = c("#1E4A3B", "#1E4A3B").
#'
#' @return Invisibly returns a list with the following components:
#' \describe{
#'   \item{`metadata`}{List containing the function name, group, time, presence, colors.}
#'   \item{`details`}{List containing:
#'         \itemize{
#'           \item `coverage_by_entity`: Named vector with number of periods covered per entity.
#'           \item `histogram_data`: Data used for histogram plotting.
#'         }
#'   }
#' }
#'
#' @details
#' \strong{Presence} parameter definitions:
#' \describe{
#'   \item{\code{"nominal"}}{Entity is present if it has a row in the data (even with only panel ID variables)}
#'   \item{\code{"observed"}}{Entity is present if it has at least one non-NA substantive variable (default)}
#'   \item{\code{"complete"}}{Entity is present only if it has no NA values in all substantive variables}
#' }
#'
#' The x-axis shows the number of time periods covered by each entity, and the
#' y-axis shows the count/frequency of entities with that coverage.
#'
#' @seealso
#' [describe_periods()], [plot_patterns()], [describe_patterns()]
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
#' plot_periods(production, group = "firm", time = "year", presence = "nominal")
#' plot_periods(production, group = "firm", time = "year", presence = "complete")
#'
#' # Custom colors - black line with gray fill
#' plot_periods(production, group = "firm", time = "year", colors = c("black", "gray"))
#'
#' # Custom colors - blue line with light blue fill
#' plot_periods(production, group = "firm", time = "year", colors = c("blue", "lightblue"))
#'
#' @export
plot_periods <- function(
  data,
  group = NULL,
  time = NULL,
  presence = "observed",
  colors = c("#1E4A3B", "#1E4A3B")
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

  # Validate colors parameter
  if (!is.character(colors) || length(colors) != 2) {
    stop(
      "'colors' must be a character vector of length 2, not ",
      class(colors)[1]
    )
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  if (!time %in% names(data)) {
    stop('variable "', time, '" not found in data')
  }

  # Extract colors
  line_color <- colors[1]
  fill_color <- colors[2]

  # Get substantive variables
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Helper function to calculate coverage for a given presence type
  calculate_coverage <- function(presence) {
    if (presence == "nominal") {
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
    } else if (presence == "observed") {
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
    } else if (presence == "complete") {
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

    # Prepare histogram data with integer breaks
    # Create a table of counts for each integer value
    coverage_table <- table(time_coverage_by_entity)
    coverage_values <- as.numeric(names(coverage_table))
    coverage_counts <- as.numeric(coverage_table)

    # Create custom histogram-like data structure
    hist_data <- list(
      breaks = coverage_values - 0.5, # Breaks at integer - 0.5
      counts = coverage_counts,
      mids = coverage_values, # Mids at integer values
      xname = "Time coverage",
      equidist = TRUE
    )

    # Add the last break point
    hist_data$breaks <- c(hist_data$breaks, max(coverage_values) + 0.5)

    return(list(
      coverage_by_entity = time_coverage_by_entity,
      histogram_data = hist_data
    ))
  }

  # Calculate coverage for the specified presence type
  coverage_result <- calculate_coverage(presence)

  # Get overall information
  unique_groups <- unique(as.character(data[[group]]))
  unique_times <- unique(as.character(data[[time]]))

  # Reset graphical parameters on exit
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Set up plot layout with normal margins (no extra space for statistics)
  par(mar = c(5, 4, 3, 2) + 0.1, las = 1)

  # Create histogram data
  hist_data <- coverage_result$histogram_data

  # Create x-axis label (without presence in parentheses)
  x_label <- paste("Time coverage by", group)

  # Get the actual data range (time coverage values)
  coverage_values <- coverage_result$coverage_by_entity

  # Ensure x-axis doesn't include 0 (since time coverage is at least 1 if an entity appears)
  # Find minimum and maximum coverage values
  x_min <- min(coverage_values)
  x_max <- max(coverage_values)

  # Since time coverage represents number of periods, it should be at least 1
  # But in practice, min could be 1 or more
  x_min <- max(1, x_min) # Start at 1 or the actual minimum, whichever is larger

  # Add space between axes and plot
  x_padding <- 0.8 # Space on x-axis
  y_padding <- max(hist_data$counts) * 0.05 # 5% space on y-axis

  # Create empty plot frame with adjusted xlim and ylim for spacing
  plot(
    NA,
    xlim = c(x_min - x_padding, x_max + x_padding),
    ylim = c(0, max(hist_data$counts) * 1.05 + y_padding),
    xlab = x_label,
    ylab = "Count",
    main = "", # No main title
    frame.plot = FALSE,
    xaxs = "r", # Regular axis style (adds padding)
    yaxs = "r", # Regular axis style (adds padding)
    xaxt = "n" # Suppress default x-axis
  )

  # Add grid FIRST (behind the histogram)
  grid()

  # Add histogram bars WITH FILL - properly aligned to integer positions
  # Bars should go from integer-0.5 to integer+0.5
  for (i in seq_along(hist_data$counts)) {
    if (hist_data$counts[i] > 0) {
      # Calculate bar boundaries
      bar_left <- hist_data$mids[i] - 0.5
      bar_right <- hist_data$mids[i] + 0.5

      rect(
        xleft = bar_left,
        ybottom = 0,
        xright = bar_right,
        ytop = hist_data$counts[i],
        col = fill_color, # FILL with second color
        border = line_color, # Line with first color
        lwd = 1
      )
    }
  }

  # Create integer x-axis ticks
  # Since time coverage can only be integer, use only integer ticks
  # Generate appropriate integer tick positions
  tick_start <- floor(x_min)
  tick_end <- ceiling(x_max)

  # Create sequence of integer ticks
  tick_positions <- tick_start:tick_end

  # Only show ticks if we have reasonable range and not too many
  if (length(tick_positions) <= 20) {
    # Limit to avoid overcrowding
    axis(1, at = tick_positions, labels = tick_positions)
  } else {
    # If too many, show every 2nd, 5th, or 10th value
    if (length(tick_positions) > 50) {
      show_every <- 10
    } else if (length(tick_positions) > 20) {
      show_every <- 5
    } else {
      show_every <- 2
    }

    show_positions <- seq(tick_start, tick_end, by = show_every)
    axis(1, at = show_positions, labels = show_positions)
  }

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time,
    presence = presence,
    colors = colors
  )

  # Build details list (excluding n_entities, n_time_periods, and summary_stats)
  details <- list(
    coverage_by_entity = coverage_result$coverage_by_entity,
    histogram_data = hist_data
  )

  # Return the result invisibly
  invisible(list(
    metadata = metadata,
    details = details
  ))
}
