#' Time Coverage Distribution Visualization
#'
#' This function calculates summary statistics and creates a histogram showing
#' the distribution of time periods covered by each entity in panel data.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#' @param colors A character vector of length 2 specifying the fill color and line color for the histogram.
#'        First color is for fill, second color is for the border line. Default = c("#1E4A3B", "white").
#'
#' @return Invisibly returns a list with summary statistics and metadata.
#'         Creates a histogram showing the distribution of time coverage across entities.
#'
#' @details
#' An entity/time combination is considered **present** if the corresponding row contains at least
#' one non-NA value in any substantive variable (i.e., all columns except the group and time identifiers).
#'
#' The x-axis shows the number of time periods covered by each entity, and the
#' y-axis shows the count/frequency of entities with that coverage.
#'
#' The returned list contains the following components:
#' \describe{
#'   \item{`metadata`}{List containing the function name, group, time, colors.}
#'   \item{`details`}{List containing:
#'         \itemize{
#'           \item `coverage_by_entity`: Named vector with number of periods covered per entity.
#'           \item `histogram_data`: Data used for histogram plotting.
#'         }
#'   }
#' }
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
#' # Custom colors - gray fill with black line
#' plot_periods(production, group = "firm", time = "year", colors = c("gray", "black"))
#'
#' # Custom colors - light blue fill with blue line
#' plot_periods(production, group = "firm", time = "year", colors = c("lightblue", "blue"))
#'
#' @export
plot_periods <- function(
  data,
  group = NULL,
  time = NULL,
  colors = c("#1E4A3B", "white") # first is fill, second is line
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

  # Extract colors - now first is fill, second is line
  fill_color <- colors[1]
  line_color <- colors[2]

  # Get substantive variables
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # --- Calculate coverage using "observed" definition ---
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

  # Calculate time coverage for each entity
  time_coverage_by_entity <- rowSums(presence_matrix)
  names(time_coverage_by_entity) <- rownames(presence_matrix)

  # Prepare histogram data with integer breaks
  coverage_table <- table(time_coverage_by_entity)
  coverage_values <- as.numeric(names(coverage_table))
  coverage_counts <- as.numeric(coverage_table)

  hist_data <- list(
    breaks = coverage_values - 0.5,
    counts = coverage_counts,
    mids = coverage_values,
    xname = "Time coverage",
    equidist = TRUE
  )
  hist_data$breaks <- c(hist_data$breaks, max(coverage_values) + 0.5)

  # --- Plotting ---
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mar = c(5, 4, 3, 2) + 0.1, las = 1)

  x_label <- paste("Time coverage by", group)
  coverage_values_vec <- time_coverage_by_entity
  x_min <- max(1, min(coverage_values_vec))
  x_max <- max(coverage_values_vec)

  x_padding <- 0.8
  y_padding <- max(hist_data$counts) * 0.05

  plot(
    NA,
    xlim = c(x_min - x_padding, x_max + x_padding),
    ylim = c(0, max(hist_data$counts) * 1.05 + y_padding),
    xlab = x_label,
    ylab = "Count",
    main = "",
    frame.plot = FALSE,
    xaxs = "r",
    yaxs = "r",
    xaxt = "n"
  )

  for (i in seq_along(hist_data$counts)) {
    if (hist_data$counts[i] > 0) {
      bar_left <- hist_data$mids[i] - 0.5
      bar_right <- hist_data$mids[i] + 0.5
      rect(
        xleft = bar_left,
        ybottom = 0,
        xright = bar_right,
        ytop = hist_data$counts[i],
        col = fill_color,
        border = line_color,
        lwd = 1
      )
    }
  }

  tick_start <- floor(x_min)
  tick_end <- ceiling(x_max)
  tick_positions <- tick_start:tick_end

  if (length(tick_positions) <= 20) {
    axis(1, at = tick_positions, labels = tick_positions)
  } else {
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
    colors = colors
  )

  details <- list(
    coverage_by_entity = time_coverage_by_entity,
    histogram_data = hist_data
  )

  invisible(list(
    metadata = metadata,
    details = details
  ))
}
