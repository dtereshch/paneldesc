#' Heterogeneity Visualization
#'
#' This function creates visualizations of heterogeneity among groups. It can handle
#' multiple numeric variables and multiple grouping variables, arranging them in a
#' grid where selection variables form rows and group variables form columns.
#'
#' @param data A data.frame containing the variables for analysis.
#' @param selection A character vector specifying the numeric variable(s) of interest.
#'        If NULL (default), all numeric variables in the dataset will be used.
#' @param group A character vector specifying the grouping variable(s).
#' @param colors A character vector of two colors: first for individual points, second for mean line and points.
#'        Default = c("#D55E00", "#0072B2").
#'
#' @return Invisibly returns a list with summary statistics. Creates plot(s) showing group heterogeneity.
#'
#' @details
#' When both `selection` and `group` contain multiple variables, plots are arranged in a grid:
#' - Rows correspond to variables in `selection`
#' - Columns correspond to variables in `group`
#' - Y-axis titles and tick labels are shown only for plots in the left column
#' - X-axis titles and tick labels are shown only for plots in the bottom row
#' - A single common legend is displayed centered below the grid
#' - Column names are always used as axis labels
#'
#' When only one variable is specified for either parameter, a single plot is created as a 1x1 grid.
#'
#' @seealso
#' [summarize_panel()], [plot_participation()]
#'
#' @examples
#' data(production)
#'
#' # Plot labor by year (single plot)
#' plot_heterogeneity(production, selection = "labor", group = "year")
#'
#' # Plot capital by firm (single plot)
#' plot_heterogeneity(production, selection = "capital", group = "firm")
#'
#' # Plot multiple variables with single grouping variable
#' plot_heterogeneity(production, selection = c("sales", "labor", "capital"), group = "year")
#'
#' # Plot single variable with multiple grouping variables
#' plot_heterogeneity(production, selection = "sales", group = c("firm", "year", "industry"))
#'
#' # Plot multiple variables with multiple grouping variables (grid)
#' plot_heterogeneity(production, selection = c("sales", "labor"), group = c("year", "industry"))
#'
#' # Use all numeric variables with default
#' plot_heterogeneity(production, group = "year")
#'
#' # Customize colors
#' plot_heterogeneity(production, selection = "sales", group = "year", colors = c("gray", "black"))
#'
#' @export
plot_heterogeneity <- function(
  data,
  selection = NULL,
  group,
  colors = c("#D55E00", "#0072B2")
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
  }

  if (!is.null(selection) && !is.character(selection)) {
    stop(
      "'selection' must be a character vector or NULL, not ",
      class(selection)[1]
    )
  }

  if (!is.character(group)) {
    stop(
      "'group' must be a character string or vector of character strings, not ",
      class(group)[1]
    )
  }

  # Check for missing variables in data
  missing_vars <- setdiff(c(selection, group), names(data))
  if (length(missing_vars) > 0) {
    stop(
      "the following variables were not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  if (!is.character(colors) || length(colors) != 2) {
    stop(
      "'colors' must be a character vector of length 2, not ",
      class(colors)[1]
    )
  }

  data <- .check_and_convert_data_robust(data, arg_name = "data")

  if (nrow(data) == 0) {
    stop("'data' must have at least one row")
  }

  # If selection is NULL, use all numeric variables
  if (is.null(selection)) {
    # Identify numeric variables
    numeric_vars <- vapply(data, is.numeric, FUN.VALUE = logical(1))
    selection <- names(data)[numeric_vars]

    # Remove group variables from selection if they are numeric
    selection <- setdiff(selection, group)

    if (length(selection) == 0) {
      stop(
        "no numeric variables found in the dataset (excluding group variables)"
      )
    }

    message(
      "Analyzing all numeric variable(s): ",
      paste(selection, collapse = ", ")
    )
  }

  # Validate selection variables are numeric
  for (var in selection) {
    if (!is.numeric(data[[var]])) {
      stop(
        "variable '",
        var,
        "' must be numeric, not ",
        class(data[[var]])[1]
      )
    }
  }

  # Extract colors
  point_col <- colors[1]
  mean_col <- colors[2]

  # Set default parameter values
  point_alpha <- 0.6
  mean_lwd <- 2
  cex <- 1
  las <- 1
  plot <- TRUE

  # Function to create single plot - no legend in individual plots
  create_single_plot <- function(
    data_sub,
    y_var_name,
    group_var_name,
    show_xlab = TRUE,
    show_ylab = TRUE
  ) {
    y_var <- data_sub[[y_var_name]]
    x_var <- data_sub[[group_var_name]]

    # Check group variable type
    if (!is.factor(x_var) && !is.character(x_var) && !is.numeric(x_var)) {
      stop(
        "group variable '",
        group_var_name,
        "' must be a factor, character, or numeric variable, not ",
        class(x_var)[1]
      )
    }

    # Convert grouping variable to factor for consistent handling
    if (!is.factor(x_var)) {
      x_var <- as.factor(x_var)
    }

    # Create color with alpha
    point_col_rgb <- col2rgb(point_col) / 255
    point_col_alpha <- rgb(
      point_col_rgb[1],
      point_col_rgb[2],
      point_col_rgb[3],
      alpha = point_alpha
    )

    # Calculate group means
    group_means <- tapply(y_var, x_var, mean, na.rm = TRUE)

    # Create the plot with appropriate axis control
    plot(
      NA,
      xlim = c(0.5, length(levels(x_var)) + 0.5),
      ylim = range(y_var, na.rm = TRUE),
      xlab = if (show_xlab) group_var_name else "",
      ylab = if (show_ylab) y_var_name else "",
      main = "",
      xaxt = if (show_xlab) "n" else "n", # Always suppress default x-axis
      yaxt = if (show_ylab) "n" else "n", # Suppress default y-axis
      frame.plot = FALSE
    )

    # Add x-axis ticks and labels only if show_xlab is TRUE
    if (show_xlab) {
      axis(1, at = seq_along(levels(x_var)), labels = levels(x_var))
    } else {
      # No ticks or labels for non-bottom row plots
      axis(1, at = seq_along(levels(x_var)), labels = FALSE, tick = FALSE)
    }

    # Add y-axis ticks and labels only if show_ylab is TRUE
    if (show_ylab) {
      axis(2)
    } else {
      # No ticks or labels for non-left column plots
      axis(2, labels = FALSE, tick = FALSE)
    }

    # Add individual points with jitter
    x_jitter <- jitter(as.numeric(x_var), amount = 0.2)
    points(
      x_jitter,
      y_var,
      col = point_col_alpha,
      pch = 16,
      cex = 0.8 * cex
    )

    # Add mean line and points
    lines(
      seq_along(group_means),
      group_means,
      col = mean_col,
      lwd = mean_lwd,
      type = "o",
      pch = 18,
      cex = 1.5 * cex
    )

    # Add grid
    grid()

    # Return summary statistics for this combination
    list(
      means = group_means,
      sd = tapply(y_var, x_var, sd, na.rm = TRUE),
      n = tapply(y_var, x_var, function(x) sum(!is.na(x)))
    )
  }

  # Initialize summary statistics list
  summary_stats <- list(
    overall_stats = list(),
    group_stats = list()
  )

  # Calculate overall summary statistics for each selection variable
  for (y_var_name in selection) {
    y_var <- data[[y_var_name]]
    summary_stats$overall_stats[[y_var_name]] <- list(
      mean = mean(y_var, na.rm = TRUE),
      sd = sd(y_var, na.rm = TRUE),
      n = sum(!is.na(y_var))
    )
  }

  if (plot) {
    # Set up plotting layout
    n_rows <- length(selection)
    n_cols <- length(group)

    # Save current par settings
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))

    # Determine the optimal layout based on the number of plots
    # We'll create a layout with minimal empty space
    layout_matrix <- matrix(
      1:(n_rows * n_cols),
      nrow = n_rows,
      ncol = n_cols,
      byrow = TRUE
    )

    # Add an extra row at the bottom for the legend (smaller height)
    layout_matrix <- rbind(layout_matrix, rep(n_rows * n_cols + 1, n_cols))

    # Adjust heights: plots get most space, legend gets minimal space
    plot_height <- 5 # Base height for plots
    legend_height <- 0.8 # Minimal height for legend

    layout(layout_matrix, heights = c(rep(plot_height, n_rows), legend_height))

    # Use outer margins to provide space for labels
    # Reduced outer margins to minimize empty space
    par(oma = c(2, 2, 1, 1), las = las)

    # Create plots in grid: rows = selection variables, columns = group variables
    for (i in seq_along(selection)) {
      y_var_name <- selection[i]

      for (j in seq_along(group)) {
        group_var_name <- group[j]

        # Determine if we should show x and y labels and ticks
        # Show Y-axis title and tick labels only for left column (j == 1)
        # Show X-axis title and tick labels only for bottom row (i == n_rows)
        show_ylab <- (j == 1)
        show_xlab <- (i == n_rows)

        # Set margins for each plot - minimized but with enough space for labels
        # Top margin: minimal for all plots (no titles)
        # Bottom margin: more space for bottom row plots (x-axis labels)
        # Left margin: more space for left column plots (y-axis labels)
        # Right margin: minimal for all plots
        top_margin <- 0.5
        bottom_margin <- if (show_xlab) 3 else 1
        left_margin <- if (show_ylab) 3.5 else 1.5
        right_margin <- 0.5

        par(mar = c(bottom_margin, left_margin, top_margin, right_margin))

        # Create plot
        group_stats <- create_single_plot(
          data,
          y_var_name,
          group_var_name,
          show_xlab = show_xlab,
          show_ylab = show_ylab
        )

        # Store statistics
        if (!y_var_name %in% names(summary_stats$group_stats)) {
          summary_stats$group_stats[[y_var_name]] <- list()
        }
        summary_stats$group_stats[[y_var_name]][[group_var_name]] <- group_stats
      }
    }

    # Create the legend in the dedicated space at the bottom
    # Reset margins for the legend area
    par(mar = c(0, 0, 0, 0))
    plot.new()

    # Create centered horizontal legend
    # Calculate legend width to position it properly
    legend_text_width <- strwidth(
      "Individual observations and Group means",
      cex = 0.9
    )

    # Position legend at the center of the legend area
    legend(
      x = 0.5,
      y = 0.5, # Center of the legend area
      legend = c("Individual observations", "Group means"),
      col = c(point_col, mean_col),
      pch = c(16, 18),
      lty = c(NA, 1),
      pt.cex = c(0.8, 1.2), # Slightly reduced point size
      bty = "n",
      cex = 0.85, # Slightly smaller font for better fit
      horiz = TRUE,
      xjust = 0.5, # Center horizontally
      yjust = 0.5, # Center vertically
      xpd = TRUE, # Allow drawing in outer margin area
      text.width = max(strwidth(
        c("Individual observations", "Group means"),
        cex = 0.85
      ))
    )
  } else {
    # Calculate statistics without plotting
    for (y_var_name in selection) {
      summary_stats$group_stats[[y_var_name]] <- list()

      for (group_var_name in group) {
        x_var <- data[[group_var_name]]
        y_var <- data[[y_var_name]]

        if (!is.factor(x_var)) {
          x_var <- as.factor(x_var)
        }

        summary_stats$group_stats[[y_var_name]][[group_var_name]] <- list(
          means = tapply(y_var, x_var, mean, na.rm = TRUE),
          sd = tapply(y_var, x_var, sd, na.rm = TRUE),
          n = tapply(y_var, x_var, function(x) sum(!is.na(x)))
        )
      }
    }
  }

  # Return summary statistics invisibly
  invisible(summary_stats)
}
