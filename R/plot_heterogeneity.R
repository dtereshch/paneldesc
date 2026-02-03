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
#' - Row titles show the selection variable names
#' - Column titles show the group variable names
#'
#' When only one variable is specified for either parameter, a single plot or single row/column is created.
#' The legend is placed above all plots and is horizontal and centered.
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

  # Check for too many plots
  n_plots <- length(selection) * length(group)
  if (n_plots > 12) {
    warning(
      "Creating ",
      n_plots,
      " plots. This may be difficult to view. ",
      "Consider reducing the number of variables in 'selection' or 'group'."
    )
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

  # Function to create single plot
  create_single_plot <- function(
    data_sub,
    y_var_name,
    group_var_name,
    show_xlab = TRUE,
    show_ylab = TRUE,
    current_mar
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

    # Set margins for this plot
    current_mar_copy <- current_mar

    # Adjust margins for axis labels
    if (!show_xlab) {
      current_mar_copy[1] <- current_mar_copy[1] - 1.5 # Reduce bottom margin
    }
    if (!show_ylab) {
      current_mar_copy[2] <- current_mar_copy[2] - 1.5 # Reduce left margin
    }

    par(mar = current_mar_copy)

    # Create the plot
    plot(
      NA,
      xlim = c(0.5, length(levels(x_var)) + 0.5),
      ylim = range(y_var, na.rm = TRUE),
      xlab = if (show_xlab) group_var_name else "",
      ylab = if (show_ylab) y_var_name else "",
      main = "",
      xaxt = "n",
      frame.plot = FALSE
    )

    # Add x-axis
    axis(1, at = seq_along(levels(x_var)), labels = levels(x_var))

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

    # Create a more complex layout with spaces for titles
    # Layout structure:
    # Row 1: Legend (spanning all columns)
    # Row 2: Column titles (if multi-column)
    # Remaining rows: Plots with row titles on the side

    # Calculate layout dimensions
    has_row_titles <- n_rows > 1
    has_col_titles <- n_cols > 1

    # Total rows in layout: legend + column titles + plot rows
    total_layout_rows <- 1 + as.integer(has_col_titles) + n_rows

    # Total columns in layout: row titles + plot columns
    total_layout_cols <- as.integer(has_row_titles) + n_cols

    # Create empty layout matrix
    layout_matrix <- matrix(
      0,
      nrow = total_layout_rows,
      ncol = total_layout_cols
    )

    # Start numbering from 1 (legend)
    current_idx <- 1

    # --- Row 1: Legend (spanning all columns) ---
    layout_matrix[1, ] <- current_idx
    current_idx <- current_idx + 1

    # --- Row 2: Column titles (if needed) ---
    if (has_col_titles) {
      # Skip row title column if present
      start_col <- if (has_row_titles) 2 else 1

      for (j in 1:n_cols) {
        layout_matrix[2, start_col + j - 1] <- current_idx
        current_idx <- current_idx + 1
      }
    }

    # --- Remaining rows: Plots with row titles ---
    plot_row_start <- 2 + as.integer(has_col_titles)

    for (i in 1:n_rows) {
      current_row <- plot_row_start + i - 1

      # Row title (if needed, first column)
      if (has_row_titles) {
        layout_matrix[current_row, 1] <- current_idx
        current_idx <- current_idx + 1
      }

      # Plots
      start_col <- if (has_row_titles) 2 else 1
      for (j in 1:n_cols) {
        layout_matrix[current_row, start_col + j - 1] <- current_idx
        current_idx <- current_idx + 1
      }
    }

    # Set layout widths and heights
    layout_widths <- rep(1, total_layout_cols)
    layout_heights <- rep(1, total_layout_rows)

    # Adjust widths for row titles
    if (has_row_titles) {
      layout_widths[1] <- 0.3 # Narrower for row titles
      layout_widths[-1] <- 0.7 / (n_cols) # Even distribution for plots
    }

    # Adjust heights
    layout_heights[1] <- 0.1 # Legend row (10%)
    if (has_col_titles) {
      layout_heights[2] <- 0.05 # Column titles row (5%)
    }
    # Remaining rows (plots) share the rest
    plot_rows_height <- 0.85
    plot_rows <- total_layout_rows - 1 - as.integer(has_col_titles)
    layout_heights[(total_layout_rows - plot_rows + 1):total_layout_rows] <-
      plot_rows_height / plot_rows

    # Apply layout
    layout(
      layout_matrix,
      widths = layout_widths,
      heights = layout_heights
    )

    # Set base margins for plots
    base_mar <- c(3, 3, 1, 1) + 0.1

    # --- Create Legend ---
    par(mar = c(0, 0, 0, 0))
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))

    # Add horizontal, centered legend
    legend(
      "center",
      legend = c("Individual observations", "Group means"),
      col = c(point_col, mean_col),
      pch = c(16, 18),
      lty = c(NA, 1),
      pt.cex = c(1.0, 1.3),
      lwd = c(NA, mean_lwd),
      bty = "n",
      cex = 0.9,
      horiz = TRUE,
      xpd = NA
    )

    # --- Create Column Titles ---
    if (has_col_titles) {
      for (j in 1:n_cols) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        plot.window(xlim = c(0, 1), ylim = c(0, 1))
        text(
          0.5,
          0.5,
          group[j],
          cex = 1.2,
          font = 2,
          xpd = NA
        )
        box()
      }
    }

    # --- Create Row Titles ---
    if (has_row_titles) {
      for (i in 1:n_rows) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        plot.window(xlim = c(0, 1), ylim = c(0, 1))
        text(
          0.5,
          0.5,
          selection[i],
          cex = 1.2,
          font = 2,
          srt = 90, # Rotate 90 degrees for vertical text
          xpd = NA
        )
        box()
      }
    }

    # --- Create Plots ---
    # Create plots in grid: rows = selection variables, columns = group variables
    for (i in 1:n_rows) {
      y_var_name <- selection[i]

      for (j in 1:n_cols) {
        group_var_name <- group[j]

        # Determine whether to show axis labels
        # Show x-axis label only for bottom row
        show_xlab <- (n_rows == 1 && n_cols == 1) || (i == n_rows)
        # Show y-axis label only for first column
        show_ylab <- (n_rows == 1 && n_cols == 1) || (j == 1)

        # Create plot for this combination
        group_stats <- create_single_plot(
          data,
          y_var_name,
          group_var_name,
          show_xlab,
          show_ylab,
          base_mar
        )

        # Store statistics
        if (!y_var_name %in% names(summary_stats$group_stats)) {
          summary_stats$group_stats[[y_var_name]] <- list()
        }
        summary_stats$group_stats[[y_var_name]][[group_var_name]] <- group_stats

        # Reset to base margins after each plot
        par(mar = base_mar)
      }
    }
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
