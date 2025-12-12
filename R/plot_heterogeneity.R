#' Plot Heterogeneity Among Groups
#'
#' Creates publication-ready visualizations of heterogeneity among groups using base R graphics.
#' Designed for panel data to show variation across object identifiers or time periods. Supports
#' multiple grouping variables (faceting).
#'
#' @param data A data frame in long panel format or tidy format
#' @param variable The numeric variable of interest (character string)
#' @param group The grouping variable(s) (character string or vector of character strings)
#' @param xlab X-axis label (default: based on grouping variable)
#' @param ylab Y-axis label (default: based on variable name)
#' @param colors Vector of two colors: first for individual points, second for mean line and points (default: c("#D55E00", "#0072B2"))
#'
#' @return Invisibly returns a list with summary statistics. Creates a base R plot.
#'
#' @seealso [decompose_variation()], [plot_participation()]
#'
#' @examples
#' data(production)
#'
#' # Plot labor by year
#' plot_heterogeneity(production, variable = "labor", group = "year")
#'
#' # Plot capital by firm
#' plot_heterogeneity(production, variable = "capital", group = "firm")
#'
#' # Plot sales with multiple grouping variables
#' plot_heterogeneity(production, variable = "sales", group = c("firm", "year"))
#'
#' # Customize colors
#' plot_heterogeneity(production, variable = "sales", group = "year",
#'                   colors = c("gray50", "black"))
#'
#' @export
plot_heterogeneity <- function(
  data,
  variable,
  group = NULL,
  xlab = NULL,
  ylab = NULL,
  colors = c("#D55E00", "#0072B2")
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (nrow(data) == 0) {
    stop("'data' must have at least one row")
  }

  # Require variable to be character string
  if (!is.character(variable) || length(variable) != 1) {
    stop("'variable' must be a single character string")
  }

  # If group is NULL, error
  if (is.null(group)) {
    stop("'group' must be provided")
  }

  # Validate group parameter
  if (!is.character(group)) {
    stop("'group' must be a character string or vector of character strings")
  }

  # Check if variables exist in data
  if (!variable %in% names(data)) {
    stop("Variable '", variable, "' not found in data")
  }

  missing_groups <- setdiff(group, names(data))
  if (length(missing_groups) > 0) {
    stop(
      "Group variable(s) '",
      paste(missing_groups, collapse = "', '"),
      "' not found in data"
    )
  }

  # Validate colors parameter
  if (!is.character(colors) || length(colors) != 2) {
    stop("'colors' must be a character vector of length 2")
  }

  # Extract colors
  point_col <- colors[1]
  mean_col <- colors[2]

  # Set default parameter values that were removed
  point_alpha <- 0.6
  mean_lwd <- 2
  cex <- 1
  las <- 1
  plot <- TRUE
  ncol <- NULL
  nrow <- NULL

  # Extract the main variable
  y_var <- data[[variable]]

  # Check variable type
  if (!is.numeric(y_var)) {
    stop("'variable' must be a numeric variable")
  }

  # Function to create single plot
  create_single_plot <- function(
    data_sub,
    group_var,
    xlab_single = NULL,
    ylab_single = NULL
  ) {
    x_var <- data_sub[[group_var]]

    # Check group variable type
    if (!is.factor(x_var) && !is.character(x_var) && !is.numeric(x_var)) {
      stop(
        "Group variable '",
        group_var,
        "' must be a factor, character, or numeric variable"
      )
    }

    # Convert grouping variable to factor for consistent handling
    if (!is.factor(x_var)) {
      x_var <- as.factor(x_var)
    }

    # Set default labels if not provided
    if (is.null(xlab_single)) {
      xlab_single <- group_var
    }
    if (is.null(ylab_single)) {
      ylab_single <- variable
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
    group_means <- tapply(data_sub[[variable]], x_var, mean, na.rm = TRUE)

    # Create the plot
    plot(
      NA,
      xlim = c(0.5, length(levels(x_var)) + 0.5),
      ylim = range(data_sub[[variable]], na.rm = TRUE),
      xlab = xlab_single,
      ylab = ylab_single,
      main = "", # Remove title
      xaxt = "n",
      frame.plot = FALSE
    )

    # Add x-axis
    axis(1, at = seq_along(levels(x_var)), labels = levels(x_var))

    # Add individual points with jitter
    x_jitter <- jitter(as.numeric(x_var), amount = 0.2)
    points(
      x_jitter,
      data_sub[[variable]],
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

    # Add legend to every plot
    legend(
      "topright",
      legend = c("Individual observations", "Group means"),
      col = c(point_col, mean_col),
      pch = c(16, 18),
      lty = c(NA, 1),
      pt.cex = c(0.8, 1.5),
      bty = "n",
      cex = 0.8 * cex # Slightly smaller legend for multi-panel plots
    )

    # Return summary statistics for this group
    list(
      means = group_means,
      sd = tapply(data_sub[[variable]], x_var, sd, na.rm = TRUE),
      n = tapply(data_sub[[variable]], x_var, function(x) sum(!is.na(x)))
    )
  }

  # Calculate overall summary statistics
  summary_stats <- list(
    overall_mean = mean(y_var, na.rm = TRUE),
    overall_sd = sd(y_var, na.rm = TRUE),
    group_stats = list()
  )

  if (plot) {
    # Set up plotting layout for multiple groups
    if (length(group) > 1) {
      # Determine grid dimensions
      n_plots <- length(group)
      if (is.null(ncol) && is.null(nrow)) {
        ncol <- ceiling(sqrt(n_plots))
        nrow <- ceiling(n_plots / ncol)
      } else if (is.null(ncol)) {
        ncol <- ceiling(n_plots / nrow)
      } else if (is.null(nrow)) {
        nrow <- ceiling(n_plots / ncol)
      }

      # Save current par settings
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))

      # Set up multi-panel plot
      par(
        mfrow = c(nrow, ncol),
        mar = c(4, 4, 2, 1) + 0.1, # Tighter margins for multi-panel
        oma = c(2, 2, 2, 2), # Outer margins for overall labels
        las = las
      )
    } else {
      # Single plot setup
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))
      par(mar = c(5, 4, 2, 2) + 0.1, las = las)
    }

    # Create plots for each grouping variable
    for (i in seq_along(group)) {
      group_var <- group[i]

      # Use provided labels only for single plot, otherwise use defaults
      xlab_single <- if (length(group) == 1) xlab else NULL
      ylab_single <- if (length(group) == 1) ylab else NULL

      group_stats <- create_single_plot(
        data,
        group_var,
        xlab_single,
        ylab_single
      )
      summary_stats$group_stats[[group_var]] <- group_stats
    }
  } else {
    # Calculate statistics without plotting
    for (group_var in group) {
      x_var <- data[[group_var]]
      if (!is.factor(x_var)) {
        x_var <- as.factor(x_var)
      }

      summary_stats$group_stats[[group_var]] <- list(
        means = tapply(data[[variable]], x_var, mean, na.rm = TRUE),
        sd = tapply(data[[variable]], x_var, sd, na.rm = TRUE),
        n = tapply(data[[variable]], x_var, function(x) sum(!is.na(x)))
      )
    }
  }

  # Return summary statistics invisibly
  invisible(summary_stats)
}
