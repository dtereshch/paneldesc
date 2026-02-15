#' Heterogeneity Visualization
#'
#' This function creates visualizations of heterogeneity among groups.
#'
#' @param data A data.frame containing the variables for analysis, or a data.frame
#'        with panel attributes.
#' @param selection A character string specifying the numeric variable of interest.
#' @param group A character string or vector of character strings specifying the
#'        grouping variable(s). If data has panel attributes and group is not specified,
#'        both panel_group and panel_time will be used as group variables.
#' @param colors A character vector of two colors: first for mean line and points,
#'        second for individual points. Default = c("#1E4A3B", "#EADCA0").
#'
#' @return Invisibly returns a list with summary statistics. Creates a plot showing group heterogeneity.
#'
#' @details
#' The returned list contains the following components:
#' \describe{
#'   \item{\code{summary}}{List with overall summary statistics including:
#'     \itemize{
#'       \item \code{overall_mean}: Mean of the selected variable across all data
#'       \item \code{overall_sd}: Standard deviation of the selected variable across all data
#'       \item \code{n_observations}: Total number of observations
#'     }
#'   }
#'   \item{\code{group_stats}}{List with statistics for each grouping variable including:
#'     \itemize{
#'       \item \code{means}: Group means
#'       \item \code{sd}: Group standard deviations
#'       \item \code{n}: Number of observations per group
#'     }
#'   }
#'   \item{\code{metadata}}{List with analysis parameters including:
#'     \itemize{
#'       \item \code{selection_var}: The selected variable name
#'       \item \code{group_vars}: The grouping variable name(s)
#'       \item \code{colors}: Colors used for plotting
#'       \item \code{used_panel_attrs}: Logical indicating whether panel attributes were used
#'     }
#'   }
#' }
#'
#' @seealso
#' [decompose_numeric()], [summarize_numeric()]
#'
#' @examples
#' data(production)
#'
#' # Method 1: With regular data.frame
#' plot_heterogeneity(production, selection = "labor", group = "year")
#'
#' # Method 2: With data.frame with panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' plot_heterogeneity(panel_data, selection = "labor")
#'
#' # Method 3: Explicit grouping even with panel data
#' plot_heterogeneity(panel_data, selection = "capital", group = "year")
#'
#' # Plot labor by year
#' plot_heterogeneity(production, selection = "labor", group = "year")
#'
#' # Plot capital by firm
#' plot_heterogeneity(production, selection = "capital", group = "firm")
#'
#' # Plot sales with multiple grouping variables
#' plot_heterogeneity(production, selection = "sales", group = c("firm", "year"))
#'
#' # Customize colors
#' plot_heterogeneity(production, selection = "sales", group = "year", colors = c("black", "gray"))
#'
#' @export
plot_heterogeneity <- function(
  data,
  selection,
  group = NULL,
  colors = c("#1E4A3B", "#EADCA0")
) {
  # Check if data has panel attributes
  has_panel_attrs <- !is.null(attr(data, "panel_group")) &&
    !is.null(attr(data, "panel_time"))

  # Handle panel attributes if present and group is not specified
  if (has_panel_attrs && is.null(group)) {
    # Extract group and time from attributes and use both as group variables
    group <- c(attr(data, "panel_group"), attr(data, "panel_time"))
    used_panel_attrs <- TRUE
  } else {
    used_panel_attrs <- FALSE
  }

  # Input validation for data
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
  }

  # Input validation for selection
  if (!is.character(selection) || length(selection) != 1) {
    stop(
      "'selection' must be a single character string, not ",
      class(selection)[1]
    )
  }

  if (!selection %in% names(data)) {
    stop('variable "', selection, '" not found in data')
  }

  if (!is.numeric(data[[selection]])) {
    stop(
      "'selection' must be a numeric variable, not ",
      class(data[[selection]])[1]
    )
  }

  # Input validation for group (after potential extraction from panel attributes)
  if (is.null(group)) {
    stop(
      "'group' must be specified. For regular data.frames, provide a character string or vector."
    )
  }

  if (!is.character(group)) {
    stop(
      "'group' must be a character string or vector of character strings, not ",
      class(group)[1]
    )
  }

  # Check that all group variables exist in data
  missing_groups <- group[!group %in% names(data)]
  if (length(missing_groups) > 0) {
    stop(
      'variable(s) "',
      paste(missing_groups, collapse = '", "'),
      '" not found in data'
    )
  }

  # Validate colors parameter
  if (!is.character(colors) || length(colors) != 2) {
    stop(
      "'colors' must be a character vector of length 2, not ",
      class(colors)[1]
    )
  }

  # Check that data has at least one row
  if (nrow(data) == 0) {
    stop("'data' must have at least one row")
  }

  # Check if variables exist in data (redundant but kept for consistency)
  if (!selection %in% names(data)) {
    stop("variable '", selection, "' not found in data")
  }

  missing_groups <- setdiff(group, names(data))
  if (length(missing_groups) > 0) {
    stop(
      "group variable(s) '",
      paste(missing_groups, collapse = "', '"),
      "' not found in data"
    )
  }

  # Extract colors
  mean_col <- colors[1]
  point_col <- colors[2]

  # Set default parameter values
  point_alpha <- 0.75
  mean_lwd <- 2
  cex <- 1
  las <- 1
  plot <- TRUE
  ncol <- NULL
  nrow <- NULL

  # Extract the main variable
  y_var <- data[[selection]]

  # Check variable type
  if (!is.numeric(y_var)) {
    stop("'selection' must be a numeric variable")
  }

  # Function to create single plot
  create_single_plot <- function(
    data_sub,
    group_var
  ) {
    x_var <- data_sub[[group_var]]

    # Check group variable type
    if (!is.factor(x_var) && !is.character(x_var) && !is.numeric(x_var)) {
      stop(
        "group variable '",
        group_var,
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
    group_means <- tapply(data_sub[[selection]], x_var, mean, na.rm = TRUE)

    # Create the plot
    plot(
      NA,
      xlim = c(0.5, length(levels(x_var)) + 0.5),
      ylim = range(data_sub[[selection]], na.rm = TRUE),
      xlab = group_var,
      ylab = selection,
      main = "", # Remove title
      xaxt = "n",
      frame.plot = FALSE
    )

    # Add x-axis
    axis(1, at = seq_along(levels(x_var)), labels = levels(x_var))

    # Generate reproducible jitter by setting a seed based on the data
    # Save current seed to restore later
    if (exists(".Random.seed", .GlobalEnv)) {
      old_seed <- .GlobalEnv$.Random.seed
      on.exit(.GlobalEnv$.Random.seed <- old_seed, add = TRUE)
    } else {
      on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
    }

    # Create a deterministic seed from the data
    # Use a combination of the grouping variable name and the data itself
    set.seed(
      sum(as.integer(charToRaw(paste(group_var, selection)))) +
        sum(data_sub[[selection]], na.rm = TRUE) +
        nrow(data_sub) +
        length(unique(x_var))
    )

    # Add individual points with jitter
    x_jitter <- jitter(as.numeric(x_var), amount = 0.2)
    points(
      x_jitter,
      data_sub[[selection]],
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
      sd = tapply(data_sub[[selection]], x_var, sd, na.rm = TRUE),
      n = tapply(data_sub[[selection]], x_var, function(x) sum(!is.na(x)))
    )
  }

  # Calculate overall summary statistics
  summary_stats <- list(
    summary = list(
      overall_mean = mean(y_var, na.rm = TRUE),
      overall_sd = sd(y_var, na.rm = TRUE),
      n_observations = nrow(data)
    ),
    group_stats = list(),
    metadata = list(
      selection_var = selection,
      group_vars = group,
      colors = colors,
      used_panel_attrs = used_panel_attrs
    )
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
      group_stats <- create_single_plot(data, group_var)
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
        means = tapply(data[[selection]], x_var, mean, na.rm = TRUE),
        sd = tapply(data[[selection]], x_var, sd, na.rm = TRUE),
        n = tapply(data[[selection]], x_var, function(x) sum(!is.na(x)))
      )
    }
  }

  # Return summary statistics invisibly
  invisible(summary_stats)
}
