#' Plot Demeaned Variable(s)
#'
#' This function performs within-group demeaning (centering) for selected numeric
#' variable(s) and creates a visualization.
#'
#' @param data A data.frame containing variables for analysis.
#' @param select A character vector of length 1 or 2 specifying the name(s) of
#'        the numeric variable(s) to demean and plot.
#' @param group A character string or vector of character strings specifying
#'        the grouping variable(s).
#'        If not specified and data is a `panel_data` object, the entity and time values
#'        will be extracted from the data.frame attributes to define grouping variables.
#'        Otherwise, overall demeaning is performed.
#' @param colors A character vector of length 2 specifying the colors for the plot.
#'        First color is for fill, second color is for the border line.
#'        Default = `c("darkblue", "white")`.
#'
#' @return Invisibly returns a list with the demeaned values and metadata.
#'
#' @details
#' The function creates a plot which shape depends on the `select` argument:
#' * If one variable is specified, a histogram of the demeaned values is plotted.
#' * If two variables are specified, a scatterplot of the demeaned values is plotted.
#'
#' The demeaning is performed by calling [make_demeaned()] internally, so it
#' shares all specifics of that function.
#'
#' Depending on the value of `group` argument, the function uses different
#' demeaning procedures:
#' * If grouping variable is not specified and `data` is not a `panel_data` object,
#'   simple overall demeaning is performed: for each numeric variable, the
#'   overall mean (ignoring `NA`s) is subtracted.
#' * If one group variable is specified, the exact calculation is used:
#'   the group mean is subtracted from each observation.
#' * If two or more groups are specidied, iterative Gauss–Seidel algorithm is used.
#'   The algorithm runs up to 2000 iterations with tolerance 1e-6
#'   (matching the defaults of `fixest::demean()`);
#'   a warning is issued if convergence is not reached.
#'
#' The returned list contains:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing the demeaned data. For a single variable,
#'         this is a numeric vector of demeaned values. For two variables, it is
#'         a data frame with columns `x` and `y` containing the demeaned values.}
#' }
#'
#' @note
#' #' Only the selected variable(s) and the grouping variables are passed to
#' `make_demeaned`, so no other numeric variables in the original data
#' are demeaned or cause message output.
#'
#' @seealso
#' See also [make_demeaned()], [plot_heterogeneity()], [decompose_numeric()]
#'
#' @examples
#' data(production)
#'
#' # Histogram of a single variable (overall demeaning)
#' plot_demeaned(production, select = "labor")
#'
#' # Scatterplot of two variables demeaned by a single group
#' plot_demeaned(production, select = c("labor", "capital"),
#'               group = "firm")
#'
#' # Demeaning with two grouping variables
#' plot_demeaned(production, select = c("labor", "capital"), group = c("firm", "year"))
#'
#' # Demeaning with multiple grouping variables
#' plot_demeaned(production, select = c("labor", "capital"), group = c("industry", "region", "year"))
#'
#' # With panel_data object (automatically demeans by entity and time)
#' panel <- make_panel(production, index = c("firm", "year"))
#' plot_demeaned(panel, select = c("labor", "capital"))
#'
#' # Custom colors
#' plot_demeaned(production, select = c("labor", "capital"), group = c("firm", "year"),
#'               colors = c("gray", "black"))
#'
#' # Accessing the returned list components
#' out_plo_dem <- plot_demeaned(production, select = "labor", group = "firm")
#' out_plo_dem$metadata
#' out_plo_dem$details
#'
#' @importFrom graphics hist axis par abline points rect plot
#' @export
plot_demeaned <- function(
  data,
  select,
  group = NULL,
  colors = c("darkblue", "white")
) {
  # ----- Input validation -------------------------------------------------
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1], call. = FALSE)
  }

  if (!is.character(select) || !length(select) %in% c(1, 2)) {
    stop("'select' must be a character vector of length 1 or 2", call. = FALSE)
  }

  if (!is.null(group) && !is.character(group)) {
    stop(
      "'group' must be a character vector or NULL, not ",
      class(group)[1],
      call. = FALSE
    )
  }

  if (!is.character(colors) || length(colors) != 2) {
    stop("'colors' must be a character vector of length 2", call. = FALSE)
  }

  # Check that selected variables exist
  missing_vars <- select[!select %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(
      "variable(s) not found in data: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # Check that selected variables are numeric
  for (v in select) {
    if (!is.numeric(data[[v]])) {
      stop(
        "'select' variable '",
        v,
        "' must be numeric, not ",
        class(data[[v]])[1],
        call. = FALSE
      )
    }
  }

  # ----- Determine grouping variables (mimics make_demeaned logic) -------
  group_used <- group
  if (inherits(data, "panel_data") && is.null(group_used)) {
    meta <- attr(data, "metadata")
    if (is.null(meta) || is.null(meta$entity) || is.null(meta$time)) {
      stop(
        "'panel_data' object missing required metadata (entity and time)",
        call. = FALSE
      )
    }
    group_used <- c(meta$entity, meta$time)
  }

  # Validate group_used (if not NULL, check existence)
  if (!is.null(group_used)) {
    if (length(group_used) == 0) {
      group_used <- NULL
    } else {
      missing_group <- group_used[!group_used %in% names(data)]
      if (length(missing_group) > 0) {
        stop(
          "grouping variable(s) not found in data: ",
          paste(missing_group, collapse = ", "),
          call. = FALSE
        )
      }
    }
  }

  # ----- Subset data to required columns ---------------------------------
  if (!is.null(group_used)) {
    cols_to_keep <- unique(c(select, group_used))
  } else {
    cols_to_keep <- select
  }

  data_subset <- data[, cols_to_keep, drop = FALSE]

  # ----- Demeaning (suppress messages) -----------------------------------
  demeaned_data <- suppressMessages(
    make_demeaned(data_subset, group = group_used)
  )

  # Extract the demeaned values for the selected variable(s)
  demeaned_vals <- demeaned_data[, select, drop = FALSE]

  # ----- Prepare for plotting (remove NAs) --------------------------------
  if (length(select) == 1) {
    x <- demeaned_vals[[1]]
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      stop(
        "No non-missing observations for variable '",
        select,
        "' after demeaning.",
        call. = FALSE
      )
    }
  } else {
    # Two variables: remove rows where either is NA
    complete <- stats::complete.cases(demeaned_vals)
    if (!any(complete)) {
      stop(
        "No complete observations for variables '",
        select[1],
        "' and '",
        select[2],
        "' after demeaning.",
        call. = FALSE
      )
    }
    x <- demeaned_vals[[select[1]]][complete]
    y <- demeaned_vals[[select[2]]][complete]
  }

  # ----- Plotting (style matching plot_* functions) ----------------------
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))
  graphics::par(mar = c(5, 4, 3, 2) + 0.1, las = 1)

  if (length(select) == 1) {
    # Histogram drawn manually (no box, no title)
    h <- graphics::hist(x, plot = FALSE)
    # Determine y limits with a small padding
    y_max <- max(h$counts) * 1.02
    # Create empty plot without frame
    graphics::plot(
      NA,
      xlim = range(h$breaks),
      ylim = c(0, y_max),
      xlab = paste(select, "(demeaned)"),
      ylab = "Count",
      main = "",
      axes = FALSE,
      frame.plot = FALSE
    )
    # Draw bars
    for (i in seq_along(h$counts)) {
      if (h$counts[i] > 0) {
        graphics::rect(
          xleft = h$breaks[i],
          ybottom = 0,
          xright = h$breaks[i + 1],
          ytop = h$counts[i],
          col = colors[1],
          border = colors[2]
        )
      }
    }
    # Add axes
    graphics::axis(2, las = 1)
    graphics::axis(1)
  } else {
    # Scatterplot: no frame, no title, manual axes
    graphics::plot(
      x,
      y,
      type = "n",
      xlab = paste(select[1], "(demeaned)"),
      ylab = paste(select[2], "(demeaned)"),
      axes = FALSE,
      frame.plot = FALSE
    )
    # Add points with custom colors
    graphics::points(
      x,
      y,
      col = colors[2],
      bg = colors[1],
      pch = 21
    )
    # Add axes
    graphics::axis(1)
    graphics::axis(2, las = 1)
    # Add reference lines through origin
    graphics::abline(h = 0, v = 0, lty = 2, col = "gray")
  }

  # ----- Build return list ------------------------------------------------
  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    select = select,
    group = group_used,
    colors = colors
  )

  if (length(select) == 1) {
    details <- list(demeaned_data = x)
  } else {
    details <- list(demeaned_data = data.frame(x = x, y = y))
  }

  invisible(list(metadata = metadata, details = details))
}
