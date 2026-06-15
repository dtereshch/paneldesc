#' Plot Demeaned Variable(s)
#'
#' This function demeaens the selected numeric variable(s) using the same
#' within-group demeaning procedure as \code{\link{make_demeaned}} and creates
#' a visualization. For a single variable, a histogram of the demeaned values
#' is drawn. For two variables, a scatterplot of the demeaned values is drawn.
#'
#' @param data A data.frame containing the variable(s) to be demeaned.
#' @param select A character vector of length 1 or 2 specifying the name(s) of
#'        the numeric variable(s) to demean and plot.
#' @param group A character vector specifying the grouping variable(s) for
#'        demeaning. If not specified and \code{data} has panel attributes,
#'        the entity and time variables are used as grouping variables.
#'        Otherwise, overall demeaning (grand mean centering) is performed.
#' @param colors A character vector of length 2 specifying the colours for the
#'        plot. For a histogram, the first colour fills the bars and the second
#'        colour is used for the bar borders. For a scatterplot, the first colour
#'        fills the points and the second colour is used for the point borders.
#'        Default = \code{c("darkblue", "white")}.
#'
#' @return Invisibly returns a list with the following components:
#' \describe{
#'   \item{\code{metadata}}{List containing the function name, the selected
#'         variable(s), the grouping variable(s) used, and the colours.}
#'   \item{\code{details}}{List containing the demeaned data. For a single
#'         variable, this is a numeric vector of demeaned values. For two
#'         variables, it is a data frame with columns \code{x} and \code{y}
#'         containing the demeaned values.}
#' }
#'
#' @details
#' The demeaning is performed by calling \code{\link{make_demeaned}} internally.
#' Therefore all features of that function are available: handling of
#' \code{panel_data} objects, iterative projection for multiple groups,
#' and removal of rows with missing values in the grouping variables.
#'
#' Missing values in the selected variable(s) are removed before plotting.
#' If no valid observations remain after removal, an error is thrown.
#'
#' Only the selected variable(s) and the grouping variables are passed to
#' \code{make_demeaned}, so no other numeric variables in the original data
#' are demeaned or cause message output.
#'
#' @seealso
#' See also [make_demeaned()], [plot_heterogeneity()], [decompose_numeric()]
#'
#' @examples
#' data(production)
#'
#' # Histogram of demeaned labor (overall demeaning)
#' plot_demeaned(production, select = "labor")
#'
#' # Histogram of labor demeaned within firms
#' plot_demeaned(production, select = "labor", group = "firm")
#'
#' # Scatterplot of demeaned labor vs. demeaned capital (by firm and year)
#' plot_demeaned(production, select = c("labor", "capital"),
#'               group = c("firm", "year"))
#'
#' # Using a panel_data object (automatically demeans by firm and year)
#' panel <- make_panel(production, index = c("firm", "year"))
#' plot_demeaned(panel, select = c("labor", "capital"))
#'
#' # Custom colours
#' plot_demeaned(production, select = "labor", group = "firm",
#'               colors = c("gray", "black"))
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
        "No non‑missing observations for variable '",
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
    # Add points with custom colours
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
