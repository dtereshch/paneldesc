#' Missing Values Heatmap by Period
#'
#' Creates a heatmap showing the number of missing values for each variable
#' across all time periods in panel data.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param select A character vector specifying which variables to include.
#'        If not specified, all substantive variables (except entity and time) are used.
#' @param index A character vector of length 2 giving the names of the entity and time variables.
#'        Not required if data has panel attributes.
#' @param colors A character vector of two colors defining the gradient for the heatmap.
#'        The first color represents the **largest** number of missing values,
#'        the second color the **smallest** number. Default = `c("#1E4A3B", "#D4B87A")`.
#'
#' @return Invisibly returns a list with summary statistics and metadata.
#'
#' @details
#' The function creates a heatmap where rows are variables and columns are time periods.
#' Cell colour reflects the number of missing values in that variable for that period,
#' using a continuous gradient from `colors[1]` (most missing) to `colors[2]` (least missing).
#' Rows are ordered as the variables appear (first at the top). Columns are ordered chronologically.
#'
#' The returned list contains:
#' \describe{
#'     \item{`metadata`}{List containing the function call, `select`, entity/time variables, and `colors`.}
#'     \item{`details`}{List with the missing count matrix (variables × periods).}
#'   }
#'
#' @note
#' The interpretation of missing counts may differ depending on whether the panel is balanced or unbalanced.
#' In a balanced panel, each time period contains the same number of entities, so the raw NA counts per period
#' are directly comparable across periods. In an unbalanced panel, the number of entities varies by period,
#' so the raw NA counts should be interpreted relative to the number of observations available in each period.
#' The function does not standardize the counts by period size; users should account for the panel structure
#' when interpreting the results.
#'
#' @seealso
#' See also [summarize_missing], [plot_patterns()], [plot_periods()].
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' plot_missing(production, index = c("firm", "year"))
#'
#' # With panel_data object
#' panel <- make_panel(production, index = c("firm", "year"))
#' plot_missing(panel)
#'
#' # Selecting specific variables
#' plot_missing(production, select = c("labor", "capital"), index = c("firm", "year"))
#'
#' # Custom colors
#' plot_missing(production, index = c("firm", "year"), colors = c("black", "white"))
#'
#' # Access the returned list
#' out_plo_mis <- plot_missing(production, index = c("firm", "year"))
#' out_plo_mis$metadata
#' out_plo_mis$details
#'
#' @export
plot_missing <- function(
  data,
  select = NULL,
  index = NULL,
  colors = c("#1E4A3B", "#D4B87A")
) {
  # --- Initialisation (similar to summarize_missing) ---
  user_index <- index
  entity_time_from_metadata <- FALSE
  msg_printed <- FALSE

  if (inherits(data, "panel_data")) {
    metadata <- attr(data, "metadata")
    if (
      is.null(metadata) || is.null(metadata$entity) || is.null(metadata$time)
    ) {
      stop(
        "Object has class 'panel_data' but missing or incomplete 'metadata' attribute."
      )
    }
    if (is.null(index)) {
      entity_var <- metadata$entity
      time_var <- metadata$time
    } else {
      if (length(index) != 2 || !is.character(index)) {
        stop("'index' must be a character vector of length 2")
      }
      entity_var <- index[1]
      time_var <- index[2]
    }
    entity_from_metadata <- is.null(user_index) && !is.null(metadata$entity)
    time_from_metadata <- is.null(user_index) && !is.null(metadata$time)
    entity_time_from_metadata <- entity_from_metadata && time_from_metadata
  } else {
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame")
    }
    if (is.null(index)) {
      stop("For regular data.frames, 'index' must be provided")
    }
    if (length(index) != 2 || !is.character(index)) {
      stop("'index' must be a character vector of length 2")
    }
    entity_var <- index[1]
    time_var <- index[2]
  }

  # Validation
  if (!is.null(select) && !is.character(select)) {
    stop("'select' must be a character vector or NULL")
  }
  if (!entity_var %in% names(data)) {
    stop('entity variable "', entity_var, '" not found in data')
  }
  if (!time_var %in% names(data)) {
    stop('time variable "', time_var, '" not found in data')
  }
  if (time_var == entity_var) {
    stop("entity and time variables cannot be the same")
  }
  if (!is.character(colors) || length(colors) != 2) {
    stop("'colors' must be a character vector of length 2")
  }

  # --- Remove rows with NA in entity or time ---
  na_entity <- is.na(data[[entity_var]])
  na_time <- is.na(data[[time_var]])

  if (any(na_entity)) {
    message(
      sum(na_entity),
      " rows with missing values in '",
      entity_var,
      "' variable found and excluded."
    )
    msg_printed <- TRUE
  }
  if (any(na_time)) {
    message(
      sum(na_time),
      " rows with missing values in '",
      time_var,
      "' variable found and excluded."
    )
    msg_printed <- TRUE
  }

  if (any(na_entity | na_time)) {
    data <- data[!(na_entity | na_time), , drop = FALSE]
    rownames(data) <- NULL
  }

  # Stop if no data left
  if (nrow(data) == 0) {
    stop("No rows remaining after removing missing entity or time values.")
  }

  # --- Duplicate check ---
  dup_rows <- duplicated(data[c(entity_var, time_var)]) |
    duplicated(data[c(entity_var, time_var)], fromLast = TRUE)
  if (any(dup_rows)) {
    dup_combinations <- unique(data[
      dup_rows,
      c(entity_var, time_var),
      drop = FALSE
    ])
    n_dup <- nrow(dup_combinations)
    if (!entity_time_from_metadata) {
      examples <- utils::head(dup_combinations, 5)
      example_strings <- paste0(
        examples[[entity_var]],
        "-",
        examples[[time_var]]
      )
      example_str <- paste(example_strings, collapse = ", ")
      message(
        n_dup,
        " duplicate entity-time combinations found. Examples: ",
        example_str
      )
      msg_printed <- TRUE
    }
  }

  # --- Determine variables to analyse ---
  if (is.null(select)) {
    analyze_vars <- setdiff(names(data), c(entity_var, time_var))
    if (length(analyze_vars) == 0) {
      stop("no variables found to analyse (besides entity and time)")
    }
    message("Analysing all variables: ", paste(analyze_vars, collapse = ", "))
    msg_printed <- TRUE
  } else {
    missing_vars <- select[!select %in% names(data)]
    if (length(missing_vars) > 0) {
      stop("variables not found: ", paste(missing_vars, collapse = ", "))
    }
    analyze_vars <- setdiff(select, c(entity_var, time_var))
    if (length(analyze_vars) == 0) {
      stop("no variables to analyse (excluding entity and time)")
    }
    # Check that entity and time are not in select (already removed, but double-check)
    if (entity_var %in% select) {
      stop("'select' cannot contain the entity variable '", entity_var, "'")
    }
    if (time_var %in% select) {
      stop("'select' cannot contain the time variable '", time_var, "'")
    }
  }

  # --- Obtain sorted time periods ---
  time_char <- as.character(data[[time_var]])
  unique_periods <- unique(time_char)
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", unique_periods))) {
    # Numeric-like: sort numerically
    ordered_periods <- as.character(sort(as.numeric(unique_periods)))
  } else {
    ordered_periods <- sort(unique_periods)
  }

  # --- Build missing count matrix (variables in rows, periods in columns) ---
  missing_mat <- matrix(
    0,
    nrow = length(analyze_vars),
    ncol = length(ordered_periods),
    dimnames = list(analyze_vars, ordered_periods)
  )

  for (i in seq_along(analyze_vars)) {
    var <- analyze_vars[i]
    for (j in seq_along(ordered_periods)) {
      period <- ordered_periods[j]
      rows_in_period <- time_char == period
      missing_mat[i, j] <- sum(is.na(data[rows_in_period, var]))
    }
  }

  # --- Prepare for plotting ---
  # Reverse colour direction: first colour = max, second colour = min
  ramp_colors <- colors[2:1] # now low -> second, high -> first
  min_val <- min(missing_mat, na.rm = TRUE)
  max_val <- max(missing_mat, na.rm = TRUE)

  # Reverse row order so first variable appears at the top of the heatmap
  missing_mat_rev <- missing_mat[nrow(missing_mat):1, , drop = FALSE]
  nr <- nrow(missing_mat_rev)
  nc <- ncol(missing_mat_rev)

  # Small padding to create whitespace around cells
  pad <- 0.2
  xlim_heat <- c(0.5 - pad, nc + 0.5 + pad)
  ylim_heat <- c(0.5 - pad, nr + 0.5 + pad)

  # Colour mapping
  if (min_val == max_val) {
    cell_colors <- matrix(colors[1], nrow = nr, ncol = nc)
    ramp_seq_colors <- rep(colors[1], 100)
  } else {
    ramp <- colorRamp(ramp_colors)
    norm_vals <- (missing_mat_rev - min_val) / (max_val - min_val)
    norm_vec <- as.vector(norm_vals)
    rgb_mat <- ramp(norm_vec)
    rgb_mat <- pmax(pmin(rgb_mat, 255), 0)
    cell_colors_vec <- rgb(
      rgb_mat[, 1],
      rgb_mat[, 2],
      rgb_mat[, 3],
      maxColorValue = 255
    )
    cell_colors <- matrix(cell_colors_vec, nrow = nr, ncol = nc)

    # Colours for the gradient legend (100 steps)
    ramp_seq <- seq(0, 1, length.out = 100)
    rgb_legend <- ramp(ramp_seq)
    ramp_seq_colors <- rgb(
      rgb_legend[, 1],
      rgb_legend[, 2],
      rgb_legend[, 3],
      maxColorValue = 255
    )
  }

  # --- Set up layout: top row for colour bar, bottom row for heatmap ---
  layout(matrix(c(1, 2), nrow = 2), heights = c(1, 6))
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    layout(1)
  })

  # ---- 1. Colour bar (legend) ----
  # Margins: bottom 0, left 4 (to match heatmap left margin), top 2, right 1
  par(mar = c(0, 4, 2, 1))
  plot.new()
  # Use the same padded x‑limits as the heatmap, so the bar has identical surrounding space
  plot.window(xlim = xlim_heat, ylim = c(0, 1), xaxs = "i", yaxs = "i")

  # Draw gradient from left edge of first column to right edge of last column
  n_rect <- 100
  x_left <- seq(0.5, nc + 0.5 - (nc) / n_rect, length.out = n_rect)
  x_right <- seq(0.5 + (nc) / n_rect, nc + 0.5, length.out = n_rect)
  for (k in seq_along(x_left)) {
    rect(
      xleft = x_left[k],
      ybottom = 0.3,
      xright = x_right[k],
      ytop = 0.7,
      col = ramp_seq_colors[k],
      border = NA
    )
  }
  # Add min/max labels at the edges of the column region (aligned with the heatmap cells)
  text(0.5, 0.15, labels = paste("Min =", min_val), adj = c(0, 0.5), cex = 0.9)
  text(
    nc + 0.5,
    0.15,
    labels = paste("Max =", max_val),
    adj = c(1, 0.5),
    cex = 0.9
  )

  # ---- 2. Heatmap ----
  # Margins: bottom 3 (for rotated labels), left 4, top 1, right 1
  par(mar = c(3, 4, 1, 1) + 0.1)
  plot(
    NA,
    xlim = xlim_heat,
    ylim = ylim_heat,
    xlab = "", # no x-axis title
    ylab = "",
    axes = FALSE,
    frame.plot = FALSE,
    xaxs = "i",
    yaxs = "i"
  )

  # Draw heatmap cells
  for (j in 1:nc) {
    for (i in 1:nr) {
      rect(
        xleft = j - 0.5,
        ybottom = i - 0.5,
        xright = j + 0.5,
        ytop = i + 0.5,
        col = cell_colors[i, j],
        border = NA
      )
    }
  }

  # X‑axis with rotated labels
  axis(
    1,
    at = 1:nc,
    labels = colnames(missing_mat_rev),
    las = 2,
    tick = TRUE,
    cex.axis = 0.8
  )

  # Y‑axis with variable names (horizontal)
  axis(
    2,
    at = 1:nr,
    labels = rownames(missing_mat_rev),
    las = 1,
    cex.axis = 0.9
  )

  if (msg_printed) {
    cat("\n")
  }

  # --- Prepare return list ---
  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    select = if (is.null(select)) analyze_vars else select,
    entity = entity_var,
    time = time_var,
    colors = colors
  )

  # Details now contains only the original missing matrix
  details <- list(
    missing_matrix = missing_mat
  )

  invisible(list(metadata = metadata, details = details))
}
