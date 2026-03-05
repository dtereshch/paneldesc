#' Time Coverage Distribution Visualization
#'
#' This function calculates summary statistics and creates a histogram showing
#' the distribution of time periods covered by each entity in panel data.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the entity and time variables.
#'        Not required if data has panel attributes.
#' @param colors A character vector of length 2 specifying the fill color and line color for the histogram.
#'        First color is for fill, second color is for the border line. Default = c("#1E4A3B", "white").
#'
#' @return Invisibly returns a list with summary statistics and metadata.
#'
#' @details
#' The function creates a histogram of the number of time periods covered by each entity.
#' The x‑axis shows coverage (periods per entity), the y‑axis shows the count of entities.
#'
#' The returned list contains:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List with the coverage vector per entity and the histogram data used for plotting.}
#' }
#'
#' @note
#' An entity/time combination is considered **present** if the corresponding row contains at least
#' one non‑NA value in any substantive variable (all columns except the entity and time identifiers).
#'
#' Before plotting, rows with missing values (`NA`) in the entity or time variables are removed.
#' Messages indicate how many rows were excluded.
#'
#' Duplicate entity‑time combinations are checked; if found, a message is printed
#' (unless the identifiers came from panel attributes).
#'
#' @seealso
#' See also [describe_periods()], [plot_patterns()].
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' plot_periods(production, index = c("firm", "year"))
#'
#' # With panel_data object
#' panel <- make_panel(production, index = c("firm", "year"))
#' plot_periods(panel)
#'
#' # Custom colors
#' plot_periods(production, index = c("firm", "year"), colors = c("gray", "black"))
#'
#' # Accessing list components
#' out_plo_per <- plot_periods(production, index = c("firm", "year"))
#' out_plo_per$metadata
#' out_plo_par$details
#'
#' @export
plot_periods <- function(
  data,
  index = NULL,
  colors = c("#1E4A3B", "white")
) {
  # --- Initialisation ---
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
  if (!entity_var %in% names(data)) {
    stop('variable "', entity_var, '" not found in data')
  }
  if (!time_var %in% names(data)) {
    stop('variable "', time_var, '" not found in data')
  }
  if (time_var == entity_var) {
    stop("entity and time variables cannot be the same")
  }
  if (!is.character(colors) || length(colors) != 2) {
    stop("'colors' must be a character vector of length 2")
  }

  fill_color <- colors[1]
  line_color <- colors[2]

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

  # --- Calculate coverage ---
  substantive_vars <- setdiff(names(data), c(entity_var, time_var))
  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides entity and time variables)")
  }

  has_data <- apply(data[substantive_vars], 1, function(x) any(!is.na(x)))
  filtered_data <- data[has_data, ]

  group_var <- as.character(filtered_data[[entity_var]])
  time_var_ch <- as.character(filtered_data[[time_var]])

  unique_groups <- unique(group_var)
  unique_times <- unique(time_var_ch)

  if (all(grepl("^-?\\d+\\.?\\d*$", unique_times))) {
    time_order <- order(as.numeric(unique_times))
  } else {
    time_order <- order(unique_times)
  }
  ordered_times <- unique_times[time_order]

  presence_mat <- matrix(
    0,
    nrow = length(unique_groups),
    ncol = length(ordered_times),
    dimnames = list(unique_groups, ordered_times)
  )

  for (i in seq_along(group_var)) {
    presence_mat[group_var[i], time_var_ch[i]] <- 1
  }

  time_coverage <- rowSums(presence_mat)
  names(time_coverage) <- rownames(presence_mat)

  # Histogram data
  coverage_table <- table(time_coverage)
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

  # --- Plot ---
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(5, 4, 3, 2) + 0.1, las = 1)

  x_label <- paste("Time coverage by", entity_var)
  x_min <- max(1, min(time_coverage))
  x_max <- max(time_coverage)
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
      rect(
        xleft = hist_data$mids[i] - 0.5,
        ybottom = 0,
        xright = hist_data$mids[i] + 0.5,
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

  # Return list
  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    entity = entity_var,
    time = time_var,
    colors = colors
  )

  details <- list(
    coverage_by_entity = time_coverage,
    histogram_data = hist_data
  )

  if (msg_printed) {
    cat("\n")
  }

  invisible(list(metadata = metadata, details = details))
}
