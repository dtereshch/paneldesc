#' Entities Presence Patterns Visualization
#'
#' This function creates a heatmap showing the presence/absence pattern of each entity over time.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#' @param interval An optional positive integer giving the expected interval between time periods.
#' @param max_patterns An integer specifying the maximum number of distinct patterns to display.
#'        If not specified, all patterns are shown.
#' @param colors A character vector of two colors for present and missing observations.
#'        Default = c("#1E4A3B", "white").
#'
#' @return Invisibly returns a list with summary statistics and metadata.
#'         Creates a heatmap showing presence/absence patterns.
#'
#' @details
#' An entity/time combination is considered **present** if the corresponding row contains at least
#' one non-NA value in any substantive variable (i.e., all columns except the group and time identifiers).
#'
#' If `interval` is supplied, the time variable is coerced to numeric (if possible).
#' The function then checks that all observed time points are compatible with a regular spacing
#' of that interval. If gaps are detected, a message lists the missing periods (unless the interval
#' was inherited from panel attributes), and columns for those periods are added to the presence matrix
#' (all zeros) before plotting. The heatmap will therefore include those missing periods on the x‑axis,
#' and all entities will appear as missing (color for 0) in those columns.
#'
#' The heatmap shows:
#' \itemize{
#'   \item \strong{Present}: Entity is present in the time period
#'   \item \strong{Missing}: Entity is absent in the time period
#' }
#'
#' Rows are ordered by pattern frequency: the most frequent pattern is at the **top**.
#' Within each pattern block, entities appear in their original order (as they first occur in the data).
#' If `max_patterns` is given, only the most frequent patterns are retained.
#'
#' The returned list contains the following components:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing additional information: `count_patterns`, `presence_matrix`,
#'         `pattern_groups`. The `pattern_groups` element is a list where each element corresponds
#'         to a pattern rank and contains the entity IDs that follow that pattern.}
#' }
#'
#' @seealso
#' [describe_patterns()], [plot_periods()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' plot_patterns(production, group = "firm", time = "year")
#'
#' # Only top 3 patterns
#' plot_patterns(production, group = "firm", time = "year", max_patterns = 3)
#'
#' # Specify interval to fill gaps
#' plot_patterns(production, group = "firm", time = "year", interval = 1)
#'
#' # Custom colors
#' plot_patterns(production, group = "firm", time = "year", colors = c("black", "white"))
#'
#' @export
plot_patterns <- function(
  data,
  group = NULL,
  time = NULL,
  interval = NULL,
  max_patterns = NULL,
  colors = c("#1E4A3B", "white")
) {
  # Capture original interval argument
  user_interval <- interval

  # --- Panel attribute handling and validation ---
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
    if (is.null(interval) && !is.null(metadata$interval)) {
      interval <- metadata$interval
    }
  } else {
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame, not ", class(data)[1])
    }
    if (is.null(group) || is.null(time)) {
      stop(
        "For regular data.frames, both 'group' and 'time' arguments must be provided"
      )
    }
  }

  # Determine if interval came from metadata
  interval_from_metadata <- is.null(user_interval) && !is.null(interval)

  # --- Basic checks ---
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }
  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string, not ", class(time)[1])
  }
  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }
  if (!time %in% names(data)) {
    stop('variable "', time, '" not found in data')
  }
  if (!is.null(max_patterns)) {
    if (
      !is.numeric(max_patterns) || length(max_patterns) != 1 || max_patterns < 1
    ) {
      stop("'max_patterns' must be a single positive integer or NULL")
    }
  }
  if (!is.character(colors) || length(colors) != 2) {
    stop(
      "'colors' must be a character vector of length 2, not ",
      class(colors)[1]
    )
  }

  # --- Interval handling ---
  if (!is.null(interval)) {
    if (
      !is.numeric(interval) ||
        length(interval) != 1 ||
        interval <= 0 ||
        interval != round(interval)
    ) {
      stop("'interval' must be a positive integer")
    }
    # Coerce time to numeric if needed
    time_vals_orig <- data[[time]]
    if (!is.numeric(time_vals_orig)) {
      time_numeric <- suppressWarnings(as.numeric(as.character(time_vals_orig)))
      if (anyNA(time_numeric)) {
        stop(
          "Cannot convert the time variable '",
          time,
          "' to numeric. ",
          "Please ensure it contains numbers or convert it manually."
        )
      }
      data[[time]] <- time_numeric
    }

    # Check consistency
    obs_periods <- sort(unique(data[[time]]))
    time_diffs <- diff(obs_periods)
    if (!all(time_diffs %% interval == 0)) {
      stop(
        "The observed time points are not evenly spaced by multiples of the specified interval (",
        interval,
        "). ",
        "For example, differences such as ",
        paste(unique(time_diffs[time_diffs %% interval != 0]), collapse = ", "),
        " are not multiples of ",
        interval,
        "."
      )
    }

    # Compute full sequence and missing periods
    full_seq <- seq(
      from = min(obs_periods),
      to = max(obs_periods),
      by = interval
    )
    missing <- setdiff(full_seq, obs_periods)
    if (length(missing) > 0 && !interval_from_metadata) {
      message(
        "Irregular time intervals detected. Missing periods: ",
        paste(missing, collapse = ", ")
      )
    }
  }

  # --- Identify data columns (excluding group and time) ---
  data_cols <- setdiff(names(data), c(group, time))
  if (length(data_cols) == 0) {
    stop("no data columns found (excluding group and time variables)")
  }

  # --- Get all entities and time periods ---
  all_groups <- unique(as.character(data[[group]]))
  if (!is.null(interval)) {
    # Use full_seq (including missing) as time columns
    all_times <- as.character(full_seq)
  } else {
    all_times <- unique(as.character(data[[time]]))
    # Sort time periods if they appear numeric
    if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", all_times))) {
      all_times <- as.character(sort(as.numeric(all_times)))
    } else {
      all_times <- sort(all_times)
    }
  }
  time_cols <- all_times

  # --- Create binary presence matrix using "observed" definition ---
  presence_binary <- matrix(
    0,
    nrow = length(all_groups),
    ncol = length(all_times),
    dimnames = list(all_groups, all_times)
  )

  group_vec <- as.character(data[[group]])
  time_vec <- as.character(data[[time]])

  has_at_least_one_non_na <- apply(data[data_cols], 1, function(row) {
    !all(is.na(row))
  })
  for (i in seq_along(group_vec)) {
    if (has_at_least_one_non_na[i] && time_vec[i] %in% all_times) {
      presence_binary[group_vec[i], time_vec[i]] <- 1
    }
  }

  # --- Compute pattern for each entity ---
  pattern_strings <- apply(presence_binary, 1, paste, collapse = "")

  # --- Frequency of each pattern ---
  pattern_freq <- table(pattern_strings)

  # --- Create pattern_groups list (before filtering) ---
  pattern_groups_full <- list()
  for (entity in all_groups) {
    pattern_vec <- presence_binary[entity, ]
    pattern_string <- paste(pattern_vec, collapse = "")

    if (!pattern_string %in% names(pattern_groups_full)) {
      pattern_groups_full[[pattern_string]] <- character(0)
    }
    pattern_groups_full[[pattern_string]] <- c(
      pattern_groups_full[[pattern_string]],
      entity
    )
  }

  # --- Filter by max_patterns if requested ---
  if (!is.null(max_patterns)) {
    top_patterns <- names(sort(pattern_freq, decreasing = TRUE))[
      1:min(max_patterns, length(pattern_freq))
    ]
    keep <- pattern_strings %in% top_patterns
    presence_binary <- presence_binary[keep, , drop = FALSE]
    pattern_strings <- pattern_strings[keep]
    pattern_freq <- pattern_freq[top_patterns]

    pattern_groups_full <- pattern_groups_full[top_patterns]
  }

  # --- Order rows: least frequent first (bottom), most frequent last (top) ---
  entity_freq <- as.numeric(pattern_freq[pattern_strings])
  order_idx <- order(entity_freq, pattern_strings, rownames(presence_binary))
  presence_binary_sorted <- presence_binary[order_idx, , drop = FALSE]
  pattern_strings_sorted <- pattern_strings[order_idx]

  unique_patterns_sorted <- unique(pattern_strings_sorted[order(
    entity_freq,
    decreasing = TRUE
  )])

  pattern_groups_sorted <- list()
  for (i in seq_along(unique_patterns_sorted)) {
    pattern_string <- unique_patterns_sorted[i]
    pattern_groups_sorted[[as.character(i)]] <- pattern_groups_full[[
      pattern_string
    ]]
  }

  # --- Prepare for plotting ---
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mar = c(3, 1, 2.5, 1) + 0.1)

  presence_rev <- 1 - presence_binary_sorted
  nr <- nrow(presence_rev)
  nc <- ncol(presence_rev)

  plot(
    NA,
    xlim = c(0.5, nc + 0.5),
    ylim = c(-0.2, nr + 1.2),
    xlab = "",
    ylab = "",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i"
  )

  for (i in 1:nc) {
    for (j in 1:nr) {
      rect(
        xleft = i - 0.5,
        ybottom = j - 0.5,
        xright = i + 0.5,
        ytop = j + 0.5,
        col = colors[presence_rev[j, i] + 1],
        border = NA
      )
    }
  }

  axis(1, at = 1:nc, labels = time_cols, las = 2, tick = TRUE)

  runs <- rle(pattern_strings_sorted)
  if (length(runs$lengths) > 1) {
    boundaries <- cumsum(runs$lengths)[-length(runs$lengths)]
    segments(
      x0 = 0.5,
      x1 = nc + 0.5,
      y0 = boundaries + 0.5,
      y1 = boundaries + 0.5,
      col = "white",
      lty = 1,
      lwd = 0.8
    )
  }

  legend(
    "top",
    legend = c("Present", "Missing"),
    fill = colors,
    bg = "white",
    horiz = TRUE,
    xpd = TRUE,
    bty = "n",
    inset = c(0, -0.04),
    cex = 0.9
  )

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time,
    interval = interval,
    max_patterns = max_patterns,
    colors = colors
  )

  details <- list(
    presence_matrix = presence_binary_sorted,
    pattern_groups = pattern_groups_sorted,
    count_patterns = length(pattern_groups_sorted)
  )

  invisible(list(
    metadata = metadata,
    details = details
  ))
}
