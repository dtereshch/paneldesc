#' Entities Presence Patterns Visualization
#'
#' This function creates a heatmap showing the presence/absence pattern of each entity
#' over time. Rows (entities) are sorted so that the most frequent pattern appears at the top.
#' Horizontal lines separate blocks of identical patterns.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#' @param presence A character string specifying how to define entity presence: "nominal", "observed", or "complete".
#'        Default = "observed".
#' @param max_patterns An integer specifying the maximum number of distinct patterns to display.
#'        If not specified, all patterns are shown.
#' @param colors A character vector of two colors for present and missing observations.
#'        Default = c("#1E4A3B", "white").
#'
#' @return Invisibly returns the presence matrix (entities × time periods) after sorting and filtering,
#'         which can be used for further inspection. Creates a heatmap plot.
#'
#' @details
#' \strong{Presence} parameter definitions:
#' \describe{
#'   \item{\code{"nominal"}}{Entity is present if it has a row in the data (even with only panel ID variables)}
#'   \item{\code{"observed"}}{Entity is present if it has at least one non-NA substantive variable (default)}
#'   \item{\code{"complete"}}{Entity is present only if it has no NA values in all substantive variables}
#' }
#'
#' The heatmap shows:
#' \itemize{
#'   \item \strong{Present}: Entity is present in the time period (based on the specified presence type)
#'   \item \strong{Missing}: Entity is absent in the time period
#' }
#'
#' Rows are ordered by pattern frequency: the most frequent pattern is at the **top**.
#' Within each pattern block, entities appear in their original order (as they first occur in the data).
#' If `max_patterns` is given, only the most frequent patterns are retained.
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
#' # Custom colors
#' plot_patterns(production, group = "firm", time = "year", colors = c("black", "white"))
#'
#' @export
plot_patterns <- function(
  data,
  group = NULL,
  time = NULL,
  presence = "observed",
  max_patterns = NULL,
  colors = c("#1E4A3B", "white")
) {
  # --- Panel attribute handling and validation (unchanged) ---
  has_panel_attrs <- !is.null(attr(data, "panel_group")) &&
    !is.null(attr(data, "panel_time"))

  if (has_panel_attrs) {
    group <- attr(data, "panel_group")
    time <- attr(data, "panel_time")
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

  # --- Basic checks ---
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }
  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string, not ", class(time)[1])
  }
  if (!is.character(presence) || length(presence) != 1) {
    stop(
      "'presence' must be a single character string, not ",
      class(presence)[1]
    )
  }
  if (!presence %in% c("observed", "nominal", "complete")) {
    stop('presence must be one of: "observed", "nominal", "complete"')
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

  # --- Identify data columns (excluding group and time) ---
  data_cols <- setdiff(names(data), c(group, time))
  if (length(data_cols) == 0) {
    stop("no data columns found (excluding group and time variables)")
  }

  # --- Get all entities and time periods ---
  all_groups <- unique(as.character(data[[group]]))
  all_times <- unique(as.character(data[[time]]))

  # Sort time periods if they appear numeric
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", all_times))) {
    all_times <- as.character(sort(as.numeric(all_times)))
  } else {
    all_times <- sort(all_times)
  }
  time_cols <- all_times

  # --- Create binary presence matrix ---
  presence_binary <- matrix(
    0,
    nrow = length(all_groups),
    ncol = length(all_times),
    dimnames = list(all_groups, all_times)
  )

  group_vec <- as.character(data[[group]])
  time_vec <- as.character(data[[time]])

  if (presence == "nominal") {
    for (i in seq_along(group_vec)) {
      presence_binary[group_vec[i], time_vec[i]] <- 1
    }
  } else if (presence == "observed") {
    has_at_least_one_non_na <- apply(data[data_cols], 1, function(row) {
      !all(is.na(row))
    })
    for (i in seq_along(group_vec)) {
      if (has_at_least_one_non_na[i]) {
        presence_binary[group_vec[i], time_vec[i]] <- 1
      }
    }
  } else {
    # complete
    complete_rows <- complete.cases(data[data_cols])
    for (i in seq_along(group_vec)) {
      if (complete_rows[i]) {
        presence_binary[group_vec[i], time_vec[i]] <- 1
      }
    }
  }

  # --- Compute pattern for each entity ---
  pattern_strings <- apply(presence_binary, 1, paste, collapse = "")

  # --- Frequency of each pattern ---
  pattern_freq <- table(pattern_strings)

  # --- Filter by max_patterns if requested ---
  if (!is.null(max_patterns)) {
    # Identify the most frequent patterns
    top_patterns <- names(sort(pattern_freq, decreasing = TRUE))[
      1:min(max_patterns, length(pattern_freq))
    ]
    keep <- pattern_strings %in% top_patterns
    presence_binary <- presence_binary[keep, , drop = FALSE]
    pattern_strings <- pattern_strings[keep]
    pattern_freq <- pattern_freq[top_patterns] # keep only top for ordering
  }

  # --- Order rows: least frequent first (bottom), most frequent last (top) ---
  # Create a vector of frequencies for each entity
  entity_freq <- as.numeric(pattern_freq[pattern_strings])

  # Order: first by frequency ascending (so most frequent ends last), then by pattern string,
  # then by group name to have deterministic order within pattern.
  order_idx <- order(entity_freq, pattern_strings, rownames(presence_binary))
  presence_binary_sorted <- presence_binary[order_idx, , drop = FALSE]
  pattern_strings_sorted <- pattern_strings[order_idx]

  # --- Prepare for plotting ---
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Set margins only – asp is now in plot()
  par(mar = c(3, 1, 2.5, 1) + 0.1)

  # Reverse 0/1 for color mapping: 1 -> missing, 0 -> present
  presence_rev <- 1 - presence_binary_sorted
  nr <- nrow(presence_rev)
  nc <- ncol(presence_rev)

  # Create empty plot with expanded y-limits to create gap below heatmap
  plot(
    NA,
    xlim = c(0.5, nc + 0.5), # Cell centers at 1:nc, so edges at 0.5 and nc+0.5
    ylim = c(-0.2, nr + 1.2), # Gap below and above heatmap
    xlab = "",
    ylab = "",
    axes = FALSE,
    xaxs = "i", # No auto-expansion, we control limits
    yaxs = "i" # No auto-expansion, we control limits
  )

  # Add the heatmap using rect() for better control
  for (i in 1:nc) {
    for (j in 1:nr) {
      rect(
        xleft = i - 0.5,
        ybottom = j - 0.5,
        xright = i + 0.5,
        ytop = j + 0.5,
        col = colors[presence_rev[j, i] + 1], # +1 because colors[1] is for 0, colors[2] for 1
        border = NA
      )
    }
  }

  # Add x-axis with time labels (vertical)
  axis(1, at = 1:nc, labels = time_cols, las = 2, tick = TRUE)

  # --- Add horizontal grid lines between patterns, confined to cell area ---
  runs <- rle(pattern_strings_sorted)
  if (length(runs$lengths) > 1) {
    boundaries <- cumsum(runs$lengths)[-length(runs$lengths)] # last row of each pattern (except last)
    # Draw lines from left edge (0.5) to right edge (nc + 0.5) at y = boundary + 0.5
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

  # Legend at the top - SLIGHTLY INCREASED space between legend and main plot
  legend(
    "top",
    legend = c("Present", "Missing"),
    fill = colors,
    bg = "white",
    horiz = TRUE,
    xpd = TRUE,
    bty = "n",
    inset = c(0, -0.04), # SLIGHTLY INCREASED from -0.02 to -0.04 (more negative = more space above plot)
    cex = 0.9
  )

  # Invisibly return the sorted presence matrix
  invisible(presence_binary_sorted)
}
