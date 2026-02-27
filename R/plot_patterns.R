#' Entities Presence Patterns Visualization
#'
#' This function creates a heatmap showing the presence/absence pattern of each entity over time.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the entity and time variables.
#'        Not required if data has panel attributes.
#' @param delta An optional positive integer giving the expected interval between time periods.
#' @param limits An integer specifying the maximum number of distinct patterns to display.
#'        If not specified, all patterns are shown.
#' @param colors A character vector of two colors for present and missing observations.
#'        Default = c("#1E4A3B", "white").
#'
#' @return Invisibly returns a list with summary statistics and metadata. Creates a heatmap.
#'
#' @details
#' An entity/time combination is considered **present** if the corresponding row contains at least
#' one non-NA value in any substantive variable (i.e., all columns except the entity and time identifiers).
#'
#' Before plotting, rows with missing values (`NA`) in the entity or time variables are removed.
#' Messages indicate how many rows were excluded. The excluded rows are stored in the returned list.
#'
#' If `delta` is supplied, the time variable is coerced to numeric (if possible). The function checks
#' for regular spacing and adds missing periods (with all zeros) to the plot. A message lists missing periods
#' unless the interval was inherited from panel attributes.
#'
#' Duplicate entity‑time combinations are checked; if found they are stored and a message is printed
#' (unless identifiers came from panel attributes).
#'
#' The heatmap shows present (color1) and missing (color2). Rows are ordered by pattern frequency:
#' the most frequent pattern is at the **top**. Within each pattern block, entities appear in their
#' original order. If `limits` is given, only the most frequent patterns are retained.
#'
#' @seealso \code{\link{describe_patterns}}, \code{\link{plot_periods}}
#'
#' @examples
#' data(production)
#' plot_patterns(production, index = c("firm", "year"))
#' plot_patterns(production, index = c("firm", "year"), limits = 3, delta = 1)
#'
#' @export
plot_patterns <- function(
  data,
  index = NULL,
  delta = NULL,
  limits = NULL,
  colors = c("#1E4A3B", "white")
) {
  # --- Initialisation ---
  user_index <- index
  user_delta <- delta
  entity_time_from_metadata <- FALSE
  delta_from_metadata <- FALSE

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
    if (is.null(delta) && !is.null(metadata$delta)) {
      delta <- metadata$delta
      delta_from_metadata <- TRUE
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
    stop("time and entity variables cannot be the same")
  }
  if (
    !is.null(limits) &&
      (!is.numeric(limits) || limits < 1 || limits != round(limits))
  ) {
    stop("'limits' must be a positive integer or NULL")
  }
  if (!is.character(colors) || length(colors) != 2) {
    stop("'colors' must be a character vector of length 2")
  }

  # --- Remove rows with NA in entity or time ---
  excluded_rows <- NULL
  na_entity <- is.na(data[[entity_var]])
  na_time <- is.na(data[[time_var]])

  if (any(na_entity)) {
    message(
      "Missing values in entity variable '",
      entity_var,
      "' found. Excluding ",
      sum(na_entity),
      " rows."
    )
  }
  if (any(na_time)) {
    message(
      "Missing values in time variable '",
      time_var,
      "' found. Excluding ",
      sum(na_time),
      " rows."
    )
  }

  if (any(na_entity | na_time)) {
    excluded_rows <- data[na_entity | na_time, , drop = FALSE]
    data <- data[!(na_entity | na_time), , drop = FALSE]
    rownames(data) <- NULL
  }

  # --- Duplicate check ---
  dup_combinations <- NULL
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
    }
  }

  # --- Delta handling ---
  if (!is.null(delta)) {
    if (
      !is.numeric(delta) ||
        length(delta) != 1 ||
        delta <= 0 ||
        delta != round(delta)
    ) {
      stop("'delta' must be a positive integer")
    }
    time_vals_orig <- data[[time_var]]
    if (!is.numeric(time_vals_orig)) {
      time_numeric <- suppressWarnings(as.numeric(as.character(time_vals_orig)))
      if (anyNA(time_numeric)) {
        stop("Cannot convert time variable '", time_var, "' to numeric.")
      }
      data[[time_var]] <- time_numeric
    }
    obs_periods <- sort(unique(data[[time_var]]))
    time_diffs <- diff(obs_periods)
    if (!all(time_diffs %% delta == 0)) {
      stop(
        "Observed time points are not evenly spaced by multiples of delta (",
        delta,
        ")."
      )
    }
    full_seq <- seq(from = min(obs_periods), to = max(obs_periods), by = delta)
    missing <- setdiff(full_seq, obs_periods)
    if (length(missing) > 0 && !delta_from_metadata) {
      message(
        "Irregular time intervals detected. Missing periods: ",
        paste(missing, collapse = ", ")
      )
    }
  }

  # --- Prepare data for heatmap ---
  data_cols <- setdiff(names(data), c(entity_var, time_var))
  if (length(data_cols) == 0) {
    stop("no data columns found (excluding entity and time)")
  }

  all_entities <- unique(as.character(data[[entity_var]]))
  if (!is.null(delta)) {
    all_times <- as.character(full_seq)
  } else {
    all_times <- unique(as.character(data[[time_var]]))
    if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", all_times))) {
      all_times <- as.character(sort(as.numeric(all_times)))
    } else {
      all_times <- sort(all_times)
    }
  }

  presence_binary <- matrix(
    0,
    nrow = length(all_entities),
    ncol = length(all_times),
    dimnames = list(all_entities, all_times)
  )

  entity_vec <- as.character(data[[entity_var]])
  time_vec <- as.character(data[[time_var]])

  has_data <- apply(data[data_cols], 1, function(row) !all(is.na(row)))
  for (i in seq_along(entity_vec)) {
    if (has_data[i] && time_vec[i] %in% all_times) {
      presence_binary[entity_vec[i], time_vec[i]] <- 1
    }
  }

  # Patterns
  pattern_strings <- apply(presence_binary, 1, paste, collapse = "")
  pattern_freq <- table(pattern_strings)

  # Full patterns_entities list
  patterns_entities_full <- list()
  for (ent in all_entities) {
    pat <- paste(presence_binary[ent, ], collapse = "")
    if (!pat %in% names(patterns_entities_full)) {
      patterns_entities_full[[pat]] <- character(0)
    }
    patterns_entities_full[[pat]] <- c(patterns_entities_full[[pat]], ent)
  }

  # Filter by limits
  if (!is.null(limits)) {
    top_patterns <- names(sort(pattern_freq, decreasing = TRUE))[
      1:min(limits, length(pattern_freq))
    ]
    keep <- pattern_strings %in% top_patterns
    presence_binary <- presence_binary[keep, , drop = FALSE]
    pattern_strings <- pattern_strings[keep]
    pattern_freq <- pattern_freq[top_patterns]
    patterns_entities_full <- patterns_entities_full[top_patterns]
  }

  # Order rows: least frequent first (bottom), most frequent last (top)
  entity_freq <- as.numeric(pattern_freq[pattern_strings])
  order_idx <- order(entity_freq, pattern_strings, rownames(presence_binary))
  presence_binary_sorted <- presence_binary[order_idx, , drop = FALSE]
  pattern_strings_sorted <- pattern_strings[order_idx]

  unique_patterns_sorted <- unique(pattern_strings_sorted[order(
    entity_freq,
    decreasing = TRUE
  )])

  patterns_entities_sorted <- list()
  for (i in seq_along(unique_patterns_sorted)) {
    pat <- unique_patterns_sorted[i]
    patterns_entities_sorted[[as.character(i)]] <- patterns_entities_full[[pat]]
  }

  patterns_matrix <- do.call(
    rbind,
    lapply(unique_patterns_sorted, function(p) as.numeric(strsplit(p, "")[[1]]))
  )
  rownames(patterns_matrix) <- seq_along(unique_patterns_sorted)
  colnames(patterns_matrix) <- all_times

  # --- Plot ---
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

  axis(1, at = 1:nc, labels = all_times, las = 2, tick = TRUE)

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

  # Build return list
  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    entity = entity_var,
    time = time_var,
    delta = delta,
    limits = limits,
    colors = colors
  )

  details <- list(
    presence_matrix = presence_binary_sorted,
    patterns_entities = patterns_entities_sorted,
    count_patterns = length(patterns_entities_sorted),
    patterns_matrix = patterns_matrix
  )
  if (!is.null(excluded_rows)) {
    details$excluded_rows <- excluded_rows
  }
  if (!is.null(dup_combinations)) {
    details$entity_time_duplicates <- dup_combinations
  }

  invisible(list(metadata = metadata, details = details))
}
