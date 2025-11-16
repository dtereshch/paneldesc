#' Draws a heatmap showing participation patterns in an unbalanced panel
#'
#' Creates a publication-ready heatmap visualization of panel data structure,
#' showing participation patterns by entity and time period. Supports regular data frames.
#'
#' @param data The data frame
#' @param group Character string specifying the entities' identifier column name
#' @param time Character string specifying the time identifier column name
#' @param main Plot title (default: "Participation Patterns")
#' @param xlab X-axis label (default: "Time Period")
#' @param ylab Y-axis label (default: NULL - no y-axis label)
#' @param show_stats Logical, whether to display summary statistics on plot (default: TRUE)
#' @param colors Vector of two colors for present/missing observations
#'               (default: c("#0072B2", "#D55E00") - blue/orange colorblind-friendly)
#' @param cex.axis Axis text size (default: 0.8)
#' @param cex.lab Label text size (default: 1)
#' @param cex.pattern Pattern label text size (default: 0.7)
#' @param mar Plot margins (default: c(5, 8, 4, 6) + 0.1)  # Increased right margin for legend
#'
#' @return Invisible list containing the cross-tabulation matrix and summary statistics
#'
#' @examples
#' # Load the production dataset
#' data(production)
#'
#' # Basic usage with production data
#' plot_participation(production, group = "firm", time = "year")
#'
#' # With custom options
#' plot_participation(production, group = "firm", time = "year",
#'                main = "Production Participation Patterns",
#'                colors = c("darkgreen", "orange"),
#'                show_stats = TRUE)
#'
#' @export
plot_participation <- function(
  data,
  group = NULL,
  time = NULL,
  main = "Participation Patterns",
  xlab = "Time Period",
  ylab = NULL,
  show_stats = TRUE,
  colors = c("#0072B2", "#D55E00"),
  cex.axis = 0.8,
  cex.lab = 1,
  cex.pattern = 0.7,
  mar = c(5, 8, 4, 6) + 0.1 # Increased right margin for vertical legend
) {
  # Input validation
  if (missing(data)) {
    stop("Argument 'data' is required")
  }

  if (is.null(group) || is.null(time)) {
    stop("Both 'group' and 'time' arguments are required as character strings")
  }

  if (
    !is.character(group) ||
      !is.character(time) ||
      length(group) != 1 ||
      length(time) != 1
  ) {
    stop("'group' and 'time' must be single character strings")
  }

  data_df <- as.data.frame(data)
  group_var <- data_df[[group]]
  time_var <- data_df[[time]]

  if (is.null(group_var)) {
    stop("Group variable '", group, "' not found in data")
  }
  if (is.null(time_var)) {
    stop("Time variable '", time, "' not found in data")
  }

  # Filter out rows with all NAs in non-group/time variables
  other_vars <- setdiff(names(data_df), c(group, time))
  if (length(other_vars) > 0) {
    has_data <- apply(data_df[other_vars], 1, function(x) any(!is.na(x)))
    data_df <- data_df[has_data, ]
    group_var <- data_df[[group]]
    time_var <- data_df[[time]]

    if (nrow(data_df) == 0) {
      stop("No observations with valid data after removing rows with all NAs")
    }
  }

  # Convert to character and get unique values
  group_var <- as.character(group_var)
  time_var <- as.character(time_var)
  unique_groups <- unique(group_var)
  unique_times <- unique(time_var)

  # Create presence matrix
  presence_matrix <- matrix(
    0,
    nrow = length(unique_groups),
    ncol = length(unique_times),
    dimnames = list(unique_groups, unique_times)
  )

  # Fill matrix with presence indicators
  obs_indices <- cbind(
    match(group_var, unique_groups),
    match(time_var, unique_times)
  )
  presence_matrix[obs_indices] <- 1

  # Group entities by missing value patterns
  pattern_strings <- apply(presence_matrix, 1, paste, collapse = "")
  pattern_groups <- split(rownames(presence_matrix), pattern_strings)

  # Create pattern matrix
  pattern_matrix <- do.call(
    rbind,
    lapply(names(pattern_groups), function(pat) {
      as.numeric(strsplit(pat, "")[[1]])
    })
  )
  rownames(pattern_matrix) <- paste0("Pattern ", seq_along(pattern_groups))
  colnames(pattern_matrix) <- colnames(presence_matrix)

  # Calculate pattern statistics
  pattern_counts <- lengths(pattern_groups)
  pattern_pcts <- pattern_counts / length(unique_groups) * 100

  # Order patterns by frequency (most common first)
  pattern_order <- order(pattern_counts, decreasing = TRUE)
  pattern_matrix <- pattern_matrix[pattern_order, , drop = FALSE]
  pattern_counts <- pattern_counts[pattern_order]
  pattern_pcts <- pattern_pcts[pattern_order]
  pattern_groups <- pattern_groups[pattern_order]

  # Calculate summary statistics
  total_cells <- nrow(presence_matrix) * ncol(presence_matrix)
  missing_obs <- total_cells - sum(presence_matrix)

  stats <- list(
    n_entities = length(unique_groups),
    n_periods = ncol(presence_matrix),
    total_obs = sum(presence_matrix),
    missing_obs = missing_obs,
    pct_missing = missing_obs / total_cells * 100,
    entities_with_gaps = sum(rowSums(presence_matrix) < ncol(presence_matrix)),
    n_patterns = length(pattern_groups)
  )

  # Order time periods numerically if possible
  if (all(grepl("^-?\\d+\\.?\\d*$", unique_times))) {
    time_order <- order(as.numeric(unique_times))
  } else {
    time_order <- order(unique_times)
  }
  ordered_matrix <- pattern_matrix[, time_order, drop = FALSE]

  # Set up plot
  old_mar <- par("mar")
  on.exit(par(mar = old_mar))
  par(mar = mar)

  # Reverse matrix for plotting (most common pattern at top)
  plot_matrix <- ordered_matrix[nrow(ordered_matrix):1, , drop = FALSE]

  # Use colors directly without reversal - ensure correct mapping
  col_palette <- colors # Present = colors[1], Missing = colors[2]

  # Create heatmap with explicit breaks to ensure correct color mapping
  image(
    1:ncol(plot_matrix),
    1:nrow(plot_matrix),
    t(plot_matrix),
    col = col_palette,
    xlab = "",
    ylab = "",
    main = main,
    axes = FALSE,
    cex.lab = cex.lab,
    useRaster = TRUE,
    breaks = c(-0.5, 0.5, 1.5) # Explicit breaks for correct color assignment
  )

  # Add axes
  axis(
    1,
    at = 1:ncol(plot_matrix),
    labels = colnames(plot_matrix),
    las = 2,
    cex.axis = cex.axis
  )

  pattern_labels <- sprintf(
    "Pattern %d (n=%d, %.1f%%)",
    seq_along(pattern_counts),
    pattern_counts,
    pattern_pcts
  )
  axis(
    2,
    at = 1:nrow(plot_matrix),
    labels = pattern_labels[nrow(plot_matrix):1],
    las = 1,
    cex.axis = cex.pattern
  )

  title(xlab = xlab, cex.lab = cex.lab, line = 3.5)

  # Add grid
  abline(h = 1:nrow(plot_matrix) - 0.5, col = "gray80", lty = 3)
  abline(v = 1:ncol(plot_matrix) - 0.5, col = "gray80", lty = 3)

  # Add statistics if requested
  if (show_stats) {
    stats_text <- sprintf(
      "Entities: %d | Time periods: %d | Observations: %d | Missing: %d (%.1f%%) | Patterns: %d",
      stats$n_entities,
      stats$n_periods,
      stats$total_obs,
      stats$missing_obs,
      stats$pct_missing,
      stats$n_patterns
    )
    mtext(stats_text, side = 3, line = 0.5, cex = 0.7 * cex.lab)
  }

  # Add vertical legend on the right
  legend(
    "right",
    legend = c("Present", "Missing"),
    fill = colors,
    bty = "n",
    cex = 0.8,
    title = "Observation Status",
    inset = c(-0.15, 0),
    xpd = TRUE
  )

  # Return results
  invisible(list(
    presence_matrix = presence_matrix,
    pattern_matrix = ordered_matrix,
    pattern_groups = pattern_groups,
    pattern_stats = data.frame(
      pattern_id = seq_along(pattern_counts),
      pattern_string = names(pattern_groups),
      n_entities = pattern_counts,
      percent_entities = pattern_pcts,
      entities = I(pattern_groups)
    ),
    statistics = stats,
    group_var = group,
    time_var = time,
    color_mapping = c("1" = colors[1], "0" = colors[2]) # Document the mapping
  ))
}
