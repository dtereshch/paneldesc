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
#' @param mar Plot margins (default: c(8, 8, 4, 2) + 0.1)  # Increased bottom margin for legend
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
  mar = c(8, 8, 4, 2) + 0.1 # Increased bottom margin for legend
) {
  # Input validation
  if (missing(data)) {
    stop("Argument 'data' is required")
  }

  # Regular data frame - require group and time arguments as character strings
  if (is.null(group) || is.null(time)) {
    stop(
      "For regular data frames, both 'group' and 'time' arguments are required as character strings"
    )
  }

  if (!is.character(group) || !is.character(time)) {
    stop(
      "For regular data frames, 'group' and 'time' arguments must be character strings"
    )
  }

  if (length(group) != 1 || length(time) != 1) {
    stop("'group' and 'time' must be single character strings")
  }

  data_df <- as.data.frame(data)
  group_name <- group
  time_name <- time

  # Extract variables
  group_var <- data_df[[group_name]]
  time_var <- data_df[[time_name]]

  if (is.null(group_var)) {
    stop(
      "Group variable '",
      group_name,
      "' not found in data. Available variables: ",
      paste(names(data_df), collapse = ", ")
    )
  }
  if (is.null(time_var)) {
    stop(
      "Time variable '",
      time_name,
      "' not found in data. Available variables: ",
      paste(names(data_df), collapse = ", ")
    )
  }

  # Filter out rows that have NAs in all columns except group and time variables
  other_vars <- setdiff(names(data_df), c(group_name, time_name))
  if (length(other_vars) > 0) {
    # Keep rows that have at least one non-NA value in the other variables
    has_data <- apply(data_df[other_vars], 1, function(x) any(!is.na(x)))
    data_df <- data_df[has_data, ]
    group_var <- data_df[[group_name]]
    time_var <- data_df[[time_name]]

    if (nrow(data_df) == 0) {
      stop("No observations with valid data after removing rows with all NAs")
    }
  }

  # Convert to character to handle different classes uniformly
  group_var <- as.character(group_var)
  time_var <- as.character(time_var)

  # Create cross-tabulation
  unique_groups <- unique(group_var)
  unique_times <- unique(time_var)

  # Create presence matrix (1 = present, 0 = missing)
  presence_matrix <- matrix(
    0,
    nrow = length(unique_groups),
    ncol = length(unique_times),
    dimnames = list(unique_groups, unique_times)
  )

  # Fill matrix with 1 for present observations
  for (i in seq_along(group_var)) {
    row_idx <- which(unique_groups == group_var[i])
    col_idx <- which(unique_times == time_var[i])
    presence_matrix[row_idx, col_idx] <- 1
  }

  # Group entities by missing value patterns
  pattern_strings <- apply(presence_matrix, 1, function(x) {
    paste(x, collapse = "")
  })
  pattern_groups <- split(rownames(presence_matrix), pattern_strings)

  # Create pattern matrix with one row per pattern
  # FIX: Ensure pattern_matrix is always a matrix, even with single pattern
  if (length(pattern_groups) == 1) {
    pattern_matrix <- matrix(
      as.numeric(strsplit(names(pattern_groups)[1], "")[[1]]),
      nrow = 1,
      ncol = ncol(presence_matrix)
    )
  } else {
    pattern_matrix <- matrix(
      0,
      nrow = length(pattern_groups),
      ncol = ncol(presence_matrix)
    )
  }

  rownames(pattern_matrix) <- paste0(
    "Pattern ",
    seq_len(length(pattern_groups))
  )
  colnames(pattern_matrix) <- colnames(presence_matrix)

  # Fill pattern matrix and count entities per pattern
  pattern_counts <- numeric(length(pattern_groups))
  pattern_pcts <- numeric(length(pattern_groups))
  total_entities <- length(unique_groups)

  for (i in seq_along(pattern_groups)) {
    if (length(pattern_groups) == 1) {
      # Already filled above for single pattern case
      pattern <- pattern_matrix[i, ]
    } else {
      pattern <- as.numeric(strsplit(names(pattern_groups)[i], "")[[1]])
      pattern_matrix[i, ] <- pattern
    }
    pattern_counts[i] <- length(pattern_groups[[i]])
    pattern_pcts[i] <- pattern_counts[i] / total_entities * 100
  }

  # Order patterns by frequency (most common first)
  pattern_order <- order(pattern_counts, decreasing = TRUE)
  pattern_matrix <- pattern_matrix[pattern_order, , drop = FALSE] # FIX: Added drop = FALSE
  pattern_counts <- pattern_counts[pattern_order]
  pattern_pcts <- pattern_pcts[pattern_order]
  pattern_groups <- pattern_groups[pattern_order]

  # Calculate summary statistics
  stats <- list(
    n_entities = total_entities,
    n_periods = ncol(presence_matrix),
    total_obs = sum(presence_matrix),
    balanced_obs = nrow(presence_matrix) * ncol(presence_matrix),
    missing_obs = nrow(presence_matrix) *
      ncol(presence_matrix) -
      sum(presence_matrix),
    pct_missing = (1 -
      sum(presence_matrix) / (nrow(presence_matrix) * ncol(presence_matrix))) *
      100,
    entities_with_gaps = sum(rowSums(presence_matrix) < ncol(presence_matrix)),
    pct_entities_with_gaps = (sum(
      rowSums(presence_matrix) < ncol(presence_matrix)
    ) /
      nrow(presence_matrix)) *
      100,
    n_patterns = length(pattern_groups)
  )

  # Order time periods
  if (all(grepl("^-?\\d+\\.?\\d*$", unique_times))) {
    col_order <- order(as.numeric(unique_times))
  } else {
    col_order <- order(unique_times)
  }

  # FIX: Ensure ordered_matrix remains a matrix
  ordered_matrix <- pattern_matrix[, col_order, drop = FALSE]

  # Set up plot parameters
  old_mar <- par("mar")
  on.exit(par(mar = old_mar)) # Ensure parameters are reset even on error
  par(mar = mar)

  # Create color palette - FIXED: Reverse the colors to match correct mapping
  # The image() function maps lowest matrix values to first color, highest to last
  # We have matrix values: 0 = missing, 1 = present
  # So we want: 0 -> colors[2] (missing), 1 -> colors[1] (present)
  # Therefore we need to reverse the color vector
  col_palette <- rev(colors) # REVERSED the colors

  # Reverse the matrix for plotting so most common pattern is at top
  reversed_matrix <- ordered_matrix[nrow(ordered_matrix):1, , drop = FALSE]

  # Create heatmap without axis labels initially
  image(
    1:ncol(reversed_matrix),
    1:nrow(reversed_matrix),
    t(reversed_matrix),
    col = col_palette,
    xlab = "",
    ylab = "",
    main = main,
    axes = FALSE,
    cex.lab = cex.lab,
    useRaster = TRUE
  )

  # Add x-axis
  axis(
    1,
    at = 1:ncol(reversed_matrix),
    labels = colnames(reversed_matrix),
    las = 2,
    cex.axis = cex.axis
  )

  # Add y-axis with pattern labels - use original order for labels but reversed positions
  pattern_labels <- sprintf(
    "Pattern %d (n=%d, %.1f%%)",
    seq_len(nrow(ordered_matrix)),
    pattern_counts,
    pattern_pcts
  )

  axis(
    2,
    at = 1:nrow(reversed_matrix),
    labels = pattern_labels[nrow(reversed_matrix):1], # Reverse labels to match reversed matrix
    las = 1,
    cex.axis = cex.pattern
  )

  # Add x-axis label
  title(xlab = xlab, cex.lab = cex.lab, line = 3.5)

  # Add grid
  abline(h = 1:nrow(reversed_matrix) - 0.5, col = "gray80", lty = 3)
  abline(v = 1:ncol(reversed_matrix) - 0.5, col = "gray80", lty = 3)

  # Add summary statistics if requested
  if (show_stats) {
    stats_text <- c(
      sprintf("Entities: %d", stats$n_entities),
      sprintf("Time periods: %d", stats$n_periods),
      sprintf("Observations: %d", stats$total_obs),
      sprintf("Missing: %d (%.1f%%)", stats$missing_obs, stats$pct_missing),
      sprintf("Patterns: %d", stats$n_patterns)
    )

    mtext(
      paste(stats_text, collapse = " | "),
      side = 3,
      line = 0.5,
      cex = 0.7 * cex.lab
    )
  }

  # Add horizontal legend at the bottom - FIXED: Use original color order
  # Since we reversed the colors in the palette, we use the original order for legend
  legend(
    x = "bottom",
    legend = c("Present", "Missing"),
    fill = colors, # Use original color order: Present = colors[1], Missing = colors[2]
    bty = "n",
    cex = 0.8,
    title = "Observation Status:",
    horiz = TRUE,
    inset = c(0, -0.25), # Position below the x-axis label
    xpd = TRUE, # Allow plotting outside plot area
    title.adj = 0
  )

  # Return invisible results with detailed pattern information
  invisible(list(
    presence_matrix = presence_matrix,
    pattern_matrix = ordered_matrix,
    pattern_groups = pattern_groups,
    pattern_stats = data.frame(
      pattern_id = seq_len(length(pattern_counts)),
      pattern_string = names(pattern_groups),
      n_entities = pattern_counts,
      percent_entities = pattern_pcts,
      entities = I(pattern_groups)
    ),
    statistics = stats,
    group_var = group_name,
    time_var = time_name,
    color_mapping = c("1" = colors[1], "0" = colors[2]) # Document the color mapping
  ))
}
