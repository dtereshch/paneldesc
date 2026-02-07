#' Detailed Panel Data Participation Patterns
#'
#' Provides detailed analysis of participation patterns in panel data, including
#' time coverage statistics and pattern distributions.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'              Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'             Not required if data has panel attributes.
#' @param type A character string specifying how to define entity presence: "observed", "balanced", or "complete". Default = "balanced".
#' @param max_patterns An integer specifying the maximum number of patterns to display in detail. Default = 10.
#' @param print_result A logical flag indicating whether to print the validation results.
#' Default = TRUE.
#'
#' @return Invisibly returns a list containing detailed participation pattern analysis.
#' Prints time coverage distribution and common participation patterns.
#'
#' @details
#' \strong{Type} parameter definitions:
#' \describe{
#'   \item{\code{"observed"}}{Entity is present if it has a row in the data (even with only panel ID variables)}
#'   \item{\code{"balanced"}}{Entity is present if it has at least one non-NA substantive variable (default)}
#'   \item{\code{"complete"}}{Entity is present only if it has no NA values in all substantive variables}
#' }
#'
#' The returned list contains the following components:
#' \describe{
#'   \item{\code{summary}}{List with summary statistics including:
#'     \itemize{
#'       \item \code{total_entities}: Total number of unique entities
#'       \item \code{n_patterns}: Number of unique participation patterns
#'       \item \code{type}: Presence type used for analysis
#'     }
#'   }
#'   \item{\code{patterns}}{List with pattern analysis including:
#'     \itemize{
#'       \item \code{pattern_matrix}: Matrix showing presence patterns (rows = patterns, columns = time periods)
#'       \item \code{pattern_groups}: List of entities belonging to each pattern
#'       \item \code{pattern_stats}: Data.frame with pattern statistics
#'       \item \code{entities_by_pattern}: Named vector with number of entities in each pattern
#'       \item \code{presence_matrix}: Original presence matrix used for analysis
#'     }
#'   }
#'   \item{\code{coverage}}{List with time coverage statistics including:
#'     \itemize{
#'       \item \code{time_coverage_stats}: Named vector with quantiles of time periods covered
#'       \item \code{time_coverage_by_entity}: Named vector with number of time periods covered for each entity
#'     }
#'   }
#'   \item{\code{metadata}}{List with analysis parameters including:
#'     \itemize{
#'       \item \code{group_var}: The group variable name
#'       \item \code{time_var}: The time variable name
#'       \item \code{type}: The presence type used for analysis
#'       \item \code{max_patterns}: Maximum number of patterns to display
#'     }
#'   }
#' }
#'
#' Patterns are sorted by frequency (most common first).
#'
#' @seealso
#' [describe_participation()], [plot_participation()], [describe_incomplete()], [explore_balance()], [explore_panel()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage (prints by default)
#' explore_participation(production, group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' explore_participation(panel_data)
#'
#' # Show only top 5 patterns
#' explore_participation(production, group = "firm", time = "year", max_patterns = 5)
#'
#' # Use different presence types
#' explore_participation(production, group = "firm", time = "year", type = "observed")
#' explore_participation(production, group = "firm", time = "year", type = "complete")
#'
#' # Assign the results without printing
#' participation_result <- explore_participation(production, group = "firm", time = "year",
#' print_result = FALSE)
#'
#' # Access the number of entities per pattern
#' entities_per_pattern <- participation_result$patterns$entities_by_pattern
#' print(entities_per_pattern)
#'
#' # Access time coverage statistics
#' coverage_stats <- participation_result$coverage$time_coverage_stats
#' print(coverage_stats)
#'
#' # Access individual entity coverage
#' entity_coverage <- participation_result$coverage$time_coverage_by_entity
#' print(entity_coverage[1:5])  # First 5 entities
#'
#' @export
explore_participation <- function(
  data,
  group = NULL,
  time = NULL,
  type = "balanced",
  max_patterns = 10,
  print_result = TRUE
) {
  # Check if data has panel attributes
  has_panel_attrs <- !is.null(attr(data, "panel_group")) &&
    !is.null(attr(data, "panel_time"))

  if (has_panel_attrs) {
    # Extract group and time from attributes
    group <- attr(data, "panel_group")
    time <- attr(data, "panel_time")
  } else {
    # Handle regular data.frame
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame, not ", class(data)[1])
    }

    if (is.null(group) || is.null(time)) {
      stop(
        "For regular data.frames, both 'group' and 'time' arguments must be provided"
      )
    }
  }

  # Common validation
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string, not ", class(time)[1])
  }

  if (!is.character(type) || length(type) != 1) {
    stop("'type' must be a single character string, not ", class(type)[1])
  }

  if (!type %in% c("balanced", "observed", "complete")) {
    stop('type must be one of: "balanced", "observed", "complete"')
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  if (!time %in% names(data)) {
    stop('variable "', time, '" not found in data')
  }

  if (
    !is.numeric(max_patterns) || length(max_patterns) != 1 || max_patterns < 1
  ) {
    stop(
      "'max_patterns' must be a single positive integer, not ",
      class(max_patterns)[1]
    )
  }

  if (!is.logical(print_result) || length(print_result) != 1) {
    stop(
      "'print_result' must be a single logical value, not ",
      class(print_result)[1]
    )
  }

  # Get substantive variables
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Filter data based on type
  if (type == "observed") {
    # Keep all rows (no filtering)
    filtered_data <- data
  } else if (type == "balanced") {
    # Keep rows with at least one non-NA substantive variable
    has_data <- apply(data[substantive_vars], 1, function(x) any(!is.na(x)))
    filtered_data <- data[has_data, ]
  } else if (type == "complete") {
    # Keep rows with no NAs in substantive variables
    complete_cases <- complete.cases(data[substantive_vars])
    filtered_data <- data[complete_cases, ]
  }

  # Extract variables
  group_var <- as.character(filtered_data[[group]])
  time_var <- as.character(filtered_data[[time]])

  # Get unique values
  unique_groups <- unique(group_var)
  unique_times <- unique(time_var)

  # Order time periods
  if (all(grepl("^-?\\d+\\.?\\d*$", unique_times))) {
    time_order <- order(as.numeric(unique_times))
  } else {
    time_order <- order(unique_times)
  }
  ordered_times <- unique_times[time_order]

  # Create presence matrix
  presence_matrix <- matrix(
    0,
    nrow = length(unique_groups),
    ncol = length(ordered_times),
    dimnames = list(unique_groups, ordered_times)
  )

  # Fill presence matrix based on type
  if (type == "observed") {
    # Count all rows as presence
    for (i in seq_along(group_var)) {
      row_idx <- which(unique_groups == group_var[i])
      col_idx <- which(ordered_times == time_var[i])
      if (length(col_idx) > 0) {
        presence_matrix[row_idx, col_idx] <- 1
      }
    }
  } else if (type == "balanced") {
    # Use filtered data which already has at least one non-NA
    for (i in seq_along(group_var)) {
      row_idx <- which(unique_groups == group_var[i])
      col_idx <- which(ordered_times == time_var[i])
      if (length(col_idx) > 0) {
        presence_matrix[row_idx, col_idx] <- 1
      }
    }
  } else if (type == "complete") {
    # For complete type, need to check each (group, time) combination
    # Get original data to check completeness for all combinations
    all_groups <- as.character(data[[group]])
    all_times <- as.character(data[[time]])

    # Create a completeness indicator for each row
    complete_rows <- complete.cases(data[substantive_vars])

    # Fill presence matrix with 1 only for complete rows
    for (i in seq_along(all_groups)) {
      if (complete_rows[i]) {
        row_idx <- which(unique_groups == all_groups[i])
        col_idx <- which(ordered_times == all_times[i])
        if (length(row_idx) > 0 && length(col_idx) > 0) {
          presence_matrix[row_idx, col_idx] <- 1
        }
      }
    }
  }

  # Calculate time coverage for each entity
  time_coverage_by_entity <- rowSums(presence_matrix)
  names(time_coverage_by_entity) <- rownames(presence_matrix)

  # Calculate time coverage statistics (quantiles)
  time_coverage_stats <- c(
    min = min(time_coverage_by_entity),
    `5%` = as.numeric(quantile(time_coverage_by_entity, 0.05)),
    `25%` = as.numeric(quantile(time_coverage_by_entity, 0.25)),
    `50%` = as.numeric(quantile(time_coverage_by_entity, 0.50)),
    `75%` = as.numeric(quantile(time_coverage_by_entity, 0.75)),
    `95%` = as.numeric(quantile(time_coverage_by_entity, 0.95)),
    max = max(time_coverage_by_entity)
  )

  # Group entities by missing value patterns
  pattern_strings <- apply(presence_matrix, 1, function(x) {
    paste(x, collapse = "")
  })
  pattern_groups <- split(rownames(presence_matrix), pattern_strings)

  # Create pattern matrix
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
    for (i in seq_along(pattern_groups)) {
      pattern_matrix[i, ] <- as.numeric(strsplit(names(pattern_groups)[i], "")[[
        1
      ]])
    }
  }

  # Initial rownames before sorting
  rownames(pattern_matrix) <- paste0("Pattern ", seq_len(nrow(pattern_matrix)))
  colnames(pattern_matrix) <- ordered_times

  # Count entities per pattern
  pattern_counts <- numeric(length(pattern_groups))
  for (i in seq_along(pattern_groups)) {
    pattern_counts[i] <- length(pattern_groups[[i]])
  }

  # Order patterns by frequency (most common first)
  pattern_order <- order(pattern_counts, decreasing = TRUE)
  pattern_matrix <- pattern_matrix[pattern_order, , drop = FALSE]
  pattern_counts <- pattern_counts[pattern_order]
  pattern_groups <- pattern_groups[pattern_order]

  # FIX: Reassign pattern IDs after sorting
  rownames(pattern_matrix) <- paste0("Pattern ", seq_len(nrow(pattern_matrix)))
  names(pattern_groups) <- rownames(pattern_matrix)

  # Calculate percentages
  total_entities <- length(unique_groups)
  pattern_pcts <- pattern_counts / total_entities * 100

  # Create vector with entities number for each pattern
  entities_by_pattern <- pattern_counts
  names(entities_by_pattern) <- rownames(pattern_matrix)

  # Calculate time coverage for each pattern
  pattern_time_coverage <- numeric(nrow(pattern_matrix))
  for (i in seq_len(nrow(pattern_matrix))) {
    pattern_time_coverage[i] <- sum(pattern_matrix[i, ])
  }

  # FIX: Update pattern strings for pattern_stats
  pattern_strings_sorted <- apply(pattern_matrix, 1, function(x) {
    paste(x, collapse = "")
  })

  # Create pattern stats data.frame
  pattern_stats <- data.frame(
    pattern_id = seq_len(length(pattern_counts)),
    pattern_string = pattern_strings_sorted,
    n_entities = pattern_counts,
    percent_entities = pattern_pcts,
    time_coverage = pattern_time_coverage,
    entities = I(pattern_groups),
    stringsAsFactors = FALSE
  )

  # Create unified return object
  result <- list(
    summary = list(
      total_entities = total_entities,
      n_patterns = length(pattern_groups),
      type = type
    ),
    patterns = list(
      pattern_matrix = pattern_matrix,
      pattern_groups = pattern_groups,
      pattern_stats = pattern_stats,
      entities_by_pattern = entities_by_pattern,
      presence_matrix = presence_matrix
    ),
    coverage = list(
      time_coverage_stats = time_coverage_stats,
      time_coverage_by_entity = time_coverage_by_entity
    ),
    metadata = list(
      group_var = group,
      time_var = time,
      type = type,
      max_patterns = max_patterns
    )
  )

  # Print if requested
  if (print_result) {
    full_width <- getOption("width", 80)
    width <- floor(full_width * 0.75)
    separator <- paste(rep("-", width), collapse = "")

    cat("\n")
    cat("PANEL DATA PARTICIPATION ANALYSIS\n")
    cat(paste(rep("=", width), collapse = ""), "\n\n")

    # Print time coverage statistics
    cat("DISTRIBUTION OF ENTITIES BY TIME COVERAGE\n")
    cat(separator, "\n")

    # Format the time coverage statistics - round them to nearest whole number
    # since they represent counts of time periods
    stats_values <- sapply(time_coverage_stats, function(x) {
      if (is.numeric(x)) {
        # Round to handle cases where quantiles give decimal values
        round(x)
      } else {
        x
      }
    })

    # Determine column width based on the maximum width of labels and values
    label_widths <- nchar(names(time_coverage_stats))
    value_widths <- nchar(as.character(stats_values))
    max_width <- max(label_widths, value_widths, na.rm = TRUE)

    # Print header with proper spacing
    header_labels <- names(time_coverage_stats)
    header_formatted <- sapply(header_labels, function(label) {
      sprintf("%*s", max_width, label)
    })
    cat(paste(header_formatted, collapse = " "))
    cat("\n")

    # Print values with proper spacing (right-aligned)
    value_formatted <- sapply(stats_values, function(value) {
      sprintf("%*s", max_width, as.character(value))
    })
    cat(paste(value_formatted, collapse = " "))
    cat("\n\n")

    # Print formatted output
    n_to_display <- min(length(pattern_groups), max_patterns)
    cat(sprintf(
      "INFORMATION ON TOP %d PARTICIPATION PATTERNS\n",
      n_to_display
    ))
    cat(separator, "\n")

    # Calculate maximum widths for alignment
    max_pattern_width <- nchar(as.character(n_to_display))
    max_count_width <- max(nchar(as.character(pattern_counts[1:n_to_display])))

    for (i in seq_len(n_to_display)) {
      pattern <- pattern_matrix[i, ]
      pattern_visual <- ifelse(pattern == 1, "1", "0")

      # Format the pattern line with aligned separators
      pattern_label <- sprintf("Pattern %*d", max_pattern_width, i)
      count_label <- sprintf("n=%*d", max_count_width, pattern_counts[i])
      pct_label <- sprintf("%5.1f%%", pattern_pcts[i])

      # Show entities for this pattern
      entities_in_pattern <- pattern_groups[[i]]
      if (length(entities_in_pattern) <= 5) {
        entities_label <- paste(entities_in_pattern, collapse = ", ")
      } else {
        entities_label <- paste(
          paste(entities_in_pattern[1:3], collapse = ", "),
          ", ... (",
          length(entities_in_pattern) - 3,
          " more)",
          sep = ""
        )
      }

      cat(sprintf(
        "%s (%s, %s): [%s], entities: %s\n",
        pattern_label,
        count_label,
        pct_label,
        paste(pattern_visual, collapse = ""),
        entities_label
      ))
    }

    if (length(pattern_groups) > max_patterns) {
      cat(sprintf(
        "\n... and %d more patterns (use max_patterns = %d to see all)\n",
        length(pattern_groups) - max_patterns,
        length(pattern_groups)
      ))
    }
    cat("\n")
  }

  invisible(result)
}
