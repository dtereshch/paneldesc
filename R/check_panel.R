#' Panel Data Structure Validation
#'
#' This function performs comprehensive validation of panel data structure,
#' checking for common issues that could affect panel data analysis.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#' @param time A character string specifying the name of the time variable in panel data.
#' @param detailed A logical flag indicating whether to return detailed validation results.
#' Default = FALSE.
#'
#' @return A list with panel validation results. The structure depends on the `detailed` parameter.
#' The result is returned invisibly, and will be printed automatically unless assigned to a variable.
#'
#' @details
#' The function performs the following checks:
#' \enumerate{
#'   \item Validates that data is a data.frame
#'   \item Checks that group and time variables exist in the data
#'   \item Ensures group and time are not the same variable
#'   \item Identifies duplicate group-time combinations
#'   \item Checks for irregular time intervals within groups (for numeric/Date time variables)
#'   \item Determines if panel is balanced (same time points for all groups)
#'   \item Provides summary statistics of panel structure
#' }
#'
#' @examples
#' data(production)
#'
#' # Basic validation
#' check_panel(production, group = "firm", time = "year")
#'
#' # Detailed validation
#' check_panel(production, group = "firm", time = "year", detailed = TRUE)
#'
#' # Assigning to variable - will not print
#' check_result <- check_panel(production, group = "firm", time = "year")
#'
#' # Access useful vectors for further analysis
#' duplicate_rows <- check_result$vectors$duplicate_rows
#' unbalanced_firms <- check_result$vectors$unbalanced_groups
#' irregular_firms <- check_result$vectors$irregular_groups
#' obs_per_firm <- check_result$vectors$observations_per_group
#'
#' # Identify problematic observations
#' problematic_indices <- unique(c(
#' check_result$vectors$duplicate_indices,
#' check_result$vectors$missing_groups,
#' check_result$vectors$missing_times
#' ))
#'
#' @seealso
#' [decompose_variation()] for variance decomposition in panel data
#'
#' @export
check_panel <- function(data, group, time, detailed = FALSE) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
  }

  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string")
  }

  if (!group %in% names(data)) {
    stop('group variable "', group, '" not found in data')
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string")
  }

  if (!time %in% names(data)) {
    stop('time variable "', time, '" not found in data')
  }

  if (time == group) {
    stop("'time' and 'group' cannot be the same variable")
  }

  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("'detailed' must be a single logical value, not ", class(detailed)[1])
  }

  # Check for missing values
  if (any(is.na(data[[group]]))) {
    warning("group variable '", group, "' contains missing values")
  }

  if (any(is.na(data[[time]]))) {
    warning("time variable '", time, "' contains missing values")
  }

  # Initialize validation results
  validation_results <- data.frame(
    variable = character(),
    status = character(),
    message = character(),
    stringsAsFactors = FALSE
  )

  # Panel structure
  n_groups <- length(unique(data[[group]]))
  n_periods <- length(unique(data[[time]]))
  n_obs <- nrow(data)

  # Get group and time vectors
  group_vector <- data[[group]]
  time_vector <- data[[time]]

  # Create combo identifier
  combos <- paste(group_vector, time_vector, sep = "|")

  # Check for duplicate group-time combinations
  has_duplicates <- any(duplicated(combos))

  # Identify duplicate indices
  duplicate_indices <- which(
    duplicated(combos) | duplicated(combos, fromLast = TRUE)
  )
  duplicate_rows <- data[duplicate_indices, ]

  # Create unique identifier for duplicates
  duplicate_combos <- unique(combos[duplicated(combos)])
  duplicate_summary <- data.frame(
    combo = duplicate_combos,
    count = sapply(duplicate_combos, function(x) sum(combos == x)),
    stringsAsFactors = FALSE
  )

  # Split combos into group and time for the summary
  if (nrow(duplicate_summary) > 0) {
    split_combos <- strsplit(duplicate_summary$combo, "\\|")
    duplicate_summary$group <- sapply(split_combos, function(x) x[1])
    duplicate_summary$time <- sapply(split_combos, function(x) x[2])
  }

  # Check for balanced panel
  time_by_group <- split(time_vector, group_vector)
  unique_time_sets <- lapply(time_by_group, unique)
  time_set_lengths <- sapply(unique_time_sets, length)

  if (length(unique(time_set_lengths)) == 1) {
    all_time_sets <- unique(unique_time_sets)
    is_balanced <- length(all_time_sets) == 1
  } else {
    is_balanced <- FALSE
  }

  # Identify groups with missing time points (if unbalanced)
  if (!is_balanced) {
    # Find all unique time points across all groups
    all_times <- sort(unique(time_vector))

    # For each group, identify missing time points
    missing_time_info <- list()
    for (grp in names(time_by_group)) {
      group_times <- unique(time_by_group[[grp]])
      missing_times <- setdiff(all_times, group_times)
      if (length(missing_times) > 0) {
        missing_time_info[[grp]] <- missing_times
      }
    }
  } else {
    missing_time_info <- list()
  }

  # Check for irregular time intervals
  has_irregular_intervals <- FALSE
  irregular_groups <- character()
  interval_details <- list()

  if (
    is.numeric(time_vector) ||
      inherits(time_vector, "Date") ||
      inherits(time_vector, "POSIXt")
  ) {
    for (grp in names(time_by_group)) {
      times <- time_by_group[[grp]]
      unique_times <- sort(unique(times))
      if (length(unique_times) > 1) {
        intervals <- diff(unique_times)
        if (length(unique(intervals)) > 1) {
          has_irregular_intervals <- TRUE
          irregular_groups <- c(irregular_groups, grp)
          interval_details[[grp]] <- list(
            times = unique_times,
            intervals = intervals,
            is_regular = FALSE
          )
        } else {
          interval_details[[grp]] <- list(
            times = unique_times,
            intervals = intervals,
            is_regular = TRUE
          )
        }
      } else {
        interval_details[[grp]] <- list(
          times = unique_times,
          intervals = numeric(0),
          is_regular = TRUE
        )
      }
    }
  }

  # Calculate observations per group
  group_table <- table(data[[group]])
  avg_obs_per_group <- mean(group_table)

  # Get groups with minimum and maximum observations
  min_obs <- min(group_table)
  max_obs <- max(group_table)
  groups_with_min_obs <- names(group_table)[group_table == min_obs]
  groups_with_max_obs <- names(group_table)[group_table == max_obs]

  # Build validation results
  validation_results <- rbind(
    validation_results,
    data.frame(
      variable = "data",
      status = "PASS",
      message = "Valid data.frame structure",
      stringsAsFactors = FALSE
    )
  )

  validation_results <- rbind(
    validation_results,
    data.frame(
      variable = "group",
      status = "PASS",
      message = paste("Group variable '", group, "' found", sep = ""),
      stringsAsFactors = FALSE
    )
  )

  validation_results <- rbind(
    validation_results,
    data.frame(
      variable = "group_completeness",
      status = ifelse(any(is.na(data[[group]])), "WARNING", "PASS"),
      message = ifelse(
        any(is.na(data[[group]])),
        paste("Group variable has", sum(is.na(data[[group]])), "NAs"),
        "No missing values in group"
      ),
      stringsAsFactors = FALSE
    )
  )

  validation_results <- rbind(
    validation_results,
    data.frame(
      variable = "time",
      status = "PASS",
      message = paste("Time variable '", time, "' found", sep = ""),
      stringsAsFactors = FALSE
    )
  )

  validation_results <- rbind(
    validation_results,
    data.frame(
      variable = "time_completeness",
      status = ifelse(any(is.na(data[[time]])), "WARNING", "PASS"),
      message = ifelse(
        any(is.na(data[[time]])),
        paste("Time variable has", sum(is.na(data[[time]])), "NAs"),
        "No missing values in time"
      ),
      stringsAsFactors = FALSE
    )
  )

  validation_results <- rbind(
    validation_results,
    data.frame(
      variable = "group_time_distinct",
      status = "PASS",
      message = "Group and time are distinct variables",
      stringsAsFactors = FALSE
    )
  )

  validation_results <- rbind(
    validation_results,
    data.frame(
      variable = "duplicates",
      status = ifelse(has_duplicates, "FAIL", "PASS"),
      message = ifelse(
        has_duplicates,
        paste(sum(duplicated(combos)), "duplicate group-time pairs"),
        "No duplicate group-time pairs"
      ),
      stringsAsFactors = FALSE
    )
  )

  validation_results <- rbind(
    validation_results,
    data.frame(
      variable = "balance",
      status = ifelse(is_balanced, "PASS", "FAIL"),
      message = ifelse(is_balanced, "Panel is balanced", "Panel is unbalanced"),
      stringsAsFactors = FALSE
    )
  )

  if (
    is.numeric(time_vector) ||
      inherits(time_vector, "Date") ||
      inherits(time_vector, "POSIXt")
  ) {
    validation_results <- rbind(
      validation_results,
      data.frame(
        variable = "intervals",
        status = ifelse(has_irregular_intervals, "FAIL", "PASS"),
        message = ifelse(
          has_irregular_intervals,
          "Irregular time intervals detected",
          "Regular time intervals"
        ),
        stringsAsFactors = FALSE
      )
    )
  } else {
    validation_results <- rbind(
      validation_results,
      data.frame(
        variable = "intervals",
        status = "INFO",
        message = "Time variable is not numeric/Date-like",
        stringsAsFactors = FALSE
      )
    )
  }

  # Determine overall status
  overall_status <- ifelse(
    any(validation_results$status %in% c("FAIL", "WARNING")),
    ifelse(any(validation_results$status == "FAIL"), "FAIL", "WARNING"),
    "PASS"
  )

  # Create panel summary string
  balance_status <- ifelse(is_balanced, "balanced", "unbalanced")
  issues <- c()
  if (has_duplicates) {
    issues <- c(issues, "with duplicates")
  }
  if (has_irregular_intervals) {
    issues <- c(issues, "with irregular intervals")
  }

  panel_summary <- paste0(
    "Panel structure: ",
    n_groups,
    " groups, ",
    n_periods,
    " time periods, ",
    n_obs,
    " observations, ",
    balance_status
  )

  if (length(issues) > 0) {
    panel_summary <- paste0(panel_summary, ", ", paste(issues, collapse = ", "))
  }

  # Create result object with useful vectors for further analysis
  result <- list(
    panel_summary = panel_summary,
    validation_status = overall_status,
    validation_message = ifelse(
      overall_status == "PASS",
      "Panel structure is valid",
      "Panel structure has issues"
    ),
    validation_results = validation_results,
    detailed = detailed,

    # Panel structure information
    panel_info = list(
      n_groups = n_groups,
      n_periods = n_periods,
      n_observations = n_obs,
      avg_obs_per_group = round(avg_obs_per_group, 2),
      is_balanced = is_balanced,
      has_duplicates = has_duplicates,
      has_irregular_intervals = has_irregular_intervals,
      group_var = group,
      time_var = time
    ),

    # Vectors for further analysis
    vectors = list(
      # All group and time vectors
      group_vector = group_vector,
      time_vector = time_vector,
      combo_vector = combos,

      # Duplicate information
      duplicate_indices = duplicate_indices,
      duplicate_rows = duplicate_rows,
      duplicate_summary = duplicate_summary,
      duplicate_combos = duplicate_combos,

      # Balance information
      all_groups = unique(group_vector),
      all_times = unique(time_vector),
      missing_time_info = missing_time_info,
      unbalanced_groups = if (!is_balanced) {
        names(missing_time_info)
      } else {
        character()
      },

      # Time interval information
      irregular_groups = irregular_groups,
      interval_details = interval_details,

      # Group statistics
      observations_per_group = group_table,
      groups_with_min_obs = groups_with_min_obs,
      groups_with_max_obs = groups_with_max_obs,
      min_observations = min_obs,
      max_observations = max_obs,

      # Missing value information
      missing_groups = which(is.na(group_vector)),
      missing_times = which(is.na(time_vector)),
      na_group_count = sum(is.na(group_vector)),
      na_time_count = sum(is.na(time_vector))
    )
  )

  class(result) <- "panel_check"

  # Check if the result is being assigned to a variable
  # by looking at the parent call
  parent_call <- sys.call(-1)
  is_assigned <- FALSE

  if (!is.null(parent_call)) {
    # Convert to string and check if it contains assignment operators
    call_text <- deparse(parent_call)
    assignment_ops <- c("<-", "=", "<<-", "assign", "->", "->>")

    for (op in assignment_ops) {
      if (grepl(op, call_text, fixed = TRUE)) {
        is_assigned <- TRUE
        break
      }
    }
  }

  # Print if not assigned (basic usage)
  if (!is_assigned) {
    # Print logic
    if (result$detailed) {
      cat("Panel Data Structure Check\n")
      cat("==============================================================\n\n")

      cat("Summary\n")
      cat("--------------------------------------------------------------\n")
      cat(result$panel_summary, "\n")
      cat("Validation Status:", result$validation_message, "\n\n")

      cat("Validation Results\n")
      cat("--------------------------------------------------------------\n")

      for (i in 1:nrow(result$validation_results)) {
        row <- result$validation_results[i, ]

        # Color coding for status
        status_str <- switch(
          row$status,
          PASS = paste0("\033[32m", row$status, "\033[0m"),
          FAIL = paste0("\033[31m", row$status, "\033[0m"),
          WARNING = paste0("\033[33m", row$status, "\033[0m"),
          INFO = paste0("\033[34m", row$status, "\033[0m"),
          row$status
        )

        cat(sprintf("  %-20s [%s] %s\n", row$variable, status_str, row$message))
      }
      cat("\n")
    } else {
      # For non-detailed output, just show the panel summary and validation status
      # without any titles
      cat(result$panel_summary, "\n")
      cat("Validation Status:", result$validation_message, "\n")
    }
  }

  # Always return the result invisibly
  invisible(result)
}
