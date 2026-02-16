#' Panel Data Structure Validation
#'
#' This function performs comprehensive validation of panel data structure
#' and returns a data.frame with validation test results along with diagnostic
#' information for any issues found.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable in panel data.
#'        Not required if data has panel attributes.
#'
#' @return A data.frame with validation test results.
#'
#' @details
#' The function performs the following validation tests:
#' \itemize{
#'   \item \strong{Data structure}: Checks if data is a valid data.frame
#'   \item \strong{Group variable}: Verifies existence of group variable
#'   \item \strong{Group completeness}: Checks for missing values in group variable
#'   \item \strong{Time variable}: Verifies existence of time variable
#'   \item \strong{Time completeness}: Checks for missing values in time variable
#'   \item \strong{Group-time distinct}: Ensures group and time are different variables
#'   \item \strong{Duplicates}: Identifies duplicate group-time combinations
#'   \item \strong{Balance}: Checks if panel is balanced (all groups have same time points)
#'   \item \strong{Time sequence}: Checks regularity of time sequence in entire panel
#'   \item \strong{Group intervals}: Checks regularity of time intervals within groups
#' }
#'
#' The returned data.frame has three columns: "test", "status", "message".
#' The first row contains an overall panel status, and subsequent rows contain
#' individual test results with human-readable test names.
#'
#' The data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`panel_info`}{Named character vector with elements `group_var` and `time_var`.}
#'   \item{`summary`}{Logical summary of test results (TRUE = passed, FALSE = failed/warning).}
#'   \item{`details`}{List containing problematic observations/entities/periods for each test.}
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#' }
#'
#' @seealso
#' [set_panel()], [describe_dimensions()], [describe_balance()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' panel_check <- check_panel(production, group = "firm", time = "year")
#'
#' # View test results
#' print(panel_check)
#'
#' # Access test results as data.frame
#' test_results <- as.data.frame(panel_check)
#'
#' # Access attributes
#' summary_info <- attr(panel_check, "summary")
#' details_info <- attr(panel_check, "details")
#' meta_info <- attr(panel_check, "metadata")
#' panel_info <- attr(panel_check, "panel_info")
#'
#' # Get specific details
#' has_complete_groups <- summary_info$group_completeness
#' duplicate_obs <- details_info$duplicate_observations
#' unbalanced_entities <- details_info$unbalanced_groups
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' panel_check2 <- check_panel(panel_data)
#'
#' @export
check_panel <- function(data, group = NULL, time = NULL) {
  # Check for panel_data class and extract info
  if (inherits(data, "panel_data")) {
    panel_info <- attr(data, "panel_info")
    if (
      is.null(panel_info) ||
        is.null(panel_info["group_var"]) ||
        is.null(panel_info["time_var"])
    ) {
      stop(
        "Object has class 'panel_data' but missing or incomplete 'panel_info' attribute."
      )
    }
    group <- panel_info["group_var"]
    time <- panel_info["time_var"]
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

  # Check for missing values
  if (any(is.na(data[[group]]))) {
    warning("group variable '", group, "' contains missing values")
  }

  if (any(is.na(data[[time]]))) {
    warning("time variable '", time, "' contains missing values")
  }

  # Initialize exploration results
  exploration_results <- data.frame(
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

  # Check for irregular time sequence in entire panel
  has_irregular_time_sequence <- FALSE
  time_sequence_regular <- TRUE

  if (
    is.numeric(time_vector) ||
      inherits(time_vector, "Date") ||
      inherits(time_vector, "POSIXt")
  ) {
    # Check entire time sequence (across all groups)
    all_unique_times <- sort(unique(time_vector))

    if (length(all_unique_times) > 1) {
      global_intervals <- diff(all_unique_times)
      if (length(unique(global_intervals)) > 1) {
        has_irregular_time_sequence <- TRUE
        time_sequence_regular <- FALSE
      }
    }
  }

  # Check for irregular time intervals within groups
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

  # Build exploration results with human-readable test names
  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Data structure",
      status = "PASS",
      message = "Valid data.frame structure",
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Group variable",
      status = "PASS",
      message = paste("Group variable '", group, "' found", sep = ""),
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Group completeness",
      status = ifelse(any(is.na(data[[group]])), "WARNING", "PASS"),
      message = ifelse(
        any(is.na(data[[group]])),
        paste(
          "Group variable has",
          sum(is.na(data[[group]])),
          "missing values"
        ),
        "No missing values in group variable"
      ),
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Time variable",
      status = "PASS",
      message = paste("Time variable '", time, "' found", sep = ""),
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Time completeness",
      status = ifelse(any(is.na(data[[time]])), "WARNING", "PASS"),
      message = ifelse(
        any(is.na(data[[time]])),
        paste("Time variable has", sum(is.na(data[[time]])), "missing values"),
        "No missing values in time variable"
      ),
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Group-time distinct",
      status = "PASS",
      message = "Group and time are distinct variables",
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Duplicates",
      status = ifelse(has_duplicates, "FAIL", "PASS"),
      message = ifelse(
        has_duplicates,
        paste(sum(duplicated(combos)), "duplicate group-time pairs found"),
        "No duplicate group-time pairs"
      ),
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Balance",
      status = ifelse(is_balanced, "PASS", "FAIL"),
      message = ifelse(is_balanced, "Panel is balanced", "Panel is unbalanced"),
      stringsAsFactors = FALSE
    )
  )

  # Check time sequence regularity (entire panel)
  if (
    is.numeric(time_vector) ||
      inherits(time_vector, "Date") ||
      inherits(time_vector, "POSIXt")
  ) {
    exploration_results <- rbind(
      exploration_results,
      data.frame(
        variable = "Time sequence",
        status = ifelse(has_irregular_time_sequence, "FAIL", "PASS"),
        message = ifelse(
          has_irregular_time_sequence,
          "Irregular time sequence in entire panel",
          "Regular time sequence in entire panel"
        ),
        stringsAsFactors = FALSE
      )
    )
  } else {
    exploration_results <- rbind(
      exploration_results,
      data.frame(
        variable = "Time sequence",
        status = "INFO",
        message = "Time variable is not numeric/Date-like",
        stringsAsFactors = FALSE
      )
    )
  }

  # Check interval regularity within groups
  if (
    is.numeric(time_vector) ||
      inherits(time_vector, "Date") ||
      inherits(time_vector, "POSIXt")
  ) {
    exploration_results <- rbind(
      exploration_results,
      data.frame(
        variable = "Group intervals",
        status = ifelse(has_irregular_intervals, "FAIL", "PASS"),
        message = ifelse(
          has_irregular_intervals,
          "Irregular time intervals within groups",
          "Regular time intervals within groups"
        ),
        stringsAsFactors = FALSE
      )
    )
  } else {
    exploration_results <- rbind(
      exploration_results,
      data.frame(
        variable = "Group intervals",
        status = "INFO",
        message = "Time variable is not numeric/Date-like",
        stringsAsFactors = FALSE
      )
    )
  }

  # Determine overall status
  overall_status <- ifelse(
    any(exploration_results$status %in% c("FAIL", "WARNING")),
    ifelse(any(exploration_results$status == "FAIL"), "FAIL", "WARNING"),
    "PASS"
  )

  # Create simplified details list with problematic observations only
  details_list <- list()

  # Add problematic observations for each test with descriptive names
  if (any(is.na(data[[group]]))) {
    details_list$missing_group_values <- which(is.na(data[[group]]))
  }

  if (any(is.na(data[[time]]))) {
    details_list$missing_time_values <- which(is.na(data[[time]]))
  }

  if (has_duplicates) {
    details_list$duplicate_observations <- duplicate_indices
    details_list$duplicate_combinations <- duplicate_combos
  }

  if (!is_balanced) {
    details_list$unbalanced_groups <- names(missing_time_info)
    if (length(details_list$unbalanced_groups) > 0) {
      details_list$missing_time_points <- missing_time_info
    }
  }

  if (has_irregular_time_sequence) {
    details_list$irregular_time_intervals <- diff(sort(unique(time_vector)))
    details_list$all_time_points <- sort(unique(time_vector))
  }

  if (has_irregular_intervals) {
    details_list$irregular_interval_groups <- irregular_groups
    details_list$group_interval_details <- interval_details[irregular_groups]
  }

  # Create simplified summary list with TRUE/FALSE for key tests
  summary_list <- list()

  # Add logical results for key tests (TRUE = passed, FALSE = failed/warning)
  summary_list$group_completeness <- !any(is.na(data[[group]]))
  summary_list$time_completeness <- !any(is.na(data[[time]]))
  summary_list$no_duplicates <- !has_duplicates
  summary_list$balance <- is_balanced
  summary_list$time_sequence <- !has_irregular_time_sequence
  summary_list$group_intervals <- !has_irregular_intervals

  # Create the output data.frame with test results
  test_results <- exploration_results

  # Add overall status as first row
  overall_row <- data.frame(
    test = "Overall",
    status = overall_status,
    message = ifelse(
      overall_status == "PASS",
      "Panel structure is valid",
      "Panel structure has issues"
    ),
    stringsAsFactors = FALSE
  )

  # Rename columns and combine
  names(test_results) <- c("test", "status", "message")
  final_results <- rbind(overall_row, test_results)

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time
  )

  # Set attributes in desired order
  attr(final_results, "panel_info") <- c(group_var = group, time_var = time)
  attr(final_results, "summary") <- summary_list
  attr(final_results, "details") <- details_list
  attr(final_results, "metadata") <- metadata

  # Set class
  class(final_results) <- c("panel_description", "data.frame")

  return(final_results)
}
