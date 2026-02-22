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
#'   \item \strong{Balance}: Checks if panel is balanced (all groups have the same observed time points)
#'   \item \strong{Time sequence}: Checks regularity of observed time sequence in entire panel
#'   \item \strong{Group intervals}: Checks regularity of observed time intervals within groups
#' }
#'
#' The returned data.frame has three columns: "test", "status", "message".
#' The first row contains an overall panel status, and subsequent rows contain
#' individual test results with human-readable test names.
#'
#' The data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing diagnostic information for each test,
#'         including problematic observations/entities/periods, and logical
#'         summary of test results.}
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
#' details_info <- attr(panel_check, "details")
#' meta_info <- attr(panel_check, "metadata")
#'
#' # Get specific details
#' has_complete_groups <- details_info$group_completeness
#' duplicate_obs <- details_info$duplicate_observations
#' unbalanced_entities <- details_info$unbalanced_groups
#' incomplete_observed <- details_info$entities_with_incomplete_observed
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' panel_check2 <- check_panel(panel_data)
#'
#' @export
check_panel <- function(data, group = NULL, time = NULL) {
  # Helper to sort unique values preserving original class
  sort_unique_preserve <- function(x) {
    ux <- unique(x)
    if (is.numeric(ux)) {
      sort(ux)
    } else if (inherits(ux, "Date") || inherits(ux, "POSIXt")) {
      sort(ux)
    } else if (is.factor(ux)) {
      # Convert to character, sort, then rebuild factor with sorted levels
      char_lev <- as.character(ux)
      sorted_char <- sort(char_lev)
      factor(sorted_char, levels = sorted_char, ordered = is.ordered(ux))
    } else {
      sort(ux) # character, logical, etc.
    }
  }

  # Check for panel_data class and extract info from metadata
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

  # Identify substantive variables (all except group and time)
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Original vectors (preserve class)
  group_orig <- data[[group]]
  time_orig <- data[[time]]

  # Character versions for internal matching
  group_char <- as.character(group_orig)
  time_char <- as.character(time_orig)

  # Sorted unique values in original class (for storage)
  unique_groups_orig <- sort_unique_preserve(group_orig)
  unique_times_orig <- sort_unique_preserve(time_orig)

  # Check for missing values
  if (any(is.na(group_orig))) {
    warning("group variable '", group, "' contains missing values")
  }

  if (any(is.na(time_orig))) {
    warning("time variable '", time, "' contains missing values")
  }

  # Determine observed rows (at least one non-NA substantive variable)
  has_observed <- apply(data[substantive_vars], 1, function(x) any(!is.na(x)))

  # Initialize exploration results
  exploration_results <- data.frame(
    variable = character(),
    status = character(),
    message = character(),
    stringsAsFactors = FALSE
  )

  # Panel structure
  n_groups <- length(unique_groups_orig)
  n_periods <- length(unique_times_orig)
  n_obs <- nrow(data)

  # Create combo identifier (character)
  combos <- paste(group_char, time_char, sep = "|")

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

  # --- BALANCE TEST (based on observed data) ---
  observed_rows <- has_observed

  # Observed time sets per group
  time_by_group_obs <- split(
    time_orig[observed_rows],
    group_orig[observed_rows]
  )
  # Ensure all groups are represented, even those with no observed rows
  unique_time_sets_obs <- lapply(unique_groups_orig, function(g) {
    g_char <- as.character(g)
    if (g_char %in% names(time_by_group_obs)) {
      unique(time_by_group_obs[[g_char]])
    } else {
      vector(class(time_orig), 0) # empty set
    }
  })
  names(unique_time_sets_obs) <- as.character(unique_groups_orig)

  # Check if all observed sets are identical (and non-empty)
  obs_set_lengths <- sapply(unique_time_sets_obs, length)
  if (all(obs_set_lengths == 0)) {
    is_balanced_observed <- TRUE
    balance_message <- "No observed data in any group"
    balance_status <- "INFO"
  } else {
    non_zero_groups <- obs_set_lengths > 0
    if (!any(non_zero_groups)) {
      is_balanced_observed <- TRUE
      balance_message <- "No observed data in any group"
      balance_status <- "INFO"
    } else {
      ref_set <- unique_time_sets_obs[[which(non_zero_groups)[1]]]
      all_equal <- all(sapply(
        unique_time_sets_obs[non_zero_groups],
        function(x) identical(x, ref_set)
      ))
      is_balanced_observed <- all_equal
      if (is_balanced_observed) {
        balance_message <- "Panel is balanced (observed data)"
        balance_status <- "PASS"
      } else {
        time_by_group_nominal <- split(time_orig, group_orig)
        unique_time_sets_nominal <- lapply(time_by_group_nominal, unique)
        is_balanced_nominal <- length(unique(unique_time_sets_nominal)) == 1
        if (is_balanced_nominal) {
          balance_message <- "Panel is balanced nominally but not all entities have observed data in all periods"
        } else {
          balance_message <- "Panel is unbalanced (observed data)"
        }
        balance_status <- "FAIL"
      }
    }
  }

  # Identify entities with incomplete observed data
  entities_with_incomplete_observed <- character(0)
  for (g in unique_groups_orig) {
    g_char <- as.character(g)
    nominal_times_this_group <- unique(time_orig[group_orig == g])
    obs_times_this_group <- unique_time_sets_obs[[g_char]]
    if (
      length(nominal_times_this_group) > 0 &&
        length(obs_times_this_group) < length(nominal_times_this_group)
    ) {
      entities_with_incomplete_observed <- c(
        entities_with_incomplete_observed,
        g
      )
    }
  }
  if (
    length(entities_with_incomplete_observed) > 0 &&
      !is.character(unique_groups_orig)
  ) {
    entities_with_incomplete_observed <- unique_groups_orig[sapply(
      entities_with_incomplete_observed,
      function(x) which(as.character(unique_groups_orig) == x)
    )]
  }

  # --- TIME SEQUENCE TEST (based on observed time points) ---
  time_is_numeric <- is.numeric(time_orig) ||
    inherits(time_orig, "Date") ||
    inherits(time_orig, "POSIXt")
  has_irregular_observed_time_sequence <- FALSE
  observed_time_points <- NULL
  observed_time_intervals <- NULL

  if (time_is_numeric) {
    # Observed time points (where at least one entity has observed data)
    observed_time_points <- sort(unique(time_orig[has_observed]))
    if (length(observed_time_points) > 1) {
      observed_time_intervals <- diff(observed_time_points)
      if (length(unique(observed_time_intervals)) > 1) {
        has_irregular_observed_time_sequence <- TRUE
        time_seq_message <- "Irregular observed time sequence in entire panel"
        time_seq_status <- "FAIL"
      } else {
        time_seq_message <- "Regular observed time sequence in entire panel"
        time_seq_status <- "PASS"
      }
    } else if (length(observed_time_points) == 1) {
      time_seq_message <- "Only one observed time point – sequence regularity not applicable"
      time_seq_status <- "INFO"
    } else {
      time_seq_message <- "No observed time points"
      time_seq_status <- "INFO"
    }
  } else {
    time_seq_message <- "Time variable is not numeric/Date-like"
    time_seq_status <- "INFO"
  }

  # --- GROUP INTERVALS TEST (based on observed time points per group) ---
  has_irregular_observed_intervals <- FALSE
  irregular_observed_interval_groups <- vector("list", 0)
  group_observed_interval_details <- list()

  if (time_is_numeric) {
    for (grp_val in unique_groups_orig) {
      grp_char <- as.character(grp_val)
      # Observed times for this group
      obs_times <- sort(unique(time_orig[group_orig == grp_val & has_observed]))
      if (length(obs_times) > 1) {
        intervals <- diff(obs_times)
        if (length(unique(intervals)) > 1) {
          has_irregular_observed_intervals <- TRUE
          irregular_observed_interval_groups <- c(
            irregular_observed_interval_groups,
            grp_val
          )
          group_observed_interval_details[[grp_char]] <- list(
            times = obs_times,
            intervals = intervals,
            is_regular = FALSE
          )
        } else {
          group_observed_interval_details[[grp_char]] <- list(
            times = obs_times,
            intervals = intervals,
            is_regular = TRUE
          )
        }
      } else {
        group_observed_interval_details[[grp_char]] <- list(
          times = obs_times,
          intervals = numeric(0),
          is_regular = TRUE
        )
      }
    }

    if (has_irregular_observed_intervals) {
      group_interval_message <- "Irregular observed time intervals within groups"
      group_interval_status <- "FAIL"
    } else {
      group_interval_message <- "Regular observed time intervals within groups"
      group_interval_status <- "PASS"
    }
  } else {
    group_interval_message <- "Time variable is not numeric/Date-like"
    group_interval_status <- "INFO"
  }

  # --- (Other tests remain unchanged) ---

  # Check for irregular time sequence in entire panel (original nominal – kept for reference)
  # We'll keep the nominal check in details but not in main test results.

  # Calculate observations per group
  group_table <- table(group_orig)
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
      status = ifelse(any(is.na(group_orig)), "WARNING", "PASS"),
      message = ifelse(
        any(is.na(group_orig)),
        paste(
          "Group variable has",
          sum(is.na(group_orig)),
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
      status = ifelse(any(is.na(time_orig)), "WARNING", "PASS"),
      message = ifelse(
        any(is.na(time_orig)),
        paste("Time variable has", sum(is.na(time_orig)), "missing values"),
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

  # Balance test row
  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Balance",
      status = balance_status,
      message = balance_message,
      stringsAsFactors = FALSE
    )
  )

  # Time sequence test row (observed-based)
  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Time sequence",
      status = time_seq_status,
      message = time_seq_message,
      stringsAsFactors = FALSE
    )
  )

  # Group intervals test row (observed-based)
  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Group intervals",
      status = group_interval_status,
      message = group_interval_message,
      stringsAsFactors = FALSE
    )
  )

  # Determine overall status
  overall_status <- ifelse(
    any(exploration_results$status %in% c("FAIL", "WARNING")),
    ifelse(any(exploration_results$status == "FAIL"), "FAIL", "WARNING"),
    "PASS"
  )

  # Create details list
  details_list <- list()

  if (any(is.na(group_orig))) {
    details_list$missing_group_values <- which(is.na(group_orig))
  }

  if (any(is.na(time_orig))) {
    details_list$missing_time_values <- which(is.na(time_orig))
  }

  if (has_duplicates) {
    details_list$duplicate_observations <- duplicate_indices
    details_list$duplicate_combinations <- duplicate_combos
  }

  if (!is_balanced_observed && any(obs_set_lengths > 0)) {
    details_list$unbalanced_groups <- unique_groups_orig[
      !sapply(unique_time_sets_obs, function(x) identical(x, ref_set))
    ]
  }

  if (length(entities_with_incomplete_observed) > 0) {
    details_list$entities_with_incomplete_observed <- entities_with_incomplete_observed
  }

  if (time_is_numeric) {
    details_list$observed_time_points <- observed_time_points
    if (has_irregular_observed_time_sequence) {
      details_list$irregular_observed_time_intervals <- observed_time_intervals
    }
    if (has_irregular_observed_intervals) {
      details_list$irregular_observed_interval_groups <- irregular_observed_interval_groups
      details_list$group_observed_interval_details <- group_observed_interval_details
    }
  }

  # Logical summaries
  details_list$group_completeness <- !any(is.na(group_orig))
  details_list$time_completeness <- !any(is.na(time_orig))
  details_list$no_duplicates <- !has_duplicates
  details_list$balance <- is_balanced_observed
  details_list$time_sequence <- !has_irregular_observed_time_sequence
  details_list$group_intervals <- !has_irregular_observed_intervals

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

  # Set attributes
  attr(final_results, "metadata") <- metadata
  attr(final_results, "details") <- details_list

  # Set class
  class(final_results) <- c("panel_description", "data.frame")

  return(final_results)
}
