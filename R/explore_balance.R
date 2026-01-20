#' Panel Data Balance Exploration
#'
#' This function provides detailed analysis of panel data balance, including
#' information about balanced entities, balanced time periods, and patterns
#' of missing values.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable.
#' @param time A character string specifying the name of the time variable.
#' @param print_result A logical flag indicating whether to print the validation results.
#' Default = TRUE.
#'
#' @return A list with panel balance analysis results.
#'
#' @details
#' The returned list contains the following components:
#' \describe{
#'   \item{\code{summary}}{List with summary statistics including:
#'     \itemize{
#'       \item \code{total_observations}: Total number of rows
#'       \item \code{observations_balanced}: Rows with at least one non-NA substantive variable
#'       \item \code{observations_complete}: Rows with no NAs in substantive variables
#'       \item \code{n_time_periods}: Number of unique time periods
#'       \item \code{n_balanced_periods}: Periods where all entities have at least one non-NA
#'       \item \code{n_periods_complete}: Periods with no NAs in any observation
#'       \item \code{n_entities}: Number of unique groups
#'       \item \code{n_balanced_entities}: Entities where all periods have at least one non-NA
#'       \item \code{n_entities_complete}: Entities with no NAs in any observation
#'     }
#'   }
#'   \item{\code{balanced_periods}}{Vector of time periods where all entities have at least one non-NA}
#'   \item{\code{periods_complete}}{Vector of time periods with no missing values}
#'   \item{\code{balanced_entities}}{Vector of entities where all time periods have at least one non-NA}
#'   \item{\code{entities_complete}}{Vector of entities with no missing values}
#'   \item{\code{presence_matrix}}{Binary matrix (entities × periods) showing presence (1) or absence (0)}
#'   \item{\code{balanced_matrix}}{Binary matrix showing which entity-period pairs have at least one non-NA}
#'   \item{\code{complete_matrix}}{Binary matrix showing which entity-period combinations have no NAs}
#'   \item{\code{group_var}}{The group variable name}
#'   \item{\code{time_var}}{The time variable name}
#' }
#'
#' @seealso
#' [describe_balance()], [explore_participation()], [explore_panel()], [describe_periods()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage (prints by default)
#' explore_balance(production, group = "firm", time = "year")
#'
#' # Assign the results without printing
#' balance_result <- explore_balance(production, group = "firm", time = "year", print_result = FALSE)
#'
#' # Balanced periods (all entities have at least one non-NA)
#' balanced_periods <- balance_result$balanced_periods
#' print(balanced_periods)
#'
#' # Time periods with no missing values
#' complete_periods <- balance_result$periods_complete
#' print(complete_periods)
#'
#' # Balanced entities (all periods have at least one non-NA)
#' balanced_entities <- balance_result$balanced_entities
#' print(balanced_entities)
#'
#' # Entities with no missing values
#' complete_entities <- balance_result$entities_complete
#' print(complete_entities)
#'
#' # Create a balanced subset
#' balanced_data <- production[production$firm %in% balanced_entities, ]
#'
#' # Create a complete subset (balanced and no NAs)
#' complete_data <- production[
#'   production$firm %in% balanced_entities &
#'   production$year %in% complete_periods,
#' ]
#'
#' @export
explore_balance <- function(
  data,
  group,
  time,
  print_result = TRUE
) {
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

  if (!is.logical(print_result) || length(print_result) != 1) {
    stop(
      "'print_result' must be a single logical value, not ",
      class(print_result)[1]
    )
  }

  # Get substantive variables (all except group and time)
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Convert to character for consistent handling
  group_vec <- as.character(data[[group]])
  time_vec <- as.character(data[[time]])

  # Get unique groups and periods
  unique_groups <- unique(group_vec)
  unique_times <- unique(time_vec)

  # Sort time periods if they appear numeric
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", unique_times))) {
    unique_times <- sort(as.numeric(unique_times))
    unique_times <- as.character(unique_times)
  } else {
    unique_times <- sort(unique_times)
  }

  # Total observations
  total_obs <- nrow(data)

  # Pre-compute row statistics
  # Balanced: at least one non-NA in substantive variables
  has_at_least_one_non_na <- apply(data[substantive_vars], 1, function(x) {
    any(!is.na(x))
  })

  # Complete: no NAs in substantive variables
  has_no_na <- apply(data[substantive_vars], 1, function(x) {
    all(!is.na(x))
  })

  obs_balanced <- sum(has_at_least_one_non_na)
  obs_complete <- sum(has_no_na)

  # Create matrices
  n_entities <- length(unique_groups)
  n_periods <- length(unique_times)

  # Presence matrix (1 = observation exists for entity-time pair)
  presence_matrix <- matrix(
    0,
    nrow = n_entities,
    ncol = n_periods,
    dimnames = list(unique_groups, unique_times)
  )

  # Balanced matrix (1 = observation has at least one non-NA substantive variable)
  balanced_matrix <- matrix(
    0,
    nrow = n_entities,
    ncol = n_periods,
    dimnames = list(unique_groups, unique_times)
  )

  # Complete matrix (1 = observation has no NAs in substantive variables)
  complete_matrix <- matrix(
    0,
    nrow = n_entities,
    ncol = n_periods,
    dimnames = list(unique_groups, unique_times)
  )

  # Fill all matrices
  for (i in seq_along(group_vec)) {
    row_idx <- which(unique_groups == group_vec[i])
    col_idx <- which(unique_times == time_vec[i])

    # Presence: always 1 if row exists
    presence_matrix[row_idx, col_idx] <- 1

    # Balanced: at least one non-NA
    if (has_at_least_one_non_na[i]) {
      balanced_matrix[row_idx, col_idx] <- 1
    }

    # Complete: no NAs
    if (has_no_na[i]) {
      complete_matrix[row_idx, col_idx] <- 1
    }
  }

  # Calculate statistics
  # 1. Time periods
  # Observations per entity (from presence matrix)
  obs_per_entity <- rowSums(presence_matrix)
  min_obs_per_entity <- min(obs_per_entity)
  max_obs_per_entity <- max(obs_per_entity)
  avg_obs_per_entity <- mean(obs_per_entity)

  # Balanced time periods (all entities have at least one non-NA)
  balanced_periods <- unique_times[
    colSums(balanced_matrix) == n_entities
  ]
  n_balanced_periods <- length(balanced_periods)

  # Complete time periods (all observations have no NAs)
  complete_periods <- unique_times[
    colSums(complete_matrix) == colSums(presence_matrix)
  ]
  n_periods_complete <- length(complete_periods)

  # 2. Entities
  # Observations per time period
  obs_per_period <- colSums(presence_matrix)
  min_obs_per_period <- min(obs_per_period)
  max_obs_per_period <- max(obs_per_period)
  avg_obs_per_period <- mean(obs_per_period)

  # Balanced entities (all periods have at least one non-NA)
  balanced_entities <- unique_groups[rowSums(balanced_matrix) == n_periods]
  n_balanced_entities <- length(balanced_entities)
  pct_balanced_entities <- ifelse(
    n_entities > 0,
    round(n_balanced_entities / n_entities * 100, 1),
    0
  )

  # Complete entities (all observations have no NAs)
  complete_entities <- c()
  for (i in seq_along(unique_groups)) {
    entity <- unique_groups[i]
    entity_present <- which(presence_matrix[i, ] == 1)
    entity_complete <- which(complete_matrix[i, ] == 1)
    if (
      length(entity_present) > 0 && all(entity_present %in% entity_complete)
    ) {
      complete_entities <- c(complete_entities, entity)
    }
  }
  n_entities_complete <- length(complete_entities)
  pct_entities_complete <- ifelse(
    n_entities > 0,
    round(n_entities_complete / n_entities * 100, 1),
    0
  )

  # Create invisible return object
  result <- list(
    summary = list(
      total_observations = total_obs,
      observations_balanced = obs_balanced,
      observations_complete = obs_complete,
      n_time_periods = n_periods,
      min_obs_per_entity = min_obs_per_entity,
      max_obs_per_entity = max_obs_per_entity,
      avg_obs_per_entity = avg_obs_per_entity,
      n_balanced_periods = n_balanced_periods,
      n_periods_complete = n_periods_complete,
      n_entities = n_entities,
      min_obs_per_period = min_obs_per_period,
      max_obs_per_period = max_obs_per_period,
      avg_obs_per_period = avg_obs_per_period,
      n_balanced_entities = n_balanced_entities,
      pct_balanced_entities = pct_balanced_entities,
      n_entities_complete = n_entities_complete,
      pct_entities_complete = pct_entities_complete
    ),
    balanced_periods = balanced_periods,
    periods_complete = complete_periods,
    balanced_entities = balanced_entities,
    entities_complete = complete_entities,
    presence_matrix = presence_matrix,
    balanced_matrix = balanced_matrix,
    complete_matrix = complete_matrix,
    group_var = group,
    time_var = time
  )

  class(result) <- "balance_exploration"

  # Print if requested
  if (print_result) {
    cat("PANEL DATA BALANCE EXPLORATION\n")
    cat("====================================================\n\n")

    # Calculate percentages
    pct_balanced_obs <- ifelse(total_obs > 0, obs_balanced / total_obs * 100, 0)
    pct_complete_obs <- ifelse(total_obs > 0, obs_complete / total_obs * 100, 0)

    # SIMPLIFIED OUTPUT - Numbers first, then descriptions
    cat("BASIC INFORMATION\n")
    cat("----------------------------------------------------\n")
    cat(sprintf("%6d  Total observations\n", total_obs))
    cat(sprintf(
      "%6d  Observations with ≥1 non-NA value (%5.1f%%)\n",
      obs_balanced,
      pct_balanced_obs
    ))
    cat(sprintf(
      "%6d  Observations with no missing values (%5.1f%%)\n",
      obs_complete,
      pct_complete_obs
    ))
    cat("\n")

    cat("TIME PERIODS\n")
    cat("----------------------------------------------------\n")
    cat(sprintf("%6d  Time periods\n", n_periods))
    cat(sprintf("%6d  Minimum observations per entity\n", min_obs_per_entity))
    cat(sprintf("%6d  Maximum observations per entity\n", max_obs_per_entity))
    cat(sprintf("%6.1f  Average observations per entity\n", avg_obs_per_entity))
    cat(sprintf(
      "%6d  Periods where all entities have ≥1 non-NA\n",
      n_balanced_periods
    ))
    cat(sprintf("%6d  Periods with no missing values\n", n_periods_complete))
    cat("\n")

    cat("ENTITIES\n")
    cat("----------------------------------------------------\n")
    cat(sprintf("%6d  Entities\n", n_entities))
    cat(sprintf("%6d  Minimum observations per period\n", min_obs_per_period))
    cat(sprintf("%6d  Maximum observations per period\n", max_obs_per_period))
    cat(sprintf("%6.1f  Average observations per period\n", avg_obs_per_period))
    cat(sprintf(
      "%6d  Balanced entities (%5.1f%%)\n",
      n_balanced_entities,
      pct_balanced_entities
    ))
    cat(sprintf(
      "%6d  Complete entities (%5.1f%%)\n",
      n_entities_complete,
      pct_entities_complete
    ))
    cat("\n")
  }

  invisible(result)
}
