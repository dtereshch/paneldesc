#' Panel Data Balance Exploration
#'
#' This function provides detailed analysis of panel data balance, including
#' information about balanced entities, balanced time periods, and patterns
#' of missing values.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable.
#' @param time A character string specifying the name of the time variable.
#'
#' @return A list containing the following components:
#'   \item{summary}{A list with summary statistics}
#'   \item{balanced_periods}{Vector of time periods where all entities are present}
#'   \item{periods_without_na}{Vector of time periods with no missing values in substantive variables}
#'   \item{balanced_entities}{Vector of entities present in all time periods}
#'   \item{entities_without_na}{Vector of entities with no missing values in substantive variables}
#'   \item{presence_matrix}{Binary matrix showing entity-time presence (1=present, 0=missing)}
#' The result is returned invisibly, and will be printed automatically unless assigned to a variable.
#'
#'
#' @details
#' The function analyzes panel data balance from multiple perspectives:
#' \enumerate{
#'   \item \strong{Overall data quality}: Counts of observations with/without NAs
#'   \item \strong{Time periods}: Which periods have all entities, which have no NAs
#'   \item \strong{Entities}: Which entities are present in all periods, which have no NAs
#' }
#'
#' @examples
#' data(production)
#'
#' # Basic usage - will print automatically
#' explore_balance(production, group = "firm", time = "year")
#'
#' # Access returned vectors for further analysis - won't print
#' result <- explore_balance(production, group = "firm", time = "year")
#'
#' # Balanced periods (all entities present)
#' balanced_periods <- result$balanced_periods
#' print(balanced_periods)
#'
#' # Time periods with no missing values
#' clean_periods <- result$periods_without_na
#' print(clean_periods)
#'
#' # Balanced entities (present in all time periods)
#' complete_entities <- result$balanced_entities
#' print(complete_entities)
#'
#' # Entities with no missing values
#' clean_entities <- result$entities_without_na
#' print(clean_entities)
#'
#' # Create a balanced subset
#' balanced_data <- production[production$firm %in% complete_entities, ]
#'
#' # Create a complete subset (balanced and no NAs)
#' complete_data <- production[
#'   production$firm %in% complete_entities &
#'   production$year %in% clean_periods,
#' ]
#'
#' @seealso
#' [check_panel()] for basic panel structure validation,
#' [describe_balance()] for summary statistics,
#' [explore_participation()] for participation pattern analysis
#'
#' @export
explore_balance <- function(data, group, time) {
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

  # Count observations with/without NAs in substantive variables
  has_any_na <- apply(data[substantive_vars], 1, function(x) any(is.na(x)))
  obs_without_na <- sum(!has_any_na)
  obs_with_na <- sum(has_any_na)

  # Create presence matrix (1 = observation exists for entity-time pair)
  presence_matrix <- matrix(
    0,
    nrow = length(unique_groups),
    ncol = length(unique_times),
    dimnames = list(unique_groups, unique_times)
  )

  # Fill presence matrix
  for (i in seq_along(group_vec)) {
    row_idx <- which(unique_groups == group_vec[i])
    col_idx <- which(unique_times == time_vec[i])
    presence_matrix[row_idx, col_idx] <- 1
  }

  # Create NA matrix for substantive variables (1 = no NA for entity-time pair)
  na_matrix <- matrix(
    0,
    nrow = length(unique_groups),
    ncol = length(unique_times),
    dimnames = list(unique_groups, unique_times)
  )

  # Fill NA matrix
  for (i in seq_along(group_vec)) {
    row_idx <- which(unique_groups == group_vec[i])
    col_idx <- which(unique_times == time_vec[i])
    # Check if this observation has no NAs in substantive variables
    if (!has_any_na[i]) {
      na_matrix[row_idx, col_idx] <- 1
    }
  }

  # Calculate statistics
  # 1. Time periods
  n_periods <- length(unique_times)

  # Observations per entity (from presence matrix)
  obs_per_entity <- rowSums(presence_matrix)
  min_obs_per_entity <- min(obs_per_entity)
  max_obs_per_entity <- max(obs_per_entity)
  avg_obs_per_entity <- mean(obs_per_entity)

  # Balanced time periods (all entities present)
  balanced_periods <- unique_times[
    colSums(presence_matrix) == length(unique_groups)
  ]
  n_balanced_periods <- length(balanced_periods)

  # Time periods without NAs (all observations have no NAs)
  periods_without_na <- unique_times[
    colSums(na_matrix) == colSums(presence_matrix)
  ]
  n_periods_without_na <- length(periods_without_na)

  # 2. Entities
  n_entities <- length(unique_groups)

  # Observations per time period
  obs_per_period <- colSums(presence_matrix)
  min_obs_per_period <- min(obs_per_period)
  max_obs_per_period <- max(obs_per_period)
  avg_obs_per_period <- mean(obs_per_period)

  # Balanced entities (present in all time periods)
  balanced_entities <- unique_groups[rowSums(presence_matrix) == n_periods]
  n_balanced_entities <- length(balanced_entities)
  pct_balanced_entities <- ifelse(
    n_entities > 0,
    round(n_balanced_entities / n_entities * 100, 1),
    0
  )

  # Entities without NAs (all observations have no NAs)
  # Alternative calculation for entities without NAs
  entities_without_na <- c()
  for (i in seq_along(unique_groups)) {
    entity <- unique_groups[i]
    entity_present <- which(presence_matrix[i, ] == 1)
    entity_no_na <- which(na_matrix[i, ] == 1)
    if (length(entity_present) > 0 && all(entity_present %in% entity_no_na)) {
      entities_without_na <- c(entities_without_na, entity)
    }
  }
  n_entities_without_na <- length(entities_without_na)
  pct_entities_without_na <- ifelse(
    n_entities > 0,
    round(n_entities_without_na / n_entities * 100, 1),
    0
  )

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
    # Print formatted output
    cat("Panel Data Balance Check\n")
    cat("===========================================\n\n")

    cat("Basic Information\n")
    cat("--------------------------------------------------------------\n")
    cat(sprintf("  Total observations: %d \n", total_obs))
    cat(sprintf(
      "  Observations without NAs: %d (%.1f%%) \n",
      obs_without_na,
      ifelse(total_obs > 0, obs_without_na / total_obs * 100, 0)
    ))
    cat(sprintf(
      "  Observations with NAs: %d (%.1f%%) \n\n",
      obs_with_na,
      ifelse(total_obs > 0, obs_with_na / total_obs * 100, 0)
    ))

    cat("Time Periods\n")
    cat("--------------------------------------------------------------\n")
    cat(sprintf("  Number of time periods: %d \n", n_periods))
    cat(sprintf(
      "  Observations per entity: %d - %d (avg: %.1f )\n",
      min_obs_per_entity,
      max_obs_per_entity,
      avg_obs_per_entity
    ))
    cat(sprintf("  Number of balanced time periods: %d\n", n_balanced_periods))
    cat(sprintf(
      "  Number of time periods without NAs: %d\n\n",
      n_periods_without_na
    ))

    cat("Entities\n")
    cat("--------------------------------------------------------------\n")
    cat(sprintf("  Number of entities: %d \n", n_entities))
    cat(sprintf(
      "  Observations per time period: %d - %d (avg: %.1f )\n",
      min_obs_per_period,
      max_obs_per_period,
      avg_obs_per_period
    ))
    cat(sprintf(
      "  Number of balanced entities: %d (%.1f%%)\n",
      n_balanced_entities,
      pct_balanced_entities
    ))
    cat(sprintf(
      "  Number of entities without NAs: %d (%.1f%%)\n",
      n_entities_without_na,
      pct_entities_without_na
    ))
    cat("\n")
  }

  # Create invisible return object
  result <- list(
    summary = list(
      total_observations = total_obs,
      observations_without_na = obs_without_na,
      observations_with_na = obs_with_na,
      n_time_periods = n_periods,
      min_obs_per_entity = min_obs_per_entity,
      max_obs_per_entity = max_obs_per_entity,
      avg_obs_per_entity = avg_obs_per_entity,
      n_balanced_periods = n_balanced_periods,
      n_periods_without_na = n_periods_without_na,
      n_entities = n_entities,
      min_obs_per_period = min_obs_per_period,
      max_obs_per_period = max_obs_per_period,
      avg_obs_per_period = avg_obs_per_period,
      n_balanced_entities = n_balanced_entities,
      pct_balanced_entities = pct_balanced_entities,
      n_entities_without_na = n_entities_without_na,
      pct_entities_without_na = pct_entities_without_na
    ),
    balanced_periods = balanced_periods,
    periods_without_na = periods_without_na,
    balanced_entities = balanced_entities,
    entities_without_na = entities_without_na,
    presence_matrix = presence_matrix,
    na_matrix = na_matrix,
    group_var = group,
    time_var = time
  )

  class(result) <- "balance_exploration"
  invisible(result)
}
