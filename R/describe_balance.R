#' Panel Data Balance Description
#'
#' Provides simplified summary statistics for panel data structure with focus on balance
#' and data completeness.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable.
#'              Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'             Not required if data has panel attributes.
#'
#' @return A data.frame with 4 columns and 3 rows containing panel data summary statistics:
#'
#' \strong{Columns:}
#' \describe{
#'   \item{\code{panel_info}}{Character vector describing the type of panel element.
#'     Contains three values: "observations", "entities", and "periods".}
#'   \item{\code{total}}{Numeric vector with total counts for each panel element.
#'     For "observations": total number of rows in the original data.
#'     For "entities": total number of unique groups present in the data.
#'     For "periods": total number of unique time periods present in the data.}
#'   \item{\code{balanced}}{Numeric vector with counts of balanced cases.
#'     For "observations": number of rows with at least one substantive variable that is not NA.
#'     For "entities": number of groups where all time periods have at least one substantive variable that is not NA.
#'     For "periods": number of time periods where all groups have at least one substantive variable that is not NA.}
#'   \item{\code{complete}}{Numeric vector with counts of cases without missing values.
#'     For "observations": number of rows with no NAs in substantive variables.
#'     For "entities": number of groups with no NAs in any of their observations.
#'     For "periods": number of time periods with no NAs in any observation.}
#' }
#'
#' The data.frame has additional attributes:
#' \describe{
#'   \item{\code{panel_group}}{The grouping variable name}
#'   \item{\code{panel_time}}{The time variable name}
#'   \item{\code{panel_n_entities}}{Total number of unique entities/groups}
#'   \item{\code{panel_n_periods}}{Total number of unique time periods}
#'   \item{\code{panel_total_obs}}{Total number of observations in the data}
#' }
#'
#' @details
#' This function provides basic panel data structure information including
#' the number of unique entities, time periods, and total observations.
#' When provided with a data.frame that has panel attributes (created by set_panel()),
#' the function automatically extracts group and time variable names from the attributes.
#'
#' @seealso
#' [explore_balance()], [describe_periods()], [describe_participation()]
#'
#' @examples
#' data(production)
#' describe_balance(production, group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' describe_balance(panel_data)
#'
#' @export
describe_balance <- function(data, group = NULL, time = NULL) {
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

  # Common validation for both cases
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string")
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string")
  }

  if (!time %in% names(data)) {
    stop('variable "', time, '" not found in data')
  }

  # Get substantive variables (all except group and time)
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Use original data for balance analysis (structural completeness)
  data_for_balance <- data

  # Get unique groups and periods from ALL data
  unique_groups <- unique(as.character(data_for_balance[[group]]))
  unique_periods <- unique(as.character(data_for_balance[[time]]))

  # Sort time periods if they appear numeric (matching explore_balance behavior)
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", unique_periods))) {
    unique_periods <- sort(as.numeric(unique_periods))
    unique_periods <- as.character(unique_periods)
  } else {
    unique_periods <- sort(unique_periods)
  }

  total_entities <- length(unique_groups)
  total_periods <- length(unique_periods)
  total_obs <- nrow(data)

  # Create presence matrix (1 = observation exists for entity-time pair)
  presence_matrix <- matrix(
    0,
    nrow = total_entities,
    ncol = total_periods,
    dimnames = list(unique_groups, unique_periods)
  )

  # Create balanced matrix (1 = observation has at least one non-NA substantive variable)
  balanced_matrix <- matrix(
    0,
    nrow = total_entities,
    ncol = total_periods,
    dimnames = list(unique_groups, unique_periods)
  )

  # Create NA matrix (1 = observation has no NAs in substantive variables)
  na_matrix <- matrix(
    0,
    nrow = total_entities,
    ncol = total_periods,
    dimnames = list(unique_groups, unique_periods)
  )

  # Fill presence, balanced, and NA matrices
  group_vec <- as.character(data_for_balance[[group]])
  time_vec <- as.character(data_for_balance[[time]])

  # Pre-compute which rows have at least one non-NA in substantive variables
  has_at_least_one_non_na <- apply(
    data_for_balance[substantive_vars],
    1,
    function(x) {
      any(!is.na(x))
    }
  )

  # Pre-compute which rows have no NAs in substantive variables
  has_no_na <- apply(data_for_balance[substantive_vars], 1, function(x) {
    all(!is.na(x))
  })

  for (i in seq_along(group_vec)) {
    row_idx <- which(unique_groups == group_vec[i])
    col_idx <- which(unique_periods == time_vec[i])
    presence_matrix[row_idx, col_idx] <- 1

    if (has_at_least_one_non_na[i]) {
      balanced_matrix[row_idx, col_idx] <- 1
    }

    if (has_no_na[i]) {
      na_matrix[row_idx, col_idx] <- 1
    }
  }

  # Calculate statistics for output data.frame

  # 1. Observations
  total_obs <- nrow(data)
  balanced_obs <- sum(has_at_least_one_non_na)
  complete_obs <- sum(has_no_na)

  # 2. Entities
  total_entities_count <- total_entities

  # Balanced entities (present in all periods with at least one non-NA substantive variable)
  balanced_entities_count <- sum(rowSums(balanced_matrix) == total_periods)

  # Entities without NA (all observations for entity have no NAs)
  entities_complete_count <- 0
  for (i in 1:total_entities) {
    entity_rows <- which(presence_matrix[i, ] == 1)
    if (length(entity_rows) > 0 && all(na_matrix[i, entity_rows] == 1)) {
      entities_complete_count <- entities_complete_count + 1
    }
  }

  # 3. Periods
  total_periods_count <- total_periods

  # Balanced periods (all entities present with at least one non-NA substantive variable)
  balanced_periods_count <- sum(colSums(balanced_matrix) == total_entities)

  # Periods without NA (all observations in period have no NAs)
  periods_complete_count <- 0
  for (j in 1:total_periods) {
    period_cols <- which(presence_matrix[, j] == 1)
    if (length(period_cols) > 0 && all(na_matrix[period_cols, j] == 1)) {
      periods_complete_count <- periods_complete_count + 1
    }
  }

  # Create and return the simplified result data.frame
  result_df <- data.frame(
    panel_info = c("observations", "entities", "periods"),
    total = c(total_obs, total_entities_count, total_periods_count),
    balanced = c(
      balanced_obs,
      balanced_entities_count,
      balanced_periods_count
    ),
    complete = c(
      complete_obs,
      entities_complete_count,
      periods_complete_count
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Add standardized attributes
  attr(result_df, "panel_group") <- group
  attr(result_df, "panel_time") <- time
  attr(result_df, "panel_n_entities") <- total_entities_count
  attr(result_df, "panel_n_periods") <- total_periods_count
  attr(result_df, "panel_total_obs") <- total_obs

  return(result_df)
}
