#' Panel Data Balance Description
#'
#' Provides simplified summary statistics for panel data structure with focus on balance
#' and data completeness.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#' @param presence A character string specifying how to define entity presence: "nominal", "observed", or "complete".
#'        Default = "observed".
#' @param digits An integer specifying the number of decimal places for rounding mean values.
#'        Default = 3.
#'
#' @return A data.frame with 5 columns and 3 rows containing panel data summary statistics.
#'
#' \strong{Columns:}
#' \describe{
#'   \item{\code{dimension}}{Character vector describing the type of panel element.
#'     Contains three values: "rows", "entities", and "periods".}
#'   \item{\code{count}}{Numeric vector with total counts for each panel element.
#'     For "rows": number of rows meeting the presence criteria.
#'     For "entities": number of entities where ALL time periods have presence according to the presence criteria.
#'     For "periods": number of time periods where ALL entities have presence according to the presence criteria.}
#'   \item{\code{mean}}{Numeric vector with mean counts.
#'     For "rows": not applicable (NA).
#'     For "entities": mean number of time periods per entity meeting the presence criteria.
#'     For "periods": mean number of entities per period meeting the presence criteria.}
#'   \item{\code{min}}{Numeric vector with minimum counts.
#'     For "rows": not applicable (NA).
#'     For "entities": minimum number of time periods per entity meeting the presence criteria.
#'     For "periods": minimum number of entities per period meeting the presence criteria.}
#'   \item{\code{max}}{Numeric vector with maximum counts.
#'     For "rows": not applicable (NA).
#'     For "entities": maximum number of time periods per entity meeting the presence criteria.
#'     For "periods": maximum number of entities per period meeting the presence criteria.}
#' }
#'
#' \strong{Presence parameter definitions:}
#' \describe{
#'   \item{\code{"nominal"}}{Entity/time is present if it has a row in the data (even with only panel ID variables)}
#'   \item{\code{"observed"}}{Entity is present if it has at least one non-NA substantive variable (default)}
#'   \item{\code{"complete"}}{Entity/time is present only if it has no NA values in all substantive variables}
#' }
#'
#' The data.frame has additional attributes:
#' \describe{
#'   \item{\code{panel_group}}{The grouping variable name}
#'   \item{\code{panel_time}}{The time variable name}
#'   \item{\code{panel_presence}}{Presence type ("nominal", "observed", or "complete")}
#'   \item{\code{panel_digits}}{Number of decimal places used for rounding}
#'   \item{\code{panel_n_entities}}{Total number of unique entities/groups}
#'   \item{\code{panel_n_periods}}{Total number of unique time periods}
#'   \item{\code{panel_total_rows}}{Total number of rows in the data}
#'   \item{\code{panel_entities}}{Vector of unique entity IDs}
#'   \item{\code{panel_periods}}{Vector of unique time period IDs}
#'   \item{\code{panel_matrix}}{Binary matrix (entities × periods) showing presence (1) or absence (0) according to the specified presence type}
#' }
#'
#' @details
#' This function provides basic panel data structure information including
#' the number of unique entities, time periods, and total rows.
#' When provided with a data.frame that has panel attributes (created by set_panel()),
#' the function automatically extracts group and time variable names from the attributes.
#'
#' @seealso
#' [describe_periods()], [describe_patterns()], [check_panel()]
#'
#' @examples
#' data(production)
#' describe_balance(production, group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' describe_balance(panel_data)
#'
#' # Use different presence types
#' describe_balance(production, group = "firm", time = "year", presence = "nominal")
#' describe_balance(production, group = "firm", time = "year", presence = "complete")
#'
#' # With custom rounding
#' describe_balance(production, group = "firm", time = "year", digits = 4)
#'
#' # Effectively no rounding (use large digit value)
#' describe_balance(production, group = "firm", time = "year", digits = 999999)
#'
#' @export
describe_balance <- function(
  data,
  group = NULL,
  time = NULL,
  presence = "observed",
  digits = 3
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

  if (!is.character(presence) || length(presence) != 1) {
    stop("'presence' must be a single character string")
  }

  if (!presence %in% c("nominal", "observed", "complete")) {
    stop('presence must be one of: "nominal", "observed", "complete"')
  }

  # Harmonized digits validation
  if (!is.numeric(digits) || length(digits) != 1) {
    stop("'digits' must be a single non-negative integer")
  }
  if (digits < 0 || digits != round(digits)) {
    stop("'digits' must be a non-negative integer")
  }
  digits <- as.integer(digits)

  # Get substantive variables (all except group and time)
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Get all unique groups and periods from the data
  all_groups <- unique(as.character(data[[group]]))
  all_times <- unique(as.character(data[[time]]))

  # Sort time periods if they appear numeric
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", all_times))) {
    all_times <- as.character(sort(as.numeric(all_times)))
  } else {
    all_times <- sort(all_times)
  }

  total_entities <- length(all_groups)
  total_periods <- length(all_times)
  total_rows <- nrow(data)

  # Create presence matrix (initialize with 0)
  presence_matrix <- matrix(
    0,
    nrow = total_entities,
    ncol = total_periods,
    dimnames = list(all_groups, all_times)
  )

  # Convert to character for consistent handling
  group_vec <- as.character(data[[group]])
  time_vec <- as.character(data[[time]])

  # Fill presence matrix based on presence
  if (presence == "nominal") {
    # For nominal type, any row presence counts
    for (i in seq_along(group_vec)) {
      row_idx <- which(all_groups == group_vec[i])
      col_idx <- which(all_times == time_vec[i])
      presence_matrix[row_idx, col_idx] <- 1
    }
  } else if (presence == "observed") {
    # For observed type, need to check if at least one substantive variable is non-NA
    has_at_least_one_non_na <- apply(
      data[substantive_vars],
      1,
      function(x) {
        any(!is.na(x))
      }
    )

    for (i in seq_along(group_vec)) {
      if (has_at_least_one_non_na[i]) {
        row_idx <- which(all_groups == group_vec[i])
        col_idx <- which(all_times == time_vec[i])
        presence_matrix[row_idx, col_idx] <- 1
      }
    }
  } else if (presence == "complete") {
    # For complete type, need to check if all substantive variables are non-NA
    has_no_na <- apply(data[substantive_vars], 1, function(x) {
      all(!is.na(x))
    })

    for (i in seq_along(group_vec)) {
      if (has_no_na[i]) {
        row_idx <- which(all_groups == group_vec[i])
        col_idx <- which(all_times == time_vec[i])
        presence_matrix[row_idx, col_idx] <- 1
      }
    }
  }

  # Calculate statistics for output data.frame

  # 1. Rows
  if (presence == "nominal") {
    row_count <- total_rows
  } else if (presence == "observed") {
    row_count <- sum(apply(data[substantive_vars], 1, function(x) {
      any(!is.na(x))
    }))
  } else if (presence == "complete") {
    row_count <- sum(apply(data[substantive_vars], 1, function(x) {
      all(!is.na(x))
    }))
  }

  # 2. Entities
  # Calculate per-entity statistics based on presence_matrix
  per_entity_counts <- rowSums(presence_matrix)

  # Count of entities: entities present in ALL periods according to presence
  entity_count <- sum(per_entity_counts == total_periods)

  # Helper function for rounding
  round_if_needed <- function(x, digits) {
    if (is.numeric(x) && !all(is.na(x))) {
      round(x, digits)
    } else {
      x
    }
  }

  # Mean, min, max: statistics for entities with at least some presence
  mean_entities <- if (sum(per_entity_counts > 0) > 0) {
    round_if_needed(
      mean(per_entity_counts[per_entity_counts > 0], na.rm = TRUE),
      digits
    )
  } else {
    NA
  }

  min_entities <- if (sum(per_entity_counts > 0) > 0) {
    min(per_entity_counts[per_entity_counts > 0])
  } else {
    NA
  }

  max_entities <- if (sum(per_entity_counts > 0) > 0) {
    max(per_entity_counts[per_entity_counts > 0])
  } else {
    NA
  }

  # 3. Periods
  # Calculate per-period statistics based on presence_matrix
  per_period_counts <- colSums(presence_matrix)

  # Count of periods: periods where ALL entities have presence according to presence
  period_count <- sum(per_period_counts == total_entities)

  # Mean, min, max: statistics for periods with at least some presence
  mean_periods <- if (sum(per_period_counts > 0) > 0) {
    round_if_needed(
      mean(per_period_counts[per_period_counts > 0], na.rm = TRUE),
      digits
    )
  } else {
    NA
  }

  min_periods <- if (sum(per_period_counts > 0) > 0) {
    min(per_period_counts[per_period_counts > 0])
  } else {
    NA
  }

  max_periods <- if (sum(per_period_counts > 0) > 0) {
    max(per_period_counts[per_period_counts > 0])
  } else {
    NA
  }

  # Create and return the simplified result data.frame
  result_df <- data.frame(
    dimension = c("rows", "entities", "periods"),
    count = c(row_count, entity_count, period_count),
    mean = c(NA, mean_entities, mean_periods),
    min = c(NA, min_entities, min_periods),
    max = c(NA, max_entities, max_periods),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Add standardized attributes
  attr(result_df, "panel_group") <- group
  attr(result_df, "panel_time") <- time
  attr(result_df, "panel_presence") <- presence
  attr(result_df, "panel_digits") <- digits
  attr(result_df, "panel_n_entities") <- total_entities
  attr(result_df, "panel_n_periods") <- total_periods
  attr(result_df, "panel_total_rows") <- total_rows
  attr(result_df, "panel_entities") <- all_groups
  attr(result_df, "panel_periods") <- all_times
  attr(result_df, "panel_matrix") <- presence_matrix

  return(result_df)
}
