#' Panel Data Balance Description
#'
#' This function provides summary statistics for panel data structure with focus on balance
#' and data completeness.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#' @param presence A character string specifying how to define entity presence: "nominal", "observed", or "complete".
#'        Default = "observed".
#' @param detailed A logical flag indicating whether to return additional statistics (p5, p25, p50, p75, p95).
#'        Default = FALSE.
#' @param digits An integer specifying the number of decimal places for rounding mean values.
#'        Default = 3.
#'
#' @return A data.frame with panel data summary statistics for entities and periods.
#'
#' @details
#' The statistics for entities describe the distribution of entities observed per time period
#' (i.e., cross-sectional size per period), while statistics for periods describe the distribution
#' of time periods observed per entity (i.e., temporal length per entity).
#'
#' The returned data.frame contains the following columns:
#' \describe{
#'   \item{\code{dimension}}{Character vector describing the type of panel element.
#'     Contains two values: "entities" and "periods".}
#'   \item{\code{mean}}{Numeric vector with mean counts.
#'     For "entities": mean number of entities observed per time period.
#'     For "periods": mean number of time periods observed per entity.}
#'   \item{\code{std}}{Standard deviation.
#'     For "entities": standard deviation of entities per period.
#'     For "periods": standard deviation of periods per entity.}
#'   \item{\code{min}}{Numeric vector with minimum counts.
#'     For "entities": minimum number of entities observed in any period.
#'     For "periods": minimum number of periods observed for any entity.}
#'   \item{\code{max}}{Numeric vector with maximum counts.
#'     For "entities": maximum number of entities observed in any period.
#'     For "periods": maximum number of periods observed for any entity.}
#' }
#'
#' When \code{detailed = TRUE}, additional columns are included:
#' \describe{
#'   \item{\code{p5}}{5th percentile}
#'   \item{\code{p25}}{25th percentile (first quartile)}
#'   \item{\code{p50}}{50th percentile (median)}
#'   \item{\code{p75}}{75th percentile (third quartile)}
#'   \item{\code{p95}}{95th percentile}
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
#'   \item{\code{panel_detailed}}{Logical indicating detailed output}
#'   \item{\code{panel_digits}}{Number of decimal places used for rounding}
#'   \item{\code{panel_n_entities}}{Total number of unique entities/groups}
#'   \item{\code{panel_n_periods}}{Total number of unique time periods}
#'   \item{\code{panel_total_rows}}{Total number of rows in the data}
#'   \item{\code{panel_entities}}{Vector of unique entity IDs}
#'   \item{\code{panel_periods}}{Vector of unique time period IDs}
#'   \item{\code{panel_matrix}}{Binary matrix (entities × periods) showing presence (1) or absence (0) according to the specified presence type}
#' }
#'
#' @seealso
#' [describe_periods()], [describe_patterns()], [check_panel()]
#'
#' @examples
#' data(production)
#' describe_balance(production, group = "firm", time = "year")
#'
#' # With detailed output including percentiles
#' describe_balance(production, group = "firm", time = "year", detailed = TRUE)
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
  detailed = FALSE,
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

  # Validate detailed parameter
  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("'detailed' must be a single logical value, not ", class(detailed)[1])
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

  # Helper function for rounding
  round_if_needed <- function(x, digits) {
    if (is.numeric(x) && !all(is.na(x))) {
      round(x, digits)
    } else {
      x
    }
  }

  # --- SWAPPED ASSIGNMENTS per user request ---
  # Statistics for entities row: now based on entities per period (colSums)
  entities_per_period <- colSums(presence_matrix)
  entity_stats <- entities_per_period[entities_per_period > 0]

  if (length(entity_stats) > 0) {
    mean_entities <- round_if_needed(mean(entity_stats, na.rm = TRUE), digits)
    std_entities <- round_if_needed(
      stats::sd(entity_stats, na.rm = TRUE),
      digits
    )
    min_entities <- min(entity_stats)
    max_entities <- max(entity_stats)

    if (detailed) {
      p5_entities <- round_if_needed(
        stats::quantile(
          entity_stats,
          probs = 0.05,
          na.rm = TRUE,
          names = FALSE
        ),
        digits
      )
      p25_entities <- round_if_needed(
        stats::quantile(
          entity_stats,
          probs = 0.25,
          na.rm = TRUE,
          names = FALSE
        ),
        digits
      )
      p50_entities <- round_if_needed(
        stats::quantile(
          entity_stats,
          probs = 0.50,
          na.rm = TRUE,
          names = FALSE
        ),
        digits
      )
      p75_entities <- round_if_needed(
        stats::quantile(
          entity_stats,
          probs = 0.75,
          na.rm = TRUE,
          names = FALSE
        ),
        digits
      )
      p95_entities <- round_if_needed(
        stats::quantile(
          entity_stats,
          probs = 0.95,
          na.rm = TRUE,
          names = FALSE
        ),
        digits
      )
    }
  } else {
    mean_entities <- NA
    std_entities <- NA
    min_entities <- NA
    max_entities <- NA

    if (detailed) {
      p5_entities <- NA
      p25_entities <- NA
      p50_entities <- NA
      p75_entities <- NA
      p95_entities <- NA
    }
  }

  # Statistics for periods row: now based on periods per entity (rowSums)
  periods_per_entity <- rowSums(presence_matrix)
  period_stats <- periods_per_entity[periods_per_entity > 0]

  if (length(period_stats) > 0) {
    mean_periods <- round_if_needed(mean(period_stats, na.rm = TRUE), digits)
    std_periods <- round_if_needed(
      stats::sd(period_stats, na.rm = TRUE),
      digits
    )
    min_periods <- min(period_stats)
    max_periods <- max(period_stats)

    if (detailed) {
      p5_periods <- round_if_needed(
        stats::quantile(
          period_stats,
          probs = 0.05,
          na.rm = TRUE,
          names = FALSE
        ),
        digits
      )
      p25_periods <- round_if_needed(
        stats::quantile(
          period_stats,
          probs = 0.25,
          na.rm = TRUE,
          names = FALSE
        ),
        digits
      )
      p50_periods <- round_if_needed(
        stats::quantile(
          period_stats,
          probs = 0.50,
          na.rm = TRUE,
          names = FALSE
        ),
        digits
      )
      p75_periods <- round_if_needed(
        stats::quantile(
          period_stats,
          probs = 0.75,
          na.rm = TRUE,
          names = FALSE
        ),
        digits
      )
      p95_periods <- round_if_needed(
        stats::quantile(
          period_stats,
          probs = 0.95,
          na.rm = TRUE,
          names = FALSE
        ),
        digits
      )
    }
  } else {
    mean_periods <- NA
    std_periods <- NA
    min_periods <- NA
    max_periods <- NA

    if (detailed) {
      p5_periods <- NA
      p25_periods <- NA
      p50_periods <- NA
      p75_periods <- NA
      p95_periods <- NA
    }
  }

  # Create the result data.frame based on detailed parameter
  if (detailed) {
    result_df <- data.frame(
      dimension = c("entities", "periods"),
      mean = c(mean_entities, mean_periods),
      std = c(std_entities, std_periods),
      min = c(min_entities, min_periods),
      p5 = c(p5_entities, p5_periods),
      p25 = c(p25_entities, p25_periods),
      p50 = c(p50_entities, p50_periods),
      p75 = c(p75_entities, p75_periods),
      p95 = c(p95_entities, p95_periods),
      max = c(max_entities, max_periods),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    result_df <- data.frame(
      dimension = c("entities", "periods"),
      mean = c(mean_entities, mean_periods),
      std = c(std_entities, std_periods),
      min = c(min_entities, min_periods),
      max = c(max_entities, max_periods),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  # Add standardized attributes
  attr(result_df, "panel_group") <- group
  attr(result_df, "panel_time") <- time
  attr(result_df, "panel_presence") <- presence
  attr(result_df, "panel_detailed") <- detailed
  attr(result_df, "panel_digits") <- digits
  attr(result_df, "panel_n_entities") <- total_entities
  attr(result_df, "panel_n_periods") <- total_periods
  attr(result_df, "panel_total_rows") <- total_rows
  attr(result_df, "panel_entities") <- all_groups
  attr(result_df, "panel_periods") <- all_times
  attr(result_df, "panel_matrix") <- presence_matrix

  return(result_df)
}
