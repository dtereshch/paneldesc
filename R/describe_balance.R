#' Panel Data Balance Description
#'
#' This function provides summary statistics for panel data structure with focus on balance
#' and data completeness.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param group A character string specifying the name of the entity/group variable.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
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
#' An entity/time combination is considered **present** if the corresponding row contains at least
#' one non-NA value in any substantive variable (i.e., all columns except the group and time identifiers).
#'
#' Before analysis, rows with missing values (`NA`) in the `group` or `time` variables are removed.
#' Messages indicate how many rows were excluded due to each variable. The excluded rows are stored in
#' `details$excluded_rows` for further inspection.
#'
#' The function also checks for duplicate group-time combinations. In a properly structured panel dataset,
#' each entity (group) should have at most one observation per time period. If duplicates are found,
#' they are stored in `details$entity_time_duplicates`. A message is printed only when the identifiers
#' were explicitly provided (i.e., not taken from `panel_data` attributes).
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
#' The returned data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing additional information: `presence_matrix`,
#'         `excluded_rows` (if any), and, if duplicates were found, `entity_time_duplicates`.}
#' }
#'
#' @seealso
#' [describe_periods()], [describe_patterns()], [plot_periods()], [plot_patterns()]
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
#' # With custom rounding
#' describe_balance(production, group = "firm", time = "year", digits = 4)
#'
#' @export
describe_balance <- function(
  data,
  group = NULL,
  time = NULL,
  detailed = FALSE,
  digits = 3
) {
  # --- Consistent initialisation ---
  user_group <- group
  user_time <- time
  group_time_from_metadata <- FALSE

  if (inherits(data, "panel_data")) {
    metadata <- attr(data, "metadata")
    if (
      is.null(metadata) || is.null(metadata$group) || is.null(metadata$time)
    ) {
      stop(
        "Object has class 'panel_data' but missing or incomplete 'metadata' attribute."
      )
    }
    if (is.null(group)) {
      group <- metadata$group
    }
    if (is.null(time)) {
      time <- metadata$time
    } # time is required

    group_from_metadata <- is.null(user_group) && !is.null(metadata$group)
    time_from_metadata <- is.null(user_time) && !is.null(metadata$time)
    group_time_from_metadata <- group_from_metadata && time_from_metadata
  } else {
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame")
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

  if (time == group) {
    stop("'time' and 'group' cannot be the same variable")
  }

  # --- Remove rows with NA in group or time ---
  excluded_rows <- NULL
  na_group <- is.na(data[[group]])
  na_time <- is.na(data[[time]])

  if (any(na_group)) {
    message(
      "Missing values in ",
      group,
      " variable found. Excluding ",
      sum(na_group),
      " rows."
    )
  }
  if (any(na_time)) {
    message(
      "Missing values in ",
      time,
      " variable found. Excluding ",
      sum(na_time),
      " rows."
    )
  }

  if (any(na_group | na_time)) {
    excluded_rows <- data[na_group | na_time, , drop = FALSE]
    data <- data[!(na_group | na_time), , drop = FALSE]
    rownames(data) <- NULL
  }
  # ----------------------------------------------------------------

  # --- Check for duplicate group-time combinations ---
  dup_combinations <- NULL
  dup_rows <- duplicated(data[c(group, time)]) |
    duplicated(data[c(group, time)], fromLast = TRUE)
  if (any(dup_rows)) {
    dup_combinations <- unique(data[dup_rows, c(group, time), drop = FALSE])
    n_dup <- nrow(dup_combinations)
    if (!group_time_from_metadata) {
      examples <- utils::head(dup_combinations, 5)
      example_strings <- paste0(examples[[group]], "-", examples[[time]])
      example_str <- paste(example_strings, collapse = ", ")
      message(
        n_dup,
        " duplicate group-time combinations found. Examples: ",
        example_str
      )
    }
  }
  # ----------------------------------------------------

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

  # Fill presence matrix based on "observed" definition:
  # entity/time is present if it has at least one non-NA substantive variable.
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

  # Helper function for rounding
  round_if_needed <- function(x, digits) {
    if (is.numeric(x) && !all(is.na(x))) {
      round(x, digits)
    } else {
      x
    }
  }

  # Statistics for entities row: based on entities per period (colSums)
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

  # Statistics for periods row: based on periods per entity (rowSums)
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

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time,
    detailed = detailed,
    digits = digits
  )

  # Build details list
  details <- list(
    presence_matrix = presence_matrix
  )

  if (!is.null(excluded_rows)) {
    details$excluded_rows <- excluded_rows
  }

  if (!is.null(dup_combinations)) {
    details$entity_time_duplicates <- dup_combinations
  }

  # Set attributes
  attr(result_df, "metadata") <- metadata
  attr(result_df, "details") <- details
  class(result_df) <- c("panel_description", "data.frame")

  return(result_df)
}
