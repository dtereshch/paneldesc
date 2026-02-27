#' Panel Data Balance Description
#'
#' This function provides summary statistics for panel data structure with focus on balance
#' and data completeness.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the entity and time variables.
#'        Not required if data has panel attributes.
#' @param detail A logical flag indicating whether to return additional statistics (p5, p25, p50, p75, p95).
#'        Default = FALSE.
#' @param digits An integer specifying the number of decimal places for rounding mean values.
#'        Default = 3.
#'
#' @return A data.frame with panel data summary statistics for entities and periods.
#'         Class `"panel_description"`.
#'
#' @details
#' The statistics for entities describe the distribution of entities observed per time period
#' (cross-sectional size per period), while statistics for periods describe the distribution
#' of time periods observed per entity (temporal length per entity).
#'
#' An entity/time combination is considered **present** if the corresponding row contains at least
#' one non-NA value in any substantive variable (all columns except the entity and time identifiers).
#'
#' Before analysis, rows with missing values (`NA`) in the entity or time variables are removed.
#' Messages indicate how many rows were excluded due to each variable. The excluded rows are stored in
#' `details$excluded_rows` for further inspection.
#'
#' The function also checks for duplicate entity-time combinations. In a properly structured panel dataset,
#' each entity should have at most one observation per time period. If duplicates are found,
#' they are stored in `details$entity_time_duplicates`. A message is printed only when the identifiers
#' were explicitly provided (i.e., not taken from panel attributes).
#'
#' The returned data.frame has columns `dimension`, `mean`, `std`, `min`, `max`, and if `detail = TRUE`,
#' also `p5`, `p25`, `p50`, `p75`, `p95`.
#'
#' @seealso \code{\link{describe_periods}}, \code{\link{describe_patterns}}, \code{\link{plot_periods}}, \code{\link{plot_patterns}}
#'
#' @examples
#' data(production)
#' describe_balance(production, index = c("firm", "year"))
#'
#' # With panel attributes
#' panel_data <- make_panel(production, index = c("firm", "year"))
#' describe_balance(panel_data)
#'
#' @export
describe_balance <- function(
  data,
  index = NULL,
  detail = FALSE,
  digits = 3
) {
  # --- Initialisation ---
  user_index <- index
  entity_time_from_metadata <- FALSE

  if (inherits(data, "panel_data")) {
    metadata <- attr(data, "metadata")
    if (
      is.null(metadata) || is.null(metadata$entity) || is.null(metadata$time)
    ) {
      stop(
        "Object has class 'panel_data' but missing or incomplete 'metadata' attribute."
      )
    }
    if (is.null(index)) {
      entity_var <- metadata$entity
      time_var <- metadata$time
    } else {
      if (length(index) != 2 || !is.character(index)) {
        stop("'index' must be a character vector of length 2")
      }
      entity_var <- index[1]
      time_var <- index[2]
    }
    entity_from_metadata <- is.null(user_index) && !is.null(metadata$entity)
    time_from_metadata <- is.null(user_index) && !is.null(metadata$time)
    entity_time_from_metadata <- entity_from_metadata && time_from_metadata
  } else {
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame")
    }
    if (is.null(index)) {
      stop("For regular data.frames, 'index' must be provided")
    }
    if (length(index) != 2 || !is.character(index)) {
      stop("'index' must be a character vector of length 2")
    }
    entity_var <- index[1]
    time_var <- index[2]
  }

  # Common validation
  if (!entity_var %in% names(data)) {
    stop('variable "', entity_var, '" not found in data')
  }
  if (!time_var %in% names(data)) {
    stop('variable "', time_var, '" not found in data')
  }
  if (time_var == entity_var) {
    stop("time and entity variables cannot be the same")
  }

  # --- Remove rows with NA in entity or time ---
  excluded_rows <- NULL
  na_entity <- is.na(data[[entity_var]])
  na_time <- is.na(data[[time_var]])

  if (any(na_entity)) {
    message(
      "Missing values in entity variable '",
      entity_var,
      "' found. Excluding ",
      sum(na_entity),
      " rows."
    )
  }
  if (any(na_time)) {
    message(
      "Missing values in time variable '",
      time_var,
      "' found. Excluding ",
      sum(na_time),
      " rows."
    )
  }

  if (any(na_entity | na_time)) {
    excluded_rows <- data[na_entity | na_time, , drop = FALSE]
    data <- data[!(na_entity | na_time), , drop = FALSE]
    rownames(data) <- NULL
  }

  # --- Check for duplicate entity-time combinations ---
  dup_combinations <- NULL
  dup_rows <- duplicated(data[c(entity_var, time_var)]) |
    duplicated(data[c(entity_var, time_var)], fromLast = TRUE)
  if (any(dup_rows)) {
    dup_combinations <- unique(data[
      dup_rows,
      c(entity_var, time_var),
      drop = FALSE
    ])
    n_dup <- nrow(dup_combinations)
    if (!entity_time_from_metadata) {
      examples <- utils::head(dup_combinations, 5)
      example_strings <- paste0(
        examples[[entity_var]],
        "-",
        examples[[time_var]]
      )
      example_str <- paste(example_strings, collapse = ", ")
      message(
        n_dup,
        " duplicate entity-time combinations found. Examples: ",
        example_str
      )
    }
  }

  # Validate detail and digits
  if (!is.logical(detail) || length(detail) != 1) {
    stop("'detail' must be a single logical value")
  }
  if (
    !is.numeric(digits) ||
      length(digits) != 1 ||
      digits < 0 ||
      digits != round(digits)
  ) {
    stop("'digits' must be a non-negative integer")
  }
  digits <- as.integer(digits)

  # Get substantive variables (all except entity and time)
  substantive_vars <- setdiff(names(data), c(entity_var, time_var))
  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides entity and time variables)")
  }

  # Get all unique entities and periods
  all_entities <- unique(as.character(data[[entity_var]]))
  all_times <- unique(as.character(data[[time_var]]))
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", all_times))) {
    all_times <- as.character(sort(as.numeric(all_times)))
  } else {
    all_times <- sort(all_times)
  }

  total_entities <- length(all_entities)
  total_periods <- length(all_times)

  # Presence matrix
  presence_matrix <- matrix(
    0,
    nrow = total_entities,
    ncol = total_periods,
    dimnames = list(all_entities, all_times)
  )
  entity_vec <- as.character(data[[entity_var]])
  time_vec <- as.character(data[[time_var]])

  has_data <- apply(data[substantive_vars], 1, function(x) any(!is.na(x)))
  for (i in seq_along(entity_vec)) {
    if (has_data[i]) {
      presence_matrix[entity_vec[i], time_vec[i]] <- 1
    }
  }

  round_if_needed <- function(x, d) {
    if (is.numeric(x) && !all(is.na(x))) round(x, d) else x
  }

  # Entities per period
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
    if (detail) {
      p5_entities <- round_if_needed(
        stats::quantile(entity_stats, 0.05, names = FALSE),
        digits
      )
      p25_entities <- round_if_needed(
        stats::quantile(entity_stats, 0.25, names = FALSE),
        digits
      )
      p50_entities <- round_if_needed(
        stats::quantile(entity_stats, 0.50, names = FALSE),
        digits
      )
      p75_entities <- round_if_needed(
        stats::quantile(entity_stats, 0.75, names = FALSE),
        digits
      )
      p95_entities <- round_if_needed(
        stats::quantile(entity_stats, 0.95, names = FALSE),
        digits
      )
    }
  } else {
    mean_entities <- std_entities <- min_entities <- max_entities <- NA
    if (detail) {
      p5_entities <- p25_entities <- p50_entities <- p75_entities <- p95_entities <- NA
    }
  }

  # Periods per entity
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
    if (detail) {
      p5_periods <- round_if_needed(
        stats::quantile(period_stats, 0.05, names = FALSE),
        digits
      )
      p25_periods <- round_if_needed(
        stats::quantile(period_stats, 0.25, names = FALSE),
        digits
      )
      p50_periods <- round_if_needed(
        stats::quantile(period_stats, 0.50, names = FALSE),
        digits
      )
      p75_periods <- round_if_needed(
        stats::quantile(period_stats, 0.75, names = FALSE),
        digits
      )
      p95_periods <- round_if_needed(
        stats::quantile(period_stats, 0.95, names = FALSE),
        digits
      )
    }
  } else {
    mean_periods <- std_periods <- min_periods <- max_periods <- NA
    if (detail) {
      p5_periods <- p25_periods <- p50_periods <- p75_periods <- p95_periods <- NA
    }
  }

  # Build result data.frame
  if (detail) {
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

  # Metadata and details
  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    entity = entity_var,
    time = time_var,
    detail = detail,
    digits = digits
  )
  details <- list(presence_matrix = presence_matrix)
  if (!is.null(excluded_rows)) {
    details$excluded_rows <- excluded_rows
  }
  if (!is.null(dup_combinations)) {
    details$entity_time_duplicates <- dup_combinations
  }

  attr(result_df, "metadata") <- metadata
  attr(result_df, "details") <- details
  class(result_df) <- c("panel_description", "data.frame")

  return(result_df)
}
