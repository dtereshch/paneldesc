#' Panel Data Balance Description
#'
#' This function provides summary statistics for panel data structure with focus on balance
#' and data completeness.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the entity and time variables.
#'        Not required if data has panel attributes.
#' @param detail A logical flag indicating whether to return additional statistics
#'        (5th, 25th, 50th, 75th, and 95th percentiles). Default = FALSE.
#' @param digits An integer specifying the number of decimal places for rounding mean values.
#'        Default = 3.
#'
#' @return A data.frame with panel data summary statistics for entities and periods.
#'
#' @details
#' The statistics for entities describe the distribution of the number of entities
#' observed per time period (cross‑sectional size per period). The statistics for
#' periods describe the distribution of the number of time periods observed per
#' entity (temporal length per entity).
#'
#' The returned data.frame always contains the following columns:
#' \describe{
#'   \item{\code{dimension}}{Either "entities" or "periods".}
#'   \item{\code{mean}}{Mean number of entities per period (or periods per entity).}
#'   \item{\code{std}}{Standard deviation.}
#'   \item{\code{min}}{Minimum value.}
#'   \item{\code{max}}{Maximum value.}
#' }
#'
#' When \code{detail = TRUE}, five additional percentile columns are included:
#' \describe{
#'   \item{\code{p5}}{5th percentile.}
#'   \item{\code{p25}}{25th percentile (first quartile).}
#'   \item{\code{p50}}{50th percentile (median).}
#'   \item{\code{p75}}{75th percentile (third quartile).}
#'   \item{\code{p95}}{95th percentile.}
#' }
#' All statistics are rounded to the number of decimal places specified by \code{digits}.
#'
#' The object has class \code{"panel_description"} and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing the full presence matrix.}
#' }
#'
#' @note
#' An entity/time combination is considered **present** if the corresponding row contains at least
#' one non‑NA value in any substantive variable (all columns except the entity and time identifiers).
#'
#' Before analysis, rows with missing values (`NA`) in the entity or time variables are removed.
#' Messages indicate how many rows were excluded.
#'
#' The function also checks for duplicate entity‑time combinations. In a properly structured panel dataset,
#' each entity should have at most one observation per time period. A message is printed only when the identifiers
#' were explicitly provided (i.e., not taken from panel attributes).
#'
#' @seealso
#' See also [describe_dimensions()], [describe_periods()], [describe_patterns()], [plot_periods()].
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' describe_balance(production, index = c("firm", "year"))
#'
#' # With panel_data object
#' panel <- make_panel(production, index = c("firm", "year"))
#' describe_balance(panel)
#'
#' # Returning detailed statisitcs
#' describe_balance(production, index = c("firm", "year"), detail = TRUE)
#'
#' # Custom rounding
#' describe_balance(production, index = c("firm", "year"), digits = 2)
#'
#' # Accessing attributes
#' out_des_bal <- describe_balance(production, index = c("firm", "year"))
#' attr(out_des_bal, "metadata")
#' attr(out_des_bal, "details")
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
  msg_printed <- FALSE

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
    stop("entity and time variables cannot be the same")
  }

  # --- Remove rows with NA in entity or time ---
  na_entity <- is.na(data[[entity_var]])
  na_time <- is.na(data[[time_var]])

  if (any(na_entity)) {
    message(
      sum(na_entity),
      " rows with missing values in '",
      entity_var,
      "' variable found and excluded."
    )
    msg_printed <- TRUE
  }
  if (any(na_time)) {
    message(
      sum(na_time),
      " rows with missing values in '",
      time_var,
      "' variable found and excluded."
    )
    msg_printed <- TRUE
  }

  if (any(na_entity | na_time)) {
    data <- data[!(na_entity | na_time), , drop = FALSE]
    rownames(data) <- NULL
  }

  # --- Check for duplicate entity-time combinations ---
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
      msg_printed <- TRUE
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
  presence_mat <- matrix(
    0,
    nrow = total_entities,
    ncol = total_periods,
    dimnames = list(all_entities, all_times)
  )
  entity_char <- as.character(data[[entity_var]])
  time_char <- as.character(data[[time_var]])

  has_data <- apply(data[substantive_vars], 1, function(x) any(!is.na(x)))
  for (i in seq_along(entity_char)) {
    if (has_data[i]) {
      presence_mat[entity_char[i], time_char[i]] <- 1
    }
  }

  # Entities per period
  entities_per_period <- colSums(presence_mat)
  entity_stats <- entities_per_period[entities_per_period > 0]
  if (length(entity_stats) > 0) {
    mean_entities <- round_if_needed(mean(entity_stats, na.rm = TRUE), digits)
    std_entities <- round_if_needed(sd(entity_stats, na.rm = TRUE), digits)
    min_entities <- min(entity_stats)
    max_entities <- max(entity_stats)
    if (detail) {
      p5_entities <- round_if_needed(
        quantile(entity_stats, 0.05, names = FALSE),
        digits
      )
      p25_entities <- round_if_needed(
        quantile(entity_stats, 0.25, names = FALSE),
        digits
      )
      p50_entities <- round_if_needed(
        quantile(entity_stats, 0.50, names = FALSE),
        digits
      )
      p75_entities <- round_if_needed(
        quantile(entity_stats, 0.75, names = FALSE),
        digits
      )
      p95_entities <- round_if_needed(
        quantile(entity_stats, 0.95, names = FALSE),
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
  periods_per_entity <- rowSums(presence_mat)
  period_stats <- periods_per_entity[periods_per_entity > 0]
  if (length(period_stats) > 0) {
    mean_periods <- round_if_needed(mean(period_stats, na.rm = TRUE), digits)
    std_periods <- round_if_needed(sd(period_stats, na.rm = TRUE), digits)
    min_periods <- min(period_stats)
    max_periods <- max(period_stats)
    if (detail) {
      p5_periods <- round_if_needed(
        quantile(period_stats, 0.05, names = FALSE),
        digits
      )
      p25_periods <- round_if_needed(
        quantile(period_stats, 0.25, names = FALSE),
        digits
      )
      p50_periods <- round_if_needed(
        quantile(period_stats, 0.50, names = FALSE),
        digits
      )
      p75_periods <- round_if_needed(
        quantile(period_stats, 0.75, names = FALSE),
        digits
      )
      p95_periods <- round_if_needed(
        quantile(period_stats, 0.95, names = FALSE),
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
    out <- data.frame(
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
    out <- data.frame(
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
  details <- list(presence_matrix = presence_mat)

  attr(out, "metadata") <- metadata
  attr(out, "details") <- details
  class(out) <- c("panel_description", "data.frame")

  if (msg_printed) {
    cat("\n")
  }

  return(out)
}
