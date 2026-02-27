#' Time Periods Completeness Description
#'
#' This function calculates, for each time period, the number of entities that have at least one
#' non‑missing value in any substantive variable, and the corresponding share of all entities.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the entity and time variables.
#'        Not required if data has panel attributes.
#' @param delta An optional positive integer giving the expected interval between time periods.
#' @param digits An integer specifying the number of decimal places for rounding the share column.
#'        Default = 3.
#'
#' @return A data.frame with entities presence summary by time period, class `"panel_description"`.
#'
#' @details
#' The returned data.frame contains the following columns:
#' \describe{
#'   \item{\code{[time]}}{Time period identifier (name matches the input `time` variable)}
#'   \item{\code{count}}{Number of distinct entities observed in that period,
#'         i.e., entities with at least one row containing a non‑NA value in substantive variables}
#'   \item{\code{share}}{Proportion of entities observed in that period (0 to 1), rounded to `digits`.}
#' }
#'
#' Before analysis, rows with missing values (`NA`) in the entity or time variables are removed.
#' Messages indicate how many rows were excluded. The excluded rows are stored in `details$excluded_rows`.
#'
#' If `delta` is supplied, the time variable is coerced to numeric (if possible). The function checks
#' that all observed time points are compatible with a regular spacing of that interval. If gaps are detected,
#' a message lists the missing periods (unless the interval was inherited from panel attributes), and rows
#' for those periods are added to the output with `count = 0` and `share = 0`.
#'
#' Duplicate entity‑time combinations are checked; if found they are stored in `details$entity_time_duplicates`
#' and a message is printed (unless identifiers came from panel attributes).
#'
#' @seealso \code{\link{plot_periods}}, \code{\link{describe_balance}}, \code{\link{describe_patterns}}
#'
#' @examples
#' data(production)
#' describe_periods(production, index = c("firm", "year"))
#' describe_periods(production, index = c("firm", "year"), delta = 1, digits = 4)
#'
#' @export
describe_periods <- function(
  data,
  index = NULL,
  delta = NULL,
  digits = 3
) {
  sort_unique_preserve <- function(x) {
    ux <- unique(x)
    if (is.numeric(ux)) {
      sort(ux)
    } else if (inherits(ux, "Date") || inherits(ux, "POSIXt")) {
      sort(ux)
    } else if (is.factor(ux)) {
      sorted_char <- sort(as.character(ux))
      factor(sorted_char, levels = sorted_char, ordered = is.ordered(ux))
    } else {
      sort(ux)
    }
  }

  round_if_needed <- function(x, d) {
    if (is.numeric(x) && !all(is.na(x))) round(x, d) else x
  }

  # --- Initialisation ---
  user_index <- index
  user_delta <- delta
  entity_time_from_metadata <- FALSE
  delta_from_metadata <- FALSE

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
    if (is.null(delta) && !is.null(metadata$delta)) {
      delta <- metadata$delta
      delta_from_metadata <- TRUE
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

  if (
    !is.numeric(digits) ||
      length(digits) != 1 ||
      digits < 0 ||
      digits != round(digits)
  ) {
    stop("'digits' must be a non-negative integer")
  }
  digits <- as.integer(digits)

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

  # --- Duplicate check ---
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

  # --- Delta handling ---
  if (!is.null(delta)) {
    if (
      !is.numeric(delta) ||
        length(delta) != 1 ||
        delta <= 0 ||
        delta != round(delta)
    ) {
      stop("'delta' must be a positive integer")
    }
    time_vals_orig <- data[[time_var]]
    if (!is.numeric(time_vals_orig)) {
      time_numeric <- suppressWarnings(as.numeric(as.character(time_vals_orig)))
      if (anyNA(time_numeric)) {
        stop("Cannot convert time variable '", time_var, "' to numeric.")
      }
      data[[time_var]] <- time_numeric
    }
    obs_periods <- sort(unique(data[[time_var]]))
    time_diffs <- diff(obs_periods)
    if (!all(time_diffs %% delta == 0)) {
      stop(
        "Observed time points are not evenly spaced by multiples of delta (",
        delta,
        ")."
      )
    }
    full_seq <- seq(from = min(obs_periods), to = max(obs_periods), by = delta)
    missing <- setdiff(full_seq, obs_periods)
    if (length(missing) > 0 && !delta_from_metadata) {
      message(
        "Irregular time intervals detected. Missing periods: ",
        paste(missing, collapse = ", ")
      )
    }
  }

  # Original vectors
  entity_orig <- data[[entity_var]]
  time_orig <- data[[time_var]]

  if (!is.null(delta)) {
    unique_times <- sort(unique(data[[time_var]]))
    if (exists("missing") && length(missing) > 0) {
      unique_times <- sort(c(unique_times, missing))
    }
  } else {
    unique_times <- sort_unique_preserve(time_orig)
  }
  unique_times_char <- as.character(unique_times)

  total_entities <- length(unique(entity_orig))
  substantive_vars <- setdiff(names(data), c(entity_var, time_var))
  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides entity and time variables)")
  }

  period_counts <- integer(length(unique_times))
  entities <- vector("list", length(unique_times))
  names(entities) <- unique_times_char

  for (i in seq_along(unique_times)) {
    current_time <- unique_times[i]
    current_time_char <- unique_times_char[i]

    if (!is.null(delta) && exists("missing") && current_time %in% missing) {
      period_counts[i] <- 0
      entities[[i]] <- vector(class(entity_orig), 0)
    } else {
      time_indices <- which(time_orig == current_time)
      if (length(time_indices) > 0) {
        period_data <- data[time_indices, substantive_vars, drop = FALSE]
        period_entities <- entity_orig[time_indices]
        has_some_data <- apply(period_data, 1, function(x) any(!is.na(x)))
        if (any(has_some_data)) {
          period_counts[i] <- length(unique(period_entities[has_some_data]))
          entities[[i]] <- unique(period_entities[has_some_data])
        } else {
          period_counts[i] <- 0
          entities[[i]] <- vector(class(entity_orig), 0)
        }
      } else {
        period_counts[i] <- 0
        entities[[i]] <- vector(class(entity_orig), 0)
      }
    }
  }

  share <- round_if_needed(period_counts / total_entities, digits)

  result_df <- data.frame(
    time_period = unique_times,
    count = period_counts,
    share = share,
    stringsAsFactors = FALSE
  )
  names(result_df)[1] <- time_var

  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    entity = entity_var,
    time = time_var,
    delta = delta,
    digits = digits
  )

  details <- list(entities = entities)
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
