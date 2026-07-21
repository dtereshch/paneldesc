#' Panel Data Structure Setting
#'
#' This function adds panel structure attributes to a data.frame, storing entity and time variable names,
#' and optionally checks the expected interval between time periods. The returned data is sorted by
#' the entity and time variables.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the entity and time variables.
#' @param delta An optional integer giving the expected interval between time periods.
#' @param ... Additional arguments (not used, except to catch deprecated `balance`).
#'
#' @return The input data.frame sorted by the entity and time variables, with additional attributes.
#'
#' @details
#' This function adds attributes to a data.frame to mark it as panel data.
#'
#' Before returning, the data is sorted by the entity variable (first element of `index`) and then by
#' the time variable (second element).
#'
#' If `delta` is supplied, the function checks that all observed time points are separated by multiples of `delta`.
#' If gaps are detected, a message lists the missing periods and the full sequence is stored in `details$periods_restored`.
#'
#' The returned object has class `"panel_data"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing the unique entities and periods, and if `delta` is supplied, the restored full period sequence and missing periods.}
#' }
#'
#' @seealso
#' See also [make_balanced()], [make_balanced()], [make_wide()], [make_long()], [make_demeaned()],
#' [describe_dimensions()].
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' panel <- make_panel(production, index = c("firm", "year"))
#'
#' # Specifying time interval
#' panel <- make_panel(production, index = c("firm", "year"), delta = 1)
#'
#' # Accessing attributes
#' attr(panel, "metadata")
#' attr(panel, "details")
#'
#' @importFrom utils head
#' @export
make_panel <- function(data, index, delta = NULL, ...) {
  # Catch deprecated 'balance' argument
  dots <- list(...)
  if ("balance" %in% names(dots)) {
    stop(
      "The 'balance' parameter is deprecated. Please use the new 'make_balanced()' function instead.",
      call. = FALSE
    )
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }
  if (length(index) != 2 || !is.character(index)) {
    stop("'index' must be a character vector of length 2")
  }
  entity_var <- index[1]
  time_var <- index[2]

  if (!entity_var %in% names(data)) {
    stop('entity variable "', entity_var, '" not found in data')
  }
  if (!time_var %in% names(data)) {
    stop('time variable "', time_var, '" not found in data')
  }
  if (time_var == entity_var) {
    stop("entity and time variables cannot be the same")
  }

  msg_printed <- FALSE

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
    examples <- utils::head(dup_combinations, 5)
    example_strings <- paste0(examples[[entity_var]], "-", examples[[time_var]])
    example_str <- paste(example_strings, collapse = ", ")

    message(
      n_dup,
      " duplicate entity-time combinations found. Examples: ",
      example_str
    )
    msg_printed <- TRUE
  }

  # --- Delta validation and coercion ---
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
  }

  # --- Sort data by entity and time ---
  data <- data[order(data[[entity_var]], data[[time_var]]), , drop = FALSE]
  rownames(data) <- NULL

  # --- Build metadata and details ---
  entities <- sort_unique_preserve(data[[entity_var]])
  periods <- sort_unique_preserve(data[[time_var]])

  details <- list(entities = entities, periods = periods)

  if (!is.null(delta)) {
    time_vals <- data[[time_var]]
    if (!is.numeric(time_vals)) {
      time_vals <- suppressWarnings(as.numeric(as.character(time_vals)))
    }
    obs_periods <- sort(unique(time_vals))
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
    if (length(missing) > 0) {
      details$periods_restored <- full_seq
      details$periods_missing <- missing
      message(
        "Irregular time intervals detected. Missing periods: ",
        paste(missing, collapse = ", ")
      )
      msg_printed <- TRUE
    }
  }

  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    entity = entity_var,
    time = time_var,
    delta = delta
  )

  out <- data
  attr(out, "metadata") <- metadata
  attr(out, "details") <- details
  class(out) <- c("panel_data", class(out))

  if (msg_printed) {
    cat("\n")
  }

  return(out)
}
