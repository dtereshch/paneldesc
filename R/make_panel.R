#' Panel Data Structure Setting and Balancing
#'
#' This function adds panel structure attributes to a data.frame, storing entity and time variable names,
#' and optionally checks the expected interval between time periods.
#' It can also balance the panel with a chosen method.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the entity and time variables.
#' @param delta An optional integer giving the expected interval between time periods.
#' @param balance One `"entities"`, `"periods"`, or `"rows"`.
#'        If specified, the panel is balanced according to the chosen method.
#'
#' @return The input data.frame with additional attributes, after possibly filtering or expanding rows.
#'
#' @details
#' This function adds attributes to a data.frame to mark it as panel data.
#' The returned object has class `"panel_data"` and includes the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used
#'         (`entity`, `time`, `delta`, and `balance` if provided).}
#'   \item{`details`}{List with diagnostic vectors:
#'         \describe{
#'           \item{`entities`}{Unique values of the entity variable.}
#'           \item{`periods`}{Sorted unique values of the time variable.}
#'           \item{`periods_restored`, `periods_missing`}{If `delta` is supplied and gaps are detected,
#'                 the full sequence and missing periods.}
#'         }}
#' }
#'
#' **Effect of `delta`:**
#' If `delta` is supplied, the time variable is coerced to numeric (if possible).
#' The function checks that all observed time points are separated by multiples of `delta`.
#' If gaps are detected, a message lists the missing periods and the full sequence is stored in `details$periods_restored`.
#'
#' **Balancing the panel** (presence definition as in `describe_patterns`):
#' \describe{
#'   \item{`balance = "entities"`}{Keep only entities present in **all** time periods.}
#'   \item{`balance = "periods"`}{Keep only time periods where **all** entities are present.}
#'   \item{`balance = "rows"`}{Create a row for every entity‑time combination. If `delta` is supplied,
#'         the full time sequence (including missing periods) is used. Missing combinations get `NA`
#'         in all other columns.}
#' }
#'
#' @note
#' First, rows with missing values in the entity or time variables are removed and messages are printed.
#' Then duplicate entity‑time combinations are checked; if `balance` is requested, duplicates are
#' automatically removed (first occurrence kept) and a message is issued.
#'
#' @seealso
#' See also [describe_dimensions()], [describe_balance()], [describe_periods()].
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
#' # Creating balanced panels
#' panel_bal_ent <- make_panel(production, index = c("firm", "year"), balance = "entities")
#' panel_bal_per <- make_panel(production, index = c("firm", "year"), balance = "periods")
#' panel_bal_row <- make_panel(production, index = c("firm", "year"), balance = "rows", delta = 1)
#'
#' # Accessing attributes
#' attr(panel, "metadata")
#' attr(panel, "details")
#'
#' @export
make_panel <- function(data, index, delta = NULL, balance = NULL) {
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

  valid_balance <- c("entities", "periods", "rows")
  if (!is.null(balance) && !balance %in% valid_balance) {
    stop("'balance' must be NULL, 'entities', 'periods', or 'all'")
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
    if (!is.null(balance)) {
      data <- data[!duplicated(data[c(entity_var, time_var)]), , drop = FALSE]
      message("Duplicates removed (first occurrence kept) for balancing.")
      msg_printed <- TRUE
    }
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

  # --- Balancing ---
  if (!is.null(balance)) {
    data_cols <- setdiff(names(data), c(entity_var, time_var))
    if (length(data_cols) == 0) {
      stop(
        "No data columns found (excluding entity and time). Cannot determine presence."
      )
    }

    unique_entities <- sort_unique_preserve(data[[entity_var]])
    unique_times <- sort_unique_preserve(data[[time_var]])
    entity_char <- as.character(unique_entities)
    time_char <- as.character(unique_times)

    presence_mat <- matrix(
      FALSE,
      nrow = length(entity_char),
      ncol = length(time_char),
      dimnames = list(entity_char, time_char)
    )
    for (i in seq_len(nrow(data))) {
      e <- as.character(data[[entity_var]][i])
      t <- as.character(data[[time_var]][i])
      if (any(!is.na(data[i, data_cols, drop = TRUE]))) {
        presence_mat[e, t] <- TRUE
      }
    }

    if (balance == "entities") {
      keep_entities <- entity_char[apply(presence_mat, 1, all)]
      if (length(keep_entities) == 0) {
        stop("No entity is present in all time periods.")
      }
      data <- data[
        as.character(data[[entity_var]]) %in% keep_entities,
        ,
        drop = FALSE
      ]
      rownames(data) <- NULL
    } else if (balance == "periods") {
      keep_periods <- time_char[apply(presence_mat, 2, all)]
      if (length(keep_periods) == 0) {
        stop("No time period has all entities present.")
      }
      data <- data[
        as.character(data[[time_var]]) %in% keep_periods,
        ,
        drop = FALSE
      ]
      rownames(data) <- NULL
    } else if (balance == "rows") {
      if (!is.null(delta)) {
        time_vals <- sort(unique(data[[time_var]]))
        full_time <- seq(from = min(time_vals), to = max(time_vals), by = delta)
        full_time_char <- as.character(full_time)
      } else {
        full_time_char <- time_char
        full_time <- unique_times
      }

      all_combos <- expand.grid(
        entity = entity_char,
        time = full_time_char,
        stringsAsFactors = FALSE
      )
      names(all_combos) <- c(entity_var, time_var)

      data_merge <- data
      data_merge[[entity_var]] <- as.character(data_merge[[entity_var]])
      data_merge[[time_var]] <- as.character(data_merge[[time_var]])

      merged <- merge(
        all_combos,
        data_merge,
        by = c(entity_var, time_var),
        all.x = TRUE,
        sort = FALSE
      )

      # Restore original classes
      if (is.factor(unique_entities)) {
        merged[[entity_var]] <- factor(
          merged[[entity_var]],
          levels = levels(unique_entities)
        )
      } else if (inherits(unique_entities, "Date")) {
        merged[[entity_var]] <- as.Date(merged[[entity_var]])
      } else if (is.numeric(unique_entities)) {
        merged[[entity_var]] <- as.numeric(merged[[entity_var]])
      }

      if (is.factor(unique_times)) {
        merged[[time_var]] <- factor(
          merged[[time_var]],
          levels = levels(unique_times)
        )
      } else if (inherits(unique_times, "Date")) {
        if (!is.null(delta) && inherits(unique_times, "Date")) {
          merged[[time_var]] <- as.Date(
            merged[[time_var]],
            origin = "1970-01-01"
          )
        } else {
          merged[[time_var]] <- as.Date(merged[[time_var]])
        }
      } else if (is.numeric(unique_times)) {
        merged[[time_var]] <- as.numeric(merged[[time_var]])
      }

      group_char_merged <- as.character(merged[[entity_var]])
      time_char_merged <- as.character(merged[[time_var]])
      ord <- order(
        factor(group_char_merged, levels = entity_char),
        factor(time_char_merged, levels = full_time_char)
      )
      data <- merged[ord, , drop = FALSE]
      rownames(data) <- NULL
    }
  }

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
    delta = delta,
    balance = balance
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
