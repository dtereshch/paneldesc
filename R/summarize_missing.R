#' Missing Values Summary for Panel Data
#'
#' This function calculates summary statistics for missing values (NAs) in panel data,
#' providing both overall and detailed period-specific missing value counts.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param select A character vector specifying which variables to analyze for missing values.
#'        If not specified, all variables (except entity and time) will be used.
#' @param index A character vector of length 2 specifying the names of the entity and time variables.
#'        Not required if data has panel attributes.
#' @param detail A logical flag indicating whether to return detailed period-specific NA counts.
#'        Default = FALSE.
#' @param digits An integer indicating the number of decimal places to round the share column.
#'        Default = 3.
#'
#' @return A data.frame with missing value summary statistics.
#'
#' @details
#' When `detail = FALSE`, returns columns:
#' \describe{
#'   \item{\code{variable}}{Variable name.}
#'   \item{\code{na_count}}{Total number of missing values in that variable.}
#'   \item{\code{na_share}}{Proportion of missing values (rounded to `digits`).}
#'   \item{\code{entities}}{Number of distinct entities that have at least one missing value in that variable.}
#'   \item{\code{periods}}{Number of distinct time periods that have at least one missing value in that variable.}
#' }
#'
#' When `detail = TRUE`, additional columns for each time period contain the number of missing values
#' in that variable for that period.
#'
#' The object has class `"panel_summary"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List with counts of variables with/without NAs, and their names.}
#' }
#'
#' @note
#' Before analysis, rows with missing values (`NA`) in the entity or time variables are removed.
#' Messages indicate how many rows were excluded.
#'
#' Duplicate entity‑time combinations are checked; if found, a message is printed
#' (unless identifiers came from panel attributes).
#'
#' The interpretation of missing counts may differ depending on whether the panel is balanced or unbalanced.
#' In a balanced panel, each time period contains the same number of entities, so the raw NA counts per period
#' (when `detail = TRUE`) are directly comparable across periods. In an unbalanced panel, the number of entities
#' varies by period, so the raw NA counts should be interpreted relative to the number of observations available
#' in each period. The function does not standardize the counts by period size; users should account for the
#' panel structure when interpreting the results.
#'
#' @seealso
#' [describe_incomplete()] for entity‑wise missing description.
#' [summarize_transition()] for transition summaries.
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' summarize_missing(production, index = c("firm", "year"))
#'
#' # With panel_data object
#' panel <- make_panel(production, index = c("firm", "year"))
#' summarize_missing(panel)
#'
#' # Returning detailed results
#' summarize_missing(production, index = c("firm", "year"), detail = TRUE)
#'
#' # Custom rounding
#' summarize_missing(production, index = c("firm", "year"), digits = 2)
#'
#' # Accessing attributes
#' out_sum_mis <- summarize_missing(production, index = c("firm", "year"))
#' attr(out_sum_mis, "metadata")
#' attr(out_sum_mis, "details")
#'
#' @export
summarize_missing <- function(
  data,
  select = NULL,
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

  # Validation
  if (!is.null(select) && !is.character(select)) {
    stop("'select' must be a character vector or NULL")
  }
  if (!entity_var %in% names(data)) {
    stop('entity variable "', entity_var, '" not found in data')
  }
  if (!time_var %in% names(data)) {
    stop('time variable "', time_var, '" not found in data')
  }
  if (time_var == entity_var) {
    stop("entity and time variables cannot be the same")
  }
  if (!is.null(select)) {
    if (entity_var %in% select) {
      stop("'select' cannot contain the entity variable '", entity_var, "'")
    }
    if (time_var %in% select) {
      stop("'select' cannot contain the time variable '", time_var, "'")
    }
  }
  if (!is.logical(detail) || length(detail) != 1) {
    stop("'detail' must be a single logical")
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

  # Determine variables to analyze
  if (is.null(select)) {
    analyze_vars <- setdiff(names(data), c(entity_var, time_var))
    if (length(analyze_vars) == 0) {
      stop("no variables found to analyze (besides entity and time)")
    }
    message("Analyzing all variables: ", paste(analyze_vars, collapse = ", "))
    msg_printed <- TRUE
  } else {
    missing_vars <- select[!select %in% names(data)]
    if (length(missing_vars) > 0) {
      stop("variables not found: ", paste(missing_vars, collapse = ", "))
    }
    analyze_vars <- setdiff(select, c(entity_var, time_var))
    if (length(analyze_vars) == 0) {
      stop("no variables to analyze (excluding entity and time)")
    }
  }

  # Unique time periods
  time_char <- as.character(data[[time_var]])
  unique_periods <- unique(time_char)
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", unique_periods))) {
    ordered_periods <- as.character(sort(as.numeric(unique_periods)))
  } else {
    ordered_periods <- sort(unique_periods)
  }

  unique_entities <- unique(as.character(data[[entity_var]]))
  total_obs <- nrow(data)
  total_entities <- length(unique_entities)
  total_periods <- length(ordered_periods)

  results_list <- list()

  for (var in analyze_vars) {
    na_count <- sum(is.na(data[[var]]))
    na_share <- ifelse(total_obs > 0, na_count / total_obs, 0)
    na_share <- round_if_needed(na_share, digits)

    if (na_count > 0) {
      entity_has_na <- tapply(data[[var]], data[[entity_var]], function(x) {
        any(is.na(x))
      })
      entities_with_na <- sum(entity_has_na, na.rm = TRUE)
      period_has_na <- tapply(data[[var]], data[[time_var]], function(x) {
        any(is.na(x))
      })
      periods_with_na <- sum(period_has_na, na.rm = TRUE)
    } else {
      entities_with_na <- 0
      periods_with_na <- 0
    }

    row_df <- data.frame(
      variable = var,
      na_count = na_count,
      na_share = na_share,
      entities = entities_with_na,
      periods = periods_with_na,
      stringsAsFactors = FALSE
    )

    if (detail) {
      for (per in ordered_periods) {
        per_data <- data[time_char == per, var, drop = FALSE]
        row_df[[per]] <- sum(is.na(per_data[[var]]))
      }
    }

    results_list[[var]] <- row_df
  }

  out <- do.call(rbind, results_list)
  rownames(out) <- NULL

  vars_with_na <- out$variable[out$na_count > 0]
  vars_without_na <- out$variable[out$na_count == 0]

  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    select = analyze_vars,
    entity = entity_var,
    time = time_var,
    detail = detail,
    digits = digits
  )

  details <- list(
    count_variables_with_na = length(vars_with_na),
    count_variables_without_na = length(vars_without_na),
    count_variables = length(analyze_vars),
    variables_with_na = vars_with_na,
    variables_without_na = vars_without_na
  )

  attr(out, "metadata") <- metadata
  attr(out, "details") <- details
  class(out) <- c("panel_summary", "data.frame")

  if (msg_printed) {
    cat("\n")
  }

  return(out)
}
