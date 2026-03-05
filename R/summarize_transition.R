#' Transition Summary
#'
#' Calculates transition counts and shares between states of a categorical (factor) variable
#' across consecutive time periods within entities for panel data.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param select A character string specifying the factor variable to analyze transitions for.
#' @param index A character vector of length 2 specifying the names of the entity and time variables.
#'        Not required if data has panel attributes.
#' @param format A character string specifying the output format: `"wide"` (default) or `"long"`.
#' @param digits An integer indicating the number of decimal places to round transition shares.
#'        Default = 3.
#'
#' @return A data.frame containing transition summaries.
#'
#' @details
#' The structure depends on `format`:
#'
#' When `format = "wide"`, a transition matrix as a data.frame:
#' \describe{
#'   \item{\code{from_to}}{The originating state (row label).}
#'   \item{Columns for each destination state, containing the share of transitions from the
#'         row state to the column state (rounded to `digits`).}
#' }
#'
#' When `format = "long"`, a data.frame with columns:
#' \describe{
#'   \item{\code{from}}{Originating state.}
#'   \item{\code{to}}{Destination state.}
#'   \item{\code{count}}{Number of observed transitions.}
#'   \item{\code{share}}{Proportion of transitions from `from` that go to `to` (rounded).}
#' }
#'
#' The object has class `"panel_summary"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List with the vector of all category levels.}
#' }
#'
#' @note
#' Before analysis, rows with missing values (`NA`) in the entity or time variables are removed.
#' Messages indicate how many rows were excluded.
#'
#' Duplicate entity‑time combinations are checked. If duplicates exist with differing values of the
#' `select` variable, an error is raised. If all duplicates have identical values, they are collapsed
#' to one row per entity‑time (first occurrence kept) and a message is printed.
#'
#' Missing values in the variable of interest (`select`) are removed before analysis.
#' The variable is coerced to a factor if not already one, and must have at least two levels.
#'
#' @seealso
#' [describe_patterns()] for entity presence patterns.
#' [summarize_missing()] for missing value summaries.
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' summarize_transition(production, select = "industry", index = c("firm", "year"))
#'
#' # With panel_data object
#' panel <- make_panel(production, index = c("firm", "year"))
#' summarize_transition(panel, select = "industry")
#'
#' # Returning results in a long format
#' summarize_transition(production, select = "industry",
#'                      index = c("firm", "year"), format = "long")
#'
#' # Custom rounding
#' summarize_transition(production, select = "industry", index = c("firm", "year"), digits = 2)
#'
#' # Accessing attributes
#' out_sum_tra <- summarize_transition(production, select = "industry", index = c("firm", "year"))
#' attr(out_sum_tra, "metadata")
#' attr(out_sum_tra, "details")
#'
#' @export
summarize_transition <- function(
  data,
  select,
  index = NULL,
  format = "wide",
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
  if (missing(select) || !is.character(select) || length(select) != 1) {
    stop("'select' must be a single character string")
  }
  if (!select %in% names(data)) {
    stop('variable "', select, '" not found in data')
  }
  if (select == entity_var) {
    stop("'select' cannot be the entity variable '", entity_var, "'")
  }
  if (select == time_var) {
    stop("'select' cannot be the time variable '", time_var, "'")
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
  if (!format %in% c("wide", "long")) {
    stop('format must be "wide" or "long"')
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

  # Convert to character for duplicate handling
  df <- as.data.frame(data)
  df[[entity_var]] <- as.character(df[[entity_var]])
  df[[time_var]] <- as.character(df[[time_var]])

  # Ensure select is factor
  if (!is.factor(df[[select]])) {
    message(
      "Converting variable '",
      select,
      "' to factor. Original class: ",
      class(df[[select]])[1]
    )
    msg_printed <- TRUE
    df[[select]] <- factor(df[[select]])
  }

  if (length(levels(df[[select]])) < 2) {
    stop("variable '", select, "' must have at least 2 levels")
  }

  # --- Duplicate check ---
  dup_combinations <- NULL
  dup_rows <- duplicated(df[c(entity_var, time_var)]) |
    duplicated(df[c(entity_var, time_var)], fromLast = TRUE)
  if (any(dup_rows)) {
    dup_combinations <- unique(df[
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

    # Check consistency of select values
    inconsistent <- FALSE
    for (i in seq_len(nrow(dup_combinations))) {
      grp <- dup_combinations[i, entity_var]
      tm <- dup_combinations[i, time_var]
      rows <- df[[entity_var]] == grp & df[[time_var]] == tm
      vals <- df[[select]][rows]
      if (length(unique(vals)) > 1) {
        inconsistent <- TRUE
        break
      }
    }

    if (inconsistent) {
      stop(
        "Duplicate entity-time combinations with differing values of '",
        select,
        "' found. Ambiguous state."
      )
    } else {
      df <- df[!duplicated(df[c(entity_var, time_var)]), ]
      if (!entity_time_from_metadata) {
        message(
          "Duplicate rows with identical '",
          select,
          "' values collapsed."
        )
        msg_printed <- TRUE
      }
    }
  }

  # Remove NA in select
  complete <- !is.na(df[[select]])
  if (sum(!complete) > 0) {
    message(
      "Removing ",
      sum(!complete),
      " rows with NA values in '",
      select,
      "'"
    )
    msg_printed <- TRUE
    df <- df[complete, ]
  }

  # Order by entity and time
  df <- df[order(df[[entity_var]], df[[time_var]]), ]

  # Compute transitions by entity
  transitions <- by(df, df[[entity_var]], function(gdat) {
    if (nrow(gdat) > 1) {
      from_vals <- gdat[[select]][-nrow(gdat)]
      to_vals <- gdat[[select]][-1]
      data.frame(from = from_vals, to = to_vals, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })

  trans_df <- do.call(rbind, transitions)
  if (is.null(trans_df)) {
    stop("no transitions found; need multiple time periods per entity")
  }

  all_levels <- levels(df[[select]])
  trans_df$from <- factor(trans_df$from, levels = all_levels)
  trans_df$to <- factor(trans_df$to, levels = all_levels)

  count_table <- table(trans_df$from, trans_df$to)
  count_df <- as.data.frame(count_table, stringsAsFactors = FALSE)
  names(count_df) <- c("from", "to", "count")
  count_df <- count_df[count_df$count > 0, ]

  from_totals <- tapply(count_df$count, count_df$from, sum, na.rm = TRUE)

  # Long result with all combinations
  complete_grid <- expand.grid(
    from = all_levels,
    to = all_levels,
    stringsAsFactors = FALSE
  )
  long_result <- merge(
    complete_grid,
    count_df,
    by = c("from", "to"),
    all.x = TRUE
  )
  long_result$count[is.na(long_result$count)] <- 0
  long_result$share <- long_result$count /
    from_totals[as.character(long_result$from)]
  long_result$share[is.na(long_result$share)] <- 0
  long_result$share <- round_if_needed(long_result$share, digits)
  long_result <- long_result[order(long_result$from, long_result$to), ]
  rownames(long_result) <- NULL

  # Wide format
  wide_matrix <- matrix(
    long_result$share,
    nrow = length(all_levels),
    ncol = length(all_levels),
    byrow = FALSE,
    dimnames = list(from = all_levels, to = all_levels)
  )
  wide_result <- as.data.frame(wide_matrix)
  wide_result <- cbind(from_to = all_levels, wide_result)
  rownames(wide_result) <- NULL

  out <- if (format == "wide") wide_result else long_result

  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    select = select,
    entity = entity_var,
    time = time_var,
    format = format,
    digits = digits
  )

  details <- list(categories = all_levels)

  attr(out, "metadata") <- metadata
  attr(out, "details") <- details
  class(out) <- c("panel_summary", "data.frame")

  if (msg_printed) {
    cat("\n")
  }

  return(out)
}
