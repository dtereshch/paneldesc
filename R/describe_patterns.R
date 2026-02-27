#' Entities Presence Patterns Description
#'
#' This function describes entities presence patterns in panel data over time.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the entity and time variables.
#'        Not required if data has panel attributes.
#' @param delta An optional positive integer giving the expected interval between time periods.
#' @param limits An integer specifying the maximum number of distinct patterns to display.
#'        If not specified, all patterns are shown.
#' @param format A character string specifying the output format: "wide" or "long". Default = "wide".
#' @param detail A logical flag indicating whether to return detailed patterns. Default = TRUE.
#' @param digits An integer specifying the number of decimal places for rounding share column.
#'        Default = 3.
#'
#' @return A data.frame with presence patterns, class `"panel_description"`.
#'
#' @details
#' An entity/time combination is considered **present** if the corresponding row contains at least
#' one non-NA value in any substantive variable (i.e., all columns except the entity and time identifiers).
#'
#' Before analysis, rows with missing values (`NA`) in the entity or time variables are removed.
#' Messages indicate how many rows were excluded. The excluded rows are stored in `details$excluded_rows`.
#'
#' If `delta` is supplied, the time variable is coerced to numeric (if possible). The function checks that
#' all observed time points are compatible with a regular spacing of that interval. If gaps are detected,
#' a message lists the missing periods (unless the interval was inherited from panel attributes), and columns
#' for those periods are added to the presence matrix (all zeros) before computing patterns.
#'
#' The function also checks for duplicate entity-time combinations. If duplicates are found, they are stored in
#' `details$entity_time_duplicates` and a message is printed (unless the identifiers came from panel attributes).
#'
#' Patterns are sorted by frequency (most common first). If `limits` is supplied, only the most frequent
#' patterns are retained.
#'
#' @seealso \code{\link{plot_patterns}}, \code{\link{describe_periods}}, \code{\link{describe_balance}}
#'
#' @examples
#' data(production)
#' describe_patterns(production, index = c("firm", "year"))
#' describe_patterns(production, index = c("firm", "year"), delta = 1, limits = 3)
#'
#' @export
describe_patterns <- function(
  data,
  index = NULL,
  delta = NULL,
  limits = NULL,
  format = "wide",
  detail = TRUE,
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
  if (!is.logical(detail) || length(detail) != 1) {
    stop("'detail' must be a single logical")
  }
  if (!format %in% c("wide", "long")) {
    stop('format must be "wide" or "long"')
  }
  if (
    !is.null(limits) &&
      (!is.numeric(limits) || limits < 1 || limits != round(limits))
  ) {
    stop("'limits' must be a positive integer or NULL")
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
  entity_char <- as.character(entity_orig)
  time_char <- as.character(time_orig)

  unique_entities_orig <- sort_unique_preserve(entity_orig)
  unique_entities_char <- as.character(unique_entities_orig)

  if (!is.null(delta)) {
    time_cols_numeric <- full_seq
    time_cols_char <- as.character(time_cols_numeric)
  } else {
    time_cols_char <- as.character(sort_unique_preserve(time_orig))
  }

  data_cols <- setdiff(names(data), c(entity_var, time_var))
  if (length(data_cols) == 0) {
    stop("no data columns found (excluding entity and time)")
  }

  # Filter rows with at least one non-NA in data columns
  has_data <- apply(data[data_cols], 1, function(row) !all(is.na(row)))
  data_filtered <- data[has_data, ]

  if (nrow(data_filtered) > 0) {
    entity_filt_char <- as.character(data_filtered[[entity_var]])
    time_filt_char <- as.character(data_filtered[[time_var]])
  } else {
    entity_filt_char <- character(0)
    time_filt_char <- character(0)
  }

  # Presence matrix
  presence_binary <- matrix(
    0,
    nrow = length(unique_entities_orig),
    ncol = length(time_cols_char),
    dimnames = list(unique_entities_char, time_cols_char)
  )
  for (i in seq_along(entity_filt_char)) {
    if (time_filt_char[i] %in% time_cols_char) {
      presence_binary[entity_filt_char[i], time_filt_char[i]] <- 1
    }
  }

  # Patterns
  pattern_strings <- apply(presence_binary, 1, paste, collapse = "")
  pattern_counts <- table(pattern_strings)

  # patterns_entities list
  patterns_entities <- list()
  for (i in seq_along(unique_entities_orig)) {
    ent_orig <- unique_entities_orig[i]
    ent_char <- unique_entities_char[i]
    pat <- pattern_strings[i]
    if (!pat %in% names(patterns_entities)) {
      patterns_entities[[pat]] <- ent_orig[0]
    }
    patterns_entities[[pat]] <- c(patterns_entities[[pat]], ent_orig)
  }

  # Build result data frame (wide)
  result <- data.frame(
    pattern = seq_along(pattern_counts),
    stringsAsFactors = FALSE
  )
  for (t in time_cols_char) {
    result[[t]] <- as.numeric(substr(
      names(pattern_counts),
      which(time_cols_char == t),
      which(time_cols_char == t)
    ))
  }
  result$count <- as.numeric(pattern_counts)
  result$share <- round_if_needed(result$count / sum(result$count), digits)
  result <- result[order(-result$count), ]
  result$pattern <- seq_len(nrow(result))
  rownames(result) <- NULL

  if (!is.null(limits)) {
    result <- result[seq_len(min(limits, nrow(result))), , drop = FALSE]
  }

  # Reorder patterns_entities to match sorted result
  sorted_pattern_strings <- apply(
    result[time_cols_char],
    1,
    paste,
    collapse = ""
  )
  patterns_entities_sorted <- list()
  for (i in seq_along(sorted_pattern_strings)) {
    pat <- sorted_pattern_strings[i]
    patterns_entities_sorted[[as.character(i)]] <- patterns_entities[[pat]]
  }

  patterns_matrix <- as.matrix(result[, time_cols_char, drop = FALSE])
  rownames(patterns_matrix) <- result$pattern

  details <- list(
    count_patterns = nrow(result),
    presence_matrix = presence_binary,
    patterns_entities = patterns_entities_sorted,
    patterns_matrix = patterns_matrix
  )
  if (!is.null(excluded_rows)) {
    details$excluded_rows <- excluded_rows
  }
  if (!is.null(dup_combinations)) {
    details$entity_time_duplicates <- dup_combinations
  }

  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    entity = entity_var,
    time = time_var,
    delta = delta,
    limits = limits,
    format = format,
    detail = detail,
    digits = digits
  )

  # Long format conversion if requested
  if (format == "long") {
    long_result <- data.frame()
    for (i in seq_len(nrow(result))) {
      row_i <- result[i, ]
      for (t in time_cols_char) {
        long_result <- rbind(
          long_result,
          data.frame(
            pattern = row_i$pattern,
            time = t,
            presence = as.integer(row_i[[t]]),
            count = row_i$count,
            share = row_i$share,
            stringsAsFactors = FALSE
          )
        )
      }
    }
    names(long_result)[names(long_result) == "time"] <- time_var
    if (!detail) {
      long_result <- long_result[c("pattern", time_var, "presence")]
    }
    out <- long_result
  } else {
    if (!detail) {
      out <- result[c("pattern", time_cols_char)]
    } else {
      out <- result
    }
  }

  attr(out, "metadata") <- metadata
  attr(out, "details") <- details
  class(out) <- c("panel_description", "data.frame")
  return(out)
}
