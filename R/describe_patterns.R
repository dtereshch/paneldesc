#' Entities Presence Patterns Description
#'
#' This function describes entities presence patterns in panel data over time.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the entity and time variables.
#'        Not required if data has panel attributes.
#' @param delta An optional integer giving the expected interval between time periods.
#' @param limits Either a single integer (show that many most frequent patterns)
#'        or a vector of two integers (show patterns with ranks between the two values, inclusive).
#'        If not specified, all patterns are shown.
#' @param detail A logical flag indicating whether to return detailed patterns. Default = TRUE.
#' @param format A character string specifying the output format: "wide" or "long". Default = "wide".
#' @param digits An integer specifying the number of decimal places for rounding share column.
#'        Default = 3.
#'
#' @return A data.frame with presence patterns.
#'
#' @details
#' An entity/time combination is considered **present** if the corresponding row contains at least
#' one non‑NA value in any substantive variable (i.e., all columns except the entity and time identifiers).
#'
#' The output format is controlled by `format` and `detail`.
#'
#' When `format = "wide"` and `detail = TRUE` (default):
#' \describe{
#'   \item{\code{pattern}}{Pattern number (ranked by frequency).}
#'   \item{\code{[time1], [time2], ...}}{Presence (1) / absence (0) for each time period.}
#'   \item{\code{count}}{Number of entities sharing this pattern.}
#'   \item{\code{share}}{Proportion of entities with this pattern (rounded to `digits`).}
#' }
#'
#' When `format = "wide"` and `detail = FALSE`, only the `pattern` and presence columns are returned.
#'
#' When `format = "long"` and `detail = TRUE`:
#' \describe{
#'   \item{\code{pattern}}{Pattern number.}
#'   \item{\code{[time]}}{Time period identifier (name equals the original time variable).}
#'   \item{\code{presence}}{Presence (1) / absence (0).}
#'   \item{\code{count}}{Number of entities with this pattern.}
#'   \item{\code{share}}{Proportion of entities with this pattern.}
#' }
#'
#' When `format = "long"` and `detail = FALSE`, only `pattern`, time, and `presence` columns are returned.
#'
#' **Effect of `delta`:**
#' If `delta` is supplied, the time variable is coerced to numeric (if possible).
#' The function checks that all observed time points are separated by multiples of `delta`.
#' If gaps are detected, a message lists the missing periods (unless the interval was inherited from panel attributes),
#' and columns for those missing periods are added to the presence matrix –
#' and therefore to the output data.frame – with all zeros.
#' This ensures that the patterns reflect the full regular sequence of time periods.
#'
#' The object has class `"panel_description"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List with the full presence matrix, pattern‑entity mapping, and the pattern matrix.}
#' }
#'
#' @note
#' Before analysis, rows with missing values (`NA`) in the entity or time variables are removed.
#' Messages indicate how many rows were excluded.
#'
#' Duplicate entity‑time combinations are checked; if found, a message is printed
#' (unless identifiers came from panel attributes).
#'
#' Patterns are sorted by frequency (most common first). If `limits` is supplied, only the requested
#' patterns are retained.
#'
#' @seealso
#' [plot_patterns()] for visualisation of presence patterns.
#' [describe_periods()] for period‑wise entity counts.
#' [describe_balance()] for balance statistics.
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' describe_patterns(production, index = c("firm", "year"))
#'
#' # With panel_data object
#' panel <- make_panel(production, index = c("firm", "year"))
#' describe_patterns(panel)
#'
#' # Specifying time interval
#' describe_patterns(production, index = c("firm", "year"), delta = 1)
#'
#' # Showing only the top 3 patterns
#' describe_patterns(production, index = c("firm", "year"), limits = 3)
#'
#' # Showing patterns ranked 4 to 6
#' describe_patterns(production, index = c("firm", "year"), limits = c(4, 6))
#'
#' # Returning results in a long format without excessive details
#' describe_patterns(production, index = c("firm", "year"), detail = FALSE, format = "long")
#'
#' # Custom rounding
#' describe_patterns(production, index = c("firm", "year"), digits = 2)
#'
#' # Accessing attributes
#' out_des_pat <- describe_patterns(production, index = c("firm", "year"))
#' attr(out_des_pat, "metadata")
#' attr(out_des_pat, "details")
#'
#' @export
describe_patterns <- function(
  data,
  index = NULL,
  delta = NULL,
  limits = NULL,
  detail = TRUE,
  format = "wide",
  digits = 3
) {
  # --- Initialisation ---
  user_index <- index
  user_delta <- delta
  entity_time_from_metadata <- FALSE
  delta_from_metadata <- FALSE
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
    stop("entity and time variables cannot be the same")
  }
  if (!is.logical(detail) || length(detail) != 1) {
    stop("'detail' must be a single logical")
  }
  if (!format %in% c("wide", "long")) {
    stop('format must be "wide" or "long"')
  }

  # --- Updated limits validation ---
  if (!is.null(limits)) {
    if (length(limits) == 1) {
      if (!is.numeric(limits) || limits < 1 || limits != round(limits)) {
        stop(
          "'limits' must be a positive integer or a vector of two positive integers."
        )
      }
    } else if (length(limits) == 2) {
      if (
        !is.numeric(limits) || any(limits < 1) || any(limits != round(limits))
      ) {
        stop("'limits' must contain positive integers.")
      }
      if (limits[1] > limits[2]) {
        stop("In 'limits = c(m, n)', m must be less than or equal to n.")
      }
    } else {
      stop(
        "'limits' must be a positive integer or a vector of two positive integers."
      )
    }
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
      msg_printed <- TRUE
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
  presence_mat <- matrix(
    0,
    nrow = length(unique_entities_orig),
    ncol = length(time_cols_char),
    dimnames = list(unique_entities_char, time_cols_char)
  )
  for (i in seq_along(entity_filt_char)) {
    if (time_filt_char[i] %in% time_cols_char) {
      presence_mat[entity_filt_char[i], time_filt_char[i]] <- 1
    }
  }

  # Patterns
  pattern_str <- apply(presence_mat, 1, paste, collapse = "")
  pattern_counts <- table(pattern_str)

  # patterns_entities list
  patterns_entities <- list()
  for (i in seq_along(unique_entities_orig)) {
    ent_orig <- unique_entities_orig[i]
    ent_char <- unique_entities_char[i]
    pat <- pattern_str[i]
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

  # --- Updated limits subsetting ---
  if (!is.null(limits)) {
    if (length(limits) == 1) {
      m <- 1
      n <- limits
    } else {
      # length 2
      m <- limits[1]
      n <- limits[2]
    }
    # Additional checks (m and n already validated)
    total_patterns <- nrow(result)
    if (n > total_patterns) {
      stop(
        "n (",
        n,
        ") exceeds the total number of patterns (",
        total_patterns,
        ")."
      )
    }
    result <- result[m:n, , drop = FALSE]
  }

  # Reorder patterns_entities to match sorted result
  sorted_pattern_str <- apply(
    result[time_cols_char],
    1,
    paste,
    collapse = ""
  )
  patterns_entities_sorted <- list()
  for (i in seq_along(sorted_pattern_str)) {
    pat <- sorted_pattern_str[i]
    patterns_entities_sorted[[as.character(i)]] <- patterns_entities[[pat]]
  }

  patterns_matrix <- as.matrix(result[, time_cols_char, drop = FALSE])
  rownames(patterns_matrix) <- result$pattern

  details <- list(
    count_patterns = nrow(result),
    presence_matrix = presence_mat,
    patterns_entities = patterns_entities_sorted,
    patterns_matrix = patterns_matrix
  )

  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    entity = entity_var,
    time = time_var,
    delta = delta,
    limits = limits,
    detail = detail,
    format = format,
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

  if (msg_printed) {
    cat("\n")
  }

  return(out)
}
