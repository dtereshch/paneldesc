#' Panel Data Dimensions Description
#'
#' This function provides basic dimension counts for panel data:
#' number of rows, unique entities, unique time periods, and substantive variables.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the entity and time variables.
#'        Not required if data has panel attributes.
#'
#' @return A data.frame with panel dimension counts, class `"panel_description"`.
#'
#' @details
#' The counts are defined as follows:
#' \itemize{
#'   \item{\bold{rows}}{ Total number of rows in the data frame.}
#'   \item{\bold{entities}}{ Number of distinct values in the entity variable.}
#'   \item{\bold{periods}}{ Number of distinct values in the time variable.}
#'   \item{\bold{variables}}{ Number of substantive variables (all columns except entity and time).}
#' }
#'
#' Before analysis, rows with missing values (`NA`) in the entity or time variables are removed.
#' Messages indicate how many rows were excluded. The excluded rows are stored in `details$excluded_rows`.
#'
#' Duplicate entity-time combinations are checked; if found, they are stored in `details$entity_time_duplicates`
#' and a message is printed (unless identifiers came from panel attributes).
#'
#' @seealso \code{\link{describe_balance}}, \code{\link{describe_periods}}, \code{\link{describe_patterns}}, \code{\link{make_panel}}
#'
#' @examples
#' data(production)
#' describe_dimensions(production, index = c("firm", "year"))
#'
#' # With panel attributes
#' panel_data <- make_panel(production, index = c("firm", "year"))
#' describe_dimensions(panel_data)
#'
#' @export
describe_dimensions <- function(data, index = NULL) {
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
    stop('entity variable "', entity_var, '" not found in data')
  }
  if (!time_var %in% names(data)) {
    stop('time variable "', time_var, '" not found in data')
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

  # Original vectors
  entity_orig <- data[[entity_var]]
  time_orig <- data[[time_var]]

  entities_vals <- sort_unique_preserve(entity_orig)
  periods_vals <- sort_unique_preserve(time_orig)
  substantive_vars <- setdiff(names(data), c(entity_var, time_var))

  counts <- c(
    rows = nrow(data),
    entities = length(entities_vals),
    periods = length(periods_vals),
    variables = length(substantive_vars)
  )

  result <- data.frame(
    dimension = names(counts),
    count = as.integer(counts),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    entity = entity_var,
    time = time_var
  )

  details <- list(
    entities = entities_vals,
    periods = periods_vals,
    variables = substantive_vars
  )
  if (!is.null(excluded_rows)) {
    details$excluded_rows <- excluded_rows
  }
  if (!is.null(dup_combinations)) {
    details$entity_time_duplicates <- dup_combinations
  }

  attr(result, "metadata") <- metadata
  attr(result, "details") <- details
  class(result) <- c("panel_description", "data.frame")

  return(result)
}
