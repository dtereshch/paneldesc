#' Panel Data Dimensions Description
#'
#' This function provides basic dimension counts for panel data:
#' number of rows, unique entities, unique time periods, and substantive variables.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the entity and time variables.
#'        Not required if data has panel attributes.
#'
#' @return A data.frame containing panel dimension counts.
#'
#' @details
#' The returned data.frame has the following structure:
#' \itemize{
#'   \item{\code{rows}}{ Total number of rows in the data frame.}
#'   \item{\code{entities}}{ Number of distinct values in the entity variable.}
#'   \item{\code{periods}}{ Number of distinct values in the time variable.}
#'   \item{\code{variables}}{ Number of substantive variables (all columns except entity and time).}
#' }
#'
#' The object has class `"panel_description"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List with the actual vectors of entities, periods, and substantive variables.}
#' }
#'
#' @seealso
#' See also [describe_balance()], [describe_periods()].
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' describe_dimensions(production, index = c("firm", "year"))
#'
#' # With panel_data object
#' panel <- make_panel(production, index = c("firm", "year"))
#' describe_dimensions(panel)
#'
#' # Accessing attributes
#' out_des_dim <- describe_dimensions(production, index = c("firm", "year"))
#' attr(out_des_dim, "metadata")
#' attr(out_des_dim, "details")
#'
#' @export
describe_dimensions <- function(data, index = NULL) {
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
    stop('entity variable "', entity_var, '" not found in data')
  }
  if (!time_var %in% names(data)) {
    stop('time variable "', time_var, '" not found in data')
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

  # Original vectors
  entity_orig <- data[[entity_var]]
  time_orig <- data[[time_var]]

  entities_vals <- sort_unique_preserve(entity_orig)
  periods_vals <- sort_unique_preserve(time_orig)
  substantive_vars <- setdiff(names(data), c(entity_var, time_var))

  # Create output data frame (single row with named columns)
  out <- data.frame(
    rows = nrow(data),
    entities = length(entities_vals),
    periods = length(periods_vals),
    variables = length(substantive_vars),
    stringsAsFactors = FALSE
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

  attr(out, "metadata") <- metadata
  attr(out, "details") <- details
  class(out) <- c("panel_description", "data.frame")

  if (msg_printed) {
    cat("\n")
  }

  return(out)
}
