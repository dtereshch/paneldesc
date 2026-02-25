#' Panel Data Dimensions Description
#'
#' This function provides basic dimension counts for panel data:
#' number of rows, unique entities, unique time periods, and substantive variables.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable in panel data.
#'        Not required if data has panel attributes.
#'
#' @return A data.frame with panel dimension counts. Contains two columns:
#'   \describe{
#'     \item{\code{dimension}}{Dimension name: "rows", "entities", "periods", "variables"}
#'     \item{\code{count}}{Corresponding count (integer).}
#'   }
#'
#' @details
#' The counts are defined as follows:
#' \itemize{
#'   \item{\bold{rows}}{ Total number of rows in the data frame.}
#'   \item{\bold{entities}}{ Number of distinct values in the group variable.}
#'   \item{\bold{periods}}{ Number of distinct values in the time variable.}
#'   \item{\bold{variables}}{ Number of substantive variables (all columns except group and time).}
#' }
#'
#' Before analysis, rows with missing values (`NA`) in the `group` or `time` variables are removed.
#' Messages indicate how many rows were excluded due to each variable. The excluded rows are stored in
#' `details$excluded_rows` for further inspection.
#'
#' The function also checks for duplicate group-time combinations. In a properly structured panel dataset,
#' each entity (group) should have at most one observation per time period. If duplicates are found,
#' they are stored in `details$entity_time_duplicates`. A message is printed only when the identifiers
#' were explicitly provided (i.e., not taken from `panel_data` attributes).
#'
#' The returned data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing:
#'         \itemize{
#'           \item{\code{entities}: Vector of unique entity identifiers (original class).}
#'           \item{\code{periods}: Vector of unique time period identifiers (original class).}
#'           \item{\code{variables}: Character vector of substantive variable names.}
#'           \item{\code{excluded_rows}: Data frame of rows removed due to missing group/time.}
#'           \item{\code{entity_time_duplicates}: If duplicates were found, a data frame
#'                 containing the distinct duplicate combinations.}
#'         }
#'   }
#' }
#'
#' @seealso
#' [describe_balance()], [describe_periods()], [describe_patterns()], [set_panel()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' describe_dimensions(production, group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' describe_dimensions(panel_data)
#'
#' # Access detailed information
#' dims <- describe_dimensions(production, group = "firm", time = "year")
#' attr(dims, "details")$entities
#' attr(dims, "details")$variables
#'
#' @export
describe_dimensions <- function(data, group = NULL, time = NULL) {
  # Helper to sort unique values preserving original class
  sort_unique_preserve <- function(x) {
    ux <- unique(x)
    if (is.numeric(ux)) {
      sort(ux)
    } else if (inherits(ux, "Date") || inherits(ux, "POSIXt")) {
      sort(ux)
    } else if (is.factor(ux)) {
      char_lev <- as.character(ux)
      sorted_char <- sort(char_lev)
      factor(sorted_char, levels = sorted_char, ordered = is.ordered(ux))
    } else {
      sort(ux) # character, logical, etc.
    }
  }

  # --- Consistent initialisation ---
  user_group <- group
  user_time <- time
  group_time_from_metadata <- FALSE

  if (inherits(data, "panel_data")) {
    metadata <- attr(data, "metadata")
    if (
      is.null(metadata) || is.null(metadata$group) || is.null(metadata$time)
    ) {
      stop(
        "Object has class 'panel_data' but missing or incomplete 'metadata' attribute."
      )
    }
    if (is.null(group)) {
      group <- metadata$group
    }
    if (is.null(time)) {
      time <- metadata$time
    }

    group_from_metadata <- is.null(user_group) && !is.null(metadata$group)
    time_from_metadata <- is.null(user_time) && !is.null(metadata$time)
    group_time_from_metadata <- group_from_metadata && time_from_metadata
  } else {
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame")
    }
    if (is.null(group) || is.null(time)) {
      stop(
        "For regular data.frames, both 'group' and 'time' arguments must be provided"
      )
    }
  }

  # Common validation
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }

  if (!group %in% names(data)) {
    stop('group variable "', group, '" not found in data')
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string, not ", class(time)[1])
  }

  if (!time %in% names(data)) {
    stop('time variable "', time, '" not found in data')
  }

  if (time == group) {
    stop("'time' and 'group' cannot be the same variable")
  }

  # --- Remove rows with NA in group or time ---
  excluded_rows <- NULL
  na_group <- is.na(data[[group]])
  na_time <- is.na(data[[time]])

  if (any(na_group)) {
    message(
      "Missing values in ",
      group,
      " variable found. Excluding ",
      sum(na_group),
      " rows."
    )
  }
  if (any(na_time)) {
    message(
      "Missing values in ",
      time,
      " variable found. Excluding ",
      sum(na_time),
      " rows."
    )
  }

  if (any(na_group | na_time)) {
    excluded_rows <- data[na_group | na_time, , drop = FALSE]
    data <- data[!(na_group | na_time), , drop = FALSE]
    rownames(data) <- NULL
  }
  # ----------------------------------------------------------------

  # --- Check for duplicate group-time combinations ---
  dup_combinations <- NULL
  dup_rows <- duplicated(data[c(group, time)]) |
    duplicated(data[c(group, time)], fromLast = TRUE)
  if (any(dup_rows)) {
    dup_combinations <- unique(data[dup_rows, c(group, time), drop = FALSE])
    n_dup <- nrow(dup_combinations)
    if (!group_time_from_metadata) {
      examples <- utils::head(dup_combinations, 5)
      example_strings <- paste0(examples[[group]], "-", examples[[time]])
      example_str <- paste(example_strings, collapse = ", ")
      message(
        n_dup,
        " duplicate group-time combinations found. Examples: ",
        example_str
      )
    }
  }
  # ----------------------------------------------------

  # Original vectors
  group_orig <- data[[group]]
  time_orig <- data[[time]]

  # Unique values, sorted, preserving original class (for details)
  entities_vals <- sort_unique_preserve(group_orig)
  periods_vals <- sort_unique_preserve(time_orig)

  # Substantive variables (all except group and time)
  substantive_vars <- setdiff(names(data), c(group, time))

  # Counts
  counts <- c(
    rows = nrow(data),
    entities = length(entities_vals),
    periods = length(periods_vals),
    variables = length(substantive_vars)
  )

  # Result data.frame
  result <- data.frame(
    dimension = names(counts),
    count = as.integer(counts),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time
  )

  # Build details list with original class vectors and variable names
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

  # Set attributes and class
  attr(result, "metadata") <- metadata
  attr(result, "details") <- details
  class(result) <- c("panel_description", "data.frame")

  return(result)
}
