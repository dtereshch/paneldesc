#' Incomplete Entities Description
#'
#' This function provides a descriptive table of entities with incomplete observations (missing values).
#'
#' @param data A data.frame containing panel data in a long format.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time An optional character string specifying the name of the time variable.
#'        If provided, the function checks for duplicate group-time combinations.
#'        Not required if data has panel attributes.
#' @param detailed A logical flag indicating whether to include detailed missing counts for each variable.
#'        Default = FALSE.
#'
#' @return A data.frame with incomplete entities description or a character message.
#'
#' @details
#' When incomplete entities exist, returns a data.frame with:
#' \describe{
#'   \item{\code{[group]}}{The entity/group identifier (name matches input `group`)}
#'   \item{\code{na_count}}{Total number of missing observations for the entity}
#'   \item{\code{variables}}{Number of variables with at least one missing value for that entity}
#' }
#'
#' When `detailed = TRUE`, additional columns are included:
#' \describe{
#'   \item{\code{[variable1]}}{Number of NAs in variable1 for the entity}
#'   \item{\code{[variable2]}}{Number of NAs in variable2 for the entity}
#'   \item{...}{Additional columns for each substantive variable in the data}
#' }
#'
#' The data.frame is sorted by:
#' 1. Number of variables with NAs (descending)
#' 2. Total number of NAs (descending)
#'
#' Before analysis, rows with missing values (`NA`) in the `group` or `time` (if provided)
#' variables are removed. Messages indicate how many rows were excluded due to each variable.
#' The excluded rows are stored in `details$excluded_rows` for further inspection.
#'
#' If no entities have incomplete data, returns the character message:
#' "There are no incomplete groups/entities in the data."
#'
#' If a time variable is supplied (either by the user or from `panel_data` metadata),
#' the function checks for duplicate group-time combinations. In a properly structured
#' panel dataset, each entity (group) should have at most one observation per time period.
#' If duplicates are found, they are stored in `details$entity_time_duplicates`.
#' A message is printed only when the identifiers were explicitly provided (i.e., not taken
#' from `panel_data` attributes).
#'
#' The returned data.frame (if any) has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing additional information:
#'         `count_entities_total`, `count_entities_incomplete`, `entities_incomplete`,
#'         `excluded_rows` (if any), and (if time was supplied and duplicates exist)
#'         `entity_time_duplicates`.}
#' }
#'
#' @seealso
#' [summarize_missing()], [describe_patterns()], [describe_balance()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' describe_incomplete(production, group = "firm")
#'
#' # More careful usage with panel balance check and duplicate check
#' describe_incomplete(production, group = "firm", time = "year")
#'
#' # Detailed view with variable-level NA counts
#' describe_incomplete(production, group = "firm", detailed = TRUE)
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' describe_incomplete(panel_data)
#'
#' @export
describe_incomplete <- function(
  data,
  group = NULL,
  time = NULL,
  detailed = FALSE
) {
  # --- Consistent initialisation ---
  user_group <- group
  user_time <- time
  group_time_from_metadata <- FALSE

  if (inherits(data, "panel_data")) {
    metadata <- attr(data, "metadata")
    if (is.null(metadata) || is.null(metadata$group)) {
      stop(
        "Object has class 'panel_data' but missing or incomplete 'metadata' attribute."
      )
    }
    if (is.null(group)) {
      group <- metadata$group
    }
    # time is optional: use metadata only if user didn't provide and metadata has time
    if (is.null(time) && !is.null(metadata$time)) {
      time <- metadata$time
    }

    group_from_metadata <- is.null(user_group) && !is.null(metadata$group)
    time_from_metadata <- is.null(user_time) && !is.null(metadata$time)
    group_time_from_metadata <- group_from_metadata &&
      (is.null(time) || time_from_metadata)
  } else {
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame")
    }
    if (is.null(group)) {
      stop("For regular data.frames, 'group' argument must be provided")
    }
    # time may be NULL
  }

  # Common validation for group
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }
  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  # Validate time if provided
  if (!is.null(time)) {
    if (!is.character(time) || length(time) != 1) {
      stop("'time' must be a single character string, not ", class(time)[1])
    }
    if (!time %in% names(data)) {
      stop('variable "', time, '" not found in data')
    }
    if (time == group) {
      stop("'time' and 'group' must be different variables")
    }
  }

  # --- Remove rows with NA in group or time (if time provided) ---
  excluded_rows <- NULL
  na_group <- is.na(data[[group]])
  na_time <- if (!is.null(time)) is.na(data[[time]]) else rep(FALSE, nrow(data))

  if (any(na_group)) {
    message(
      "Missing values in ",
      group,
      " variable found. Excluding ",
      sum(na_group),
      " rows."
    )
  }
  if (!is.null(time) && any(na_time)) {
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

  # --- Check for duplicate group-time combinations (only if time is provided) ---
  dup_combinations <- NULL
  if (!is.null(time)) {
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
  }
  # -------------------------------------------------------------

  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("'detailed' must be a single logical value, not ", class(detailed)[1])
  }

  # Check if group variable has valid values
  if (length(data[[group]]) == 0) {
    stop("group variable has no observations")
  }

  # Check panel balance if time variable is provided
  if (!is.null(time)) {
    # Check if panel is unbalanced
    time_counts <- table(data[[group]], data[[time]])
    if (!all(time_counts == 1)) {
      warning("The panel is unbalanced.")
    }
  }

  # Get unique groups while preserving original type and order
  unique_groups <- unique(data[[group]])

  # Get variable names excluding group and time variables
  exclude_vars <- group
  if (!is.null(time)) {
    exclude_vars <- c(exclude_vars, time)
  }
  vars <- setdiff(names(data), exclude_vars)

  if (length(vars) == 0) {
    stop("no variables to analyze (only group and time variables found)")
  }

  # Initialize base results with original group type
  result <- data.frame(
    group = unique_groups,
    na_count = 0,
    variables = 0,
    stringsAsFactors = FALSE
  )
  names(result)[1] <- group

  # If detailed = TRUE, pre-allocate columns for each variable
  if (detailed) {
    for (var in vars) {
      result[[var]] <- 0
    }
  }

  # For each group, calculate missing statistics
  for (i in seq_along(unique_groups)) {
    current_group <- unique_groups[i]

    group_indices <- data[[group]] == current_group
    group_data <- data[group_indices, vars, drop = FALSE]

    vars_with_na <- sum(vapply(
      group_data,
      function(x) any(is.na(x)),
      logical(1)
    ))

    na_count <- sum(vapply(group_data, function(x) sum(is.na(x)), numeric(1)))

    result$variables[i] <- vars_with_na
    result$na_count[i] <- na_count

    if (detailed) {
      for (var in vars) {
        result[[var]][i] <- sum(is.na(group_data[[var]]))
      }
    }
  }

  # Filter groups with any missing variables and arrange
  incomplete_result <- result[result$variables > 0, ]
  incomplete_ids <- result[[group]][result$variables > 0]

  # Check if there are any incomplete groups
  if (nrow(incomplete_result) == 0) {
    return("There are no incomplete groups/entities in the data.")
  }

  # Sort by primary and secondary criteria
  incomplete_result <- incomplete_result[
    order(-incomplete_result$variables, -incomplete_result$na_count),
  ]

  rownames(incomplete_result) <- NULL

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time,
    detailed = detailed
  )

  # Build details list
  details <- list(
    count_entities_total = length(unique_groups),
    count_entities_incomplete = nrow(incomplete_result),
    entities_incomplete = incomplete_ids
  )

  if (!is.null(excluded_rows)) {
    details$excluded_rows <- excluded_rows
  }

  if (!is.null(dup_combinations)) {
    details$entity_time_duplicates <- dup_combinations
  }

  # Set attributes
  attr(incomplete_result, "metadata") <- metadata
  attr(incomplete_result, "details") <- details
  class(incomplete_result) <- c("panel_description", "data.frame")

  return(incomplete_result)
}
