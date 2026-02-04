#' Incomplete Entities Description
#'
#' This function provides a descriptive table of entities with incomplete observations (missing values).
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#' @param time A character string specifying the name of the time variable (optional, for checking panel balance).
#' @param detailed A logical flag indicating whether to include detailed missing counts
#'        for each variable. Default = FALSE.
#'
#' @return A data.frame with incomplete entities description or a character message.
#'
#' @details
#' When incomplete entities exist, returns a data.frame with:
#' \describe{
#'   \item{\code{[group]}}{The entity/group identifier (name matches input `group`)}
#'   \item{\code{vars_with_na}}{Number of variables with at least one missing value for that entity}
#'   \item{\code{total_na}}{Total number of missing observations for the entity}
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
#' If no entities have incomplete data, returns the character message:
#' "There are no incomplete groups/entities in the data."
#'
#' @seealso
#' [explore_participation()], [describe_participation()], [explore_balance()], [describe_balance()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' describe_incomplete(production, group = "firm")
#'
#' # More careful usage with panel balance check
#' describe_incomplete(production, group = "firm", time = "year")
#'
#' # Detailed view with variable-level NA counts
#' describe_incomplete(production, group = "firm", detailed = TRUE)
#'
#' @export
describe_incomplete <- function(
  data,
  group,
  time = NULL,
  detailed = FALSE
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
  }

  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  if (!is.null(time) && (!is.character(time) || length(time) != 1)) {
    stop(
      "'time' must be a single character string or NULL, not ",
      class(time)[1]
    )
  }

  if (!is.null(time) && !time %in% names(data)) {
    stop('variable "', time, '" not found in data')
  }

  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("'detailed' must be a single logical value, not ", class(detailed)[1])
  }

  data <- .check_and_convert_data_robust(data, arg_name = "data")

  # Validate detailed parameter
  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("argument 'detailed' must be a single logical value")
  }

  # Validate group variable
  if (!group %in% names(data)) {
    stop("variable '", group, "' not found in data")
  }

  # Check if group variable has valid values
  if (length(data[[group]]) == 0) {
    stop("group variable has no observations")
  }

  # Check panel balance if time variable is provided
  if (!is.null(time)) {
    if (!time %in% names(data)) {
      stop("variable '", time, "' not found in data")
    }

    # Check if panel is unbalanced
    # Use table with the original variables (preserving types)
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
    vars_with_na = 0,
    total_na = 0,
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

    # Use logical indexing that preserves data types
    group_indices <- data[[group]] == current_group
    group_data <- data[group_indices, vars, drop = FALSE]

    # Count variables with at least one NA
    vars_with_na <- sum(vapply(
      group_data,
      function(x) any(is.na(x)),
      logical(1)
    ))

    # Count total number of NA observations
    total_na <- sum(vapply(group_data, function(x) sum(is.na(x)), numeric(1)))

    result$vars_with_na[i] <- vars_with_na
    result$total_na[i] <- total_na

    # If detailed = TRUE, add NA counts for each variable
    if (detailed) {
      for (var in vars) {
        result[[var]][i] <- sum(is.na(group_data[[var]]))
      }
    }
  }

  # Filter groups with any missing variables and arrange
  result <- result[result$vars_with_na > 0, ]

  # Check if there are any incomplete groups
  if (nrow(result) == 0) {
    return("There are no incomplete groups/entities in the data.")
  }

  # Sort by primary and secondary criteria
  result <- result[order(-result$vars_with_na, -result$total_na), ]

  # Reset row names
  rownames(result) <- NULL

  return(result)
}
