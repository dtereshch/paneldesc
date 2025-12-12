#' Providing a descriptive table of entities with incomplete observations (missing values)
#'
#' @param data The data frame
#' @param group Entities' identifier
#' @param time Time identifier (optional, for checking panel balance)
#' @param detailed Logical indicating whether to include detailed missing counts
#'        for each variable (TRUE) or just summary counts (FALSE). Default is FALSE.
#' @return A data frame containing entities with number of variables that have
#'         at least one missing value for that entity, as well as total number
#'         of missing observations. The data frame is arranged by number of
#'         variables with missing values. If detailed = TRUE, includes additional
#'         columns with NA counts for each variable.
#'
#' @seealso [find_incomplete()], [describe_participation()], [plot_participation()], [explore_participation()]
#'
#' @examples
#' data(production)
#'
#' # Using regular data frame - summary view
#' explore_incomplete(production, group = "firm")
#'
#' # Detailed view with variable-level NA counts
#' explore_incomplete(production, group = "firm", detailed = TRUE)
#'
#' # Check for panel balance
#' explore_incomplete(production, group = "firm", time = "year")
#'
#' @export
explore_incomplete <- function(data, group, time = NULL, detailed = FALSE) {
  # Input validation
  data <- .check_and_convert_data_robust(data, arg_name = "data")

  # Check if group is provided
  if (missing(group)) {
    stop("Argument 'group' is required")
  }

  # Validate detailed parameter
  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("Argument 'detailed' must be a single logical value (TRUE or FALSE)")
  }

  # Validate group variable
  if (!group %in% names(data)) {
    stop("Group variable '", group, "' not found in data")
  }

  # Check if group variable has valid values
  if (length(data[[group]]) == 0) {
    stop("Group variable has no observations")
  }

  # Check panel balance if time variable is provided
  if (!is.null(time)) {
    if (!time %in% names(data)) {
      stop("Time variable '", time, "' not found in data")
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
    stop("No variables to analyze (only group and time variables found)")
  }

  # Initialize base results with original group type
  result <- data.frame(
    group = unique_groups,
    n_vars_with_na = 0,
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

    result$n_vars_with_na[i] <- vars_with_na
    result$total_na[i] <- total_na

    # If detailed = TRUE, add NA counts for each variable
    if (detailed) {
      for (var in vars) {
        result[[var]][i] <- sum(is.na(group_data[[var]]))
      }
    }
  }

  # Filter groups with any missing variables and arrange
  result <- result[result$n_vars_with_na > 0, ]

  # Check if there are any incomplete groups
  if (nrow(result) == 0) {
    return("There are no incomplete groups/entities in the data.")
  }

  # Sort by primary and secondary criteria
  result <- result[order(-result$n_vars_with_na, -result$total_na), ]

  # Reset row names
  rownames(result) <- NULL

  return(result)
}
