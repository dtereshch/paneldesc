#' Providing a table of entities with incomplete observations (missing values)
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
  # Check if data is provided
  if (missing(data)) {
    stop("Argument 'data' is required")
  }

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

  # Extract group variable and handle special types (like pseries from plm)
  group_var <- data[[group]]

  # Convert group variable to appropriate type for consistent handling
  # This is the key fix - same as in find_incomplete
  if (is.factor(group_var) || inherits(group_var, "pseries")) {
    group_var <- as.character(group_var)
  }

  # Check panel balance if time variable is provided
  if (!is.null(time)) {
    if (!time %in% names(data)) {
      stop("Time variable '", time, "' not found in data")
    }

    # Extract time variable and handle special types
    time_var <- data[[time]]
    if (is.factor(time_var) || inherits(time_var, "pseries")) {
      time_var <- as.character(time_var)
    }

    # Check if panel is unbalanced using the converted variables
    time_counts <- tapply(time_var, group_var, function(x) length(unique(x)))
    if (length(unique(time_counts)) > 1) {
      warning("The panel is unbalanced.")
    }
  }

  # Get unique groups using the converted group variable
  unique_groups <- unique(group_var)

  # Get variable names excluding group and time variables
  exclude_vars <- group
  if (!is.null(time)) {
    exclude_vars <- c(exclude_vars, time)
  }
  vars <- setdiff(names(data), exclude_vars)

  if (length(vars) == 0) {
    stop("No variables to analyze (only group and time variables found)")
  }

  # Initialize base results
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

    # Use logical indexing with the converted group variable
    # This avoids the "comparison of these types is not implemented" error
    group_indices <- group_var == current_group
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

  # Convert back to original type if possible (same as find_incomplete)
  original_type <- class(data[[group]])
  if ("numeric" %in% original_type || "integer" %in% original_type) {
    # Handle numeric conversion carefully to avoid warnings
    numeric_groups <- suppressWarnings(as.numeric(result[[group]]))
    if (!any(is.na(numeric_groups))) {
      result[[group]] <- numeric_groups
    }
  } else if ("factor" %in% original_type) {
    result[[group]] <- factor(
      result[[group]],
      levels = levels(data[[group]])
    )
  }

  return(result)
}
