#' Identify entities with incomplete observations (missing values)
#'
#' @param data The data frame
#' @param group Entities' identifier (column name as character string)
#' @param time Time identifier (optional, for checking panel balance)
#' @return A vector containing entities with missing values, or a message if no incomplete entities found
#'
#' @seealso [explore_incomplete()], [describe_participation()], [plot_participation()], [explore_participation()]
#'
#' @examples
#' data(production)
#'
#' # Find firms with missing values in any variable
#' find_incomplete(production, group = "firm", time = "year")
#'
#' # Example without time variable (no unbalanced check)
#' find_incomplete(production, group = "firm")
#'
#' @export
find_incomplete <- function(data, group = NULL, time = NULL) {
  # Input validation
  if (missing(data)) {
    stop("Argument 'data' is required")
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (missing(group) || is.null(group)) {
    stop("Argument 'group' is required")
  }

  # Validate group is a character string
  if (!is.character(group) || length(group) != 1) {
    stop(
      "'group' must be a single character string specifying the column name"
    )
  }

  group_name <- group

  # Validate group column exists
  if (!group_name %in% names(data)) {
    stop(sprintf("Group variable '%s' not found in data", group_name))
  }

  group_var <- data[[group_name]]

  # Check if time variable is provided for unbalanced panel check
  check_unbalanced <- FALSE
  if (!is.null(time)) {
    if (!is.character(time) || length(time) != 1) {
      stop(
        "'time' must be a single character string specifying the column name"
      )
    }
    if (!time %in% names(data)) {
      stop(sprintf("Time variable '%s' not found in data", time))
    }
    check_unbalanced <- TRUE
    time_var <- data[[time]]

    # Convert time variable to appropriate type for consistent handling
    if (is.factor(time_var)) {
      time_var <- as.character(time_var)
    }
  }

  # Convert group variable to appropriate type for consistent handling
  if (is.factor(group_var)) {
    group_var <- as.character(group_var)
  }

  # Check if group variable has missing values itself
  if (any(is.na(group_var))) {
    warning(
      "Group variable contains missing values. These will be included in the results."
    )
  }

  # Check for unbalanced panel if time variable is provided
  if (check_unbalanced) {
    # Count unique time periods per group
    time_counts <- tapply(time_var, group_var, function(x) length(unique(x)))

    # Check if all groups have the same number of time periods
    if (length(unique(time_counts)) > 1) {
      warning("The panel is unbalanced.")
    }
  }

  # Identify complete cases by group
  group_unique <- unique(group_var)
  incomplete_entities <- character(0)

  for (entity in group_unique) {
    # Handle NA values in group variable
    if (is.na(entity)) {
      entity_data <- data[is.na(group_var), , drop = FALSE]
    } else {
      entity_data <- data[group_var == entity, , drop = FALSE]
    }

    # Check if any row for this entity has missing values
    # Remove group column from missing value check to avoid self-reference
    entity_data_subset <- entity_data[,
      setdiff(names(entity_data), group_name),
      drop = FALSE
    ]

    if (any(apply(entity_data_subset, 1, function(row) any(is.na(row))))) {
      incomplete_entities <- c(incomplete_entities, entity)
    }
  }

  # Check if no incomplete entities were found
  if (length(incomplete_entities) == 0) {
    return("There are no incomplete groups/entities in the data.")
  }

  # Convert back to original type if possible
  original_type <- class(data[[group_name]])
  if ("numeric" %in% original_type || "integer" %in% original_type) {
    # Handle numeric conversion carefully to avoid warnings
    numeric_entities <- suppressWarnings(as.numeric(incomplete_entities))
    if (!any(is.na(numeric_entities))) {
      incomplete_entities <- numeric_entities
    }
  } else if ("factor" %in% original_type) {
    incomplete_entities <- factor(
      incomplete_entities,
      levels = levels(data[[group_name]])
    )
  }

  return(incomplete_entities)
}
