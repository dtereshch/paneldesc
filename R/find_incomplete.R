#' Incomplete Entities Identification
#'
#' This function identifies entities with incomplete observations (missing values) in panel data.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#' @param time A character string specifying the name of the time variable (optional, for checking panel balance).
#'
#' @return A vector containing entities with missing values, or a message if no incomplete entities found.
#'
#' @seealso
#' [explore_incomplete()], [describe_participation()], [plot_participation()], [explore_participation()]
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
#' # Create balanced panel data
#' incomplete_entities <- find_incomplete(production, group = "firm")
#' production_balanced <- subset(production, !(firm %in% incomplete_entities))
#'
#' @export
find_incomplete <- function(data, group = NULL, time = NULL) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("find_incomplete: 'data' must be a data.frame, not ", class(data)[1])
  }

  if (is.null(group) || !is.character(group) || length(group) != 1) {
    stop(
      "find_incomplete: 'group' must be a single character string, not ",
      class(group)[1]
    )
  }

  if (!group %in% names(data)) {
    stop('find_incomplete: variable "', group, '" not found in data')
  }

  if (!is.null(time) && (!is.character(time) || length(time) != 1)) {
    stop(
      "find_incomplete: 'time' must be a single character string, not ",
      class(time)[1]
    )
  }

  if (!is.null(time) && !time %in% names(data)) {
    stop('find_incomplete: variable "', time, '" not found in data')
  }

  data <- .check_and_convert_data_robust(data, arg_name = "data")

  if (missing(group) || is.null(group)) {
    stop("find_incomplete: argument 'group' is required")
  }

  # Validate group is a character string
  if (!is.character(group) || length(group) != 1) {
    stop(
      "find_incomplete: 'group' must be a single character string specifying the column name"
    )
  }

  group_name <- group

  # Validate group column exists
  if (!group_name %in% names(data)) {
    stop("find_incomplete: variable '", group_name, "' not found in data")
  }

  group_var <- data[[group_name]]

  # Check if time variable is provided for unbalanced panel check
  check_unbalanced <- FALSE
  if (!is.null(time)) {
    if (!is.character(time) || length(time) != 1) {
      stop(
        "find_incomplete: 'time' must be a single character string specifying the column name"
      )
    }
    if (!time %in% names(data)) {
      stop("find_incomplete: variable '", time, "' not found in data")
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
      "Note: group variable contains missing values. These will be included in the results. (occurred in find_incomplete)"
    )
  }

  # Check for unbalanced panel if time variable is provided
  if (check_unbalanced) {
    # Count unique time periods per group
    time_counts <- tapply(time_var, group_var, function(x) length(unique(x)))

    # Check if all groups have the same number of time periods
    if (length(unique(time_counts)) > 1) {
      warning("Note: the panel is unbalanced. (occurred in find_incomplete)")
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
