#' Panel Data Dimensions Description
#'
#' This function calculates number of rows, entities/groups, and time periods for panel data.
#' The function uses different ways to define the completeness of each row, entity/group, and period.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable in panel data.
#'        Not required if data has panel attributes.
#'
#' @return A data.frame with panel dimension counts.
#'
#' @details
#' This function provides panel data structure information across three presence
#' definitions:
#' \itemize{
#'   \item{\bold{nominal:}}{ Entity/time is present if it has a row in the data (even with only panel ID variables)}
#'   \item{\bold{observed:}}{
#'     \itemize{
#'       \item{For rows: Row has at least one non-NA substantive variable}
#'       \item{For entities: Entity has at least one non-NA substantive variable at \bold{each} time period}
#'       \item{For periods: Period has at least one non-NA substantive variable for \bold{all} entities}
#'     }
#'   }
#'   \item{\bold{complete:}}{
#'     \itemize{
#'       \item{For rows: Row has no NA values in all substantive variables}
#'       \item{For entities: Entity has no NA values in all substantive variables at \bold{each} time period}
#'       \item{For periods: Period has no NA values in all substantive variables for \bold{all} entities}
#'     }
#'   }
#' }
#'
#' The returned data.frame has 4 columns:
#' \describe{
#'   \item{\code{dimension}}{Dimension name: "rows", "entities", or "periods"}
#'   \item{\code{nominal}}{Count based on presence of any row (even with only panel ID variables)}
#'   \item{\code{observed}}{Count based on presence of at least one non-NA substantive variable}
#'   \item{\code{complete}}{Count based on presence of no NA values in all substantive variables}
#' }
#'
#' The data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing additional information: `entity_values`, `period_values`,
#'         `total_rows`, `substantive_vars`.}
#' }
#'
#' @examples
#' data(production)
#'
#' # Method 1: With regular data.frame
#' panel_desc <- describe_dimensions(production, group = "firm", time = "year")
#' print(panel_desc)
#'
#' # Method 2: With data.frame with panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' panel_desc <- describe_dimensions(panel_data)
#' print(panel_desc)
#'
#' @seealso
#' [check_panel()], [describe_balance()], [describe_periods()], [set_panel()]
#'
#' @export
describe_dimensions <- function(data, group = NULL, time = NULL) {
  # Check for panel_data class and extract info from metadata
  if (inherits(data, "panel_data")) {
    metadata <- attr(data, "metadata")
    if (
      is.null(metadata) || is.null(metadata$group) || is.null(metadata$time)
    ) {
      stop(
        "Object has class 'panel_data' but missing or incomplete 'metadata' attribute."
      )
    }
    group <- metadata$group
    time <- metadata$time
  } else {
    # Handle regular data.frame
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame, not ", class(data)[1])
    }

    if (is.null(group) || is.null(time)) {
      stop(
        "For regular data.frames, both 'group' and 'time' arguments must be provided"
      )
    }
  }

  # Common validation for both cases
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string")
  }

  if (!group %in% names(data)) {
    stop('group variable "', group, '" not found in data')
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string")
  }

  if (!time %in% names(data)) {
    stop('time variable "', time, '" not found in data')
  }

  if (time == group) {
    stop("'time' and 'group' cannot be the same variable")
  }

  # Get substantive variables (all except group and time)
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Get unique values
  entity_values <- sort(unique(data[[group]]))
  period_values <- sort(unique(data[[time]]))

  # Sort time periods if they appear numeric
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", period_values))) {
    period_values <- as.character(sort(as.numeric(period_values)))
  } else {
    period_values <- sort(period_values)
  }

  # Convert to character for consistent handling
  group_vec <- as.character(data[[group]])
  time_vec <- as.character(data[[time]])

  # Create presence matrices for different criteria
  # Matrix dimensions: entities × periods
  observed_matrix <- matrix(
    FALSE,
    nrow = length(entity_values),
    ncol = length(period_values),
    dimnames = list(entity_values, period_values)
  )

  complete_matrix <- matrix(
    FALSE,
    nrow = length(entity_values),
    ncol = length(period_values),
    dimnames = list(entity_values, period_values)
  )

  # Fill matrices based on data completeness
  for (i in seq_len(nrow(data))) {
    row_entity <- as.character(group_vec[i])
    row_time <- as.character(time_vec[i])

    row_idx <- which(entity_values == row_entity)
    col_idx <- which(period_values == row_time)

    # Check observed criteria (at least one non-NA)
    if (any(!is.na(data[i, substantive_vars]))) {
      observed_matrix[row_idx, col_idx] <- TRUE
    }

    # Check complete criteria (all non-NA)
    if (all(!is.na(data[i, substantive_vars]))) {
      complete_matrix[row_idx, col_idx] <- TRUE
    }
  }

  # Calculate counts for each presence type

  # 1. ROWS
  rows_nominal <- nrow(data)
  rows_observed <- sum(apply(data[substantive_vars], 1, function(x) {
    any(!is.na(x))
  }))
  rows_complete <- sum(apply(data[substantive_vars], 1, function(x) {
    all(!is.na(x))
  }))

  # 2. ENTITIES
  entities_nominal <- length(entity_values)

  # For observed: entity must have observed data in ALL periods where it exists
  # An entity "exists" in a period if it has a row (nominal presence)
  entities_observed <- 0
  entities_complete <- 0

  for (i in seq_along(entity_values)) {
    # Get periods where this entity has any row
    entity_rows <- group_vec == entity_values[i]
    periods_for_entity <- unique(time_vec[entity_rows])

    if (length(periods_for_entity) > 0) {
      # Check observed: for all periods this entity appears, it must have observed data
      observed_in_all_periods <- all(
        sapply(periods_for_entity, function(p) {
          col_idx <- which(period_values == p)
          observed_matrix[i, col_idx]
        })
      )
      if (observed_in_all_periods) {
        entities_observed <- entities_observed + 1
      }

      # Check complete: for all periods this entity appears, it must have complete data
      complete_in_all_periods <- all(
        sapply(periods_for_entity, function(p) {
          col_idx <- which(period_values == p)
          complete_matrix[i, col_idx]
        })
      )
      if (complete_in_all_periods) {
        entities_complete <- entities_complete + 1
      }
    }
  }

  # 3. PERIODS
  periods_nominal <- length(period_values)

  # For observed: period must have observed data for ALL entities that exist in this period
  periods_observed <- 0
  periods_complete <- 0

  for (j in seq_along(period_values)) {
    # Get entities that exist in this period (have any row)
    period_rows <- time_vec == period_values[j]
    entities_in_period <- unique(group_vec[period_rows])

    if (length(entities_in_period) > 0) {
      # Check observed: for all entities in this period, they must have observed data
      observed_for_all_entities <- all(
        sapply(entities_in_period, function(e) {
          row_idx <- which(entity_values == e)
          observed_matrix[row_idx, j]
        })
      )
      if (observed_for_all_entities) {
        periods_observed <- periods_observed + 1
      }

      # Check complete: for all entities in this period, they must have complete data
      complete_for_all_entities <- all(
        sapply(entities_in_period, function(e) {
          row_idx <- which(entity_values == e)
          complete_matrix[row_idx, j]
        })
      )
      if (complete_for_all_entities) {
        periods_complete <- periods_complete + 1
      }
    }
  }

  # Create result data.frame
  result <- data.frame(
    dimension = c("rows", "entities", "periods"),
    nominal = c(rows_nominal, entities_nominal, periods_nominal),
    observed = c(rows_observed, entities_observed, periods_observed),
    complete = c(rows_complete, entities_complete, periods_complete),
    stringsAsFactors = FALSE
  )

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time
  )

  # Build details list
  details <- list(
    entity_values = entity_values,
    period_values = period_values,
    total_rows = nrow(data),
    substantive_vars = substantive_vars
  )

  # Set attributes in desired order
  attr(result, "metadata") <- metadata
  attr(result, "details") <- details

  # Set class
  class(result) <- c("panel_description", "data.frame")

  return(result)
}
