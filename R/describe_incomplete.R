#' Incomplete Entities Description
#'
#' This function provides a descriptive table of entities with incomplete observations (missing values).
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 1 or 2 specifying the names of the
#'        entity and (optionally) time variables. The first element is the entity
#'        variable; if a second element is provided, it is used as the time variable.
#'        Not required if data has panel attributes.
#' @param detail A logical flag indicating whether to include detailed missing counts for each variable.
#'        Default = FALSE.
#'
#' @return A data.frame with incomplete entities description or a character message, class `"panel_description"`.
#'
#' @details
#' When incomplete entities exist, returns a data.frame with:
#' \describe{
#'   \item{\code{[entity]}}{The entity identifier (name matches input entity variable)}
#'   \item{\code{na_count}}{Total number of missing observations for the entity}
#'   \item{\code{variables}}{Number of variables with at least one missing value for that entity}
#' }
#'
#' When `detail = TRUE`, additional columns are included:
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
#' Before analysis, rows with missing values (`NA`) in the entity or (if provided)
#' time variables are removed. Messages indicate how many rows were excluded.
#' The excluded rows are stored in `details$excluded_rows` for further inspection.
#'
#' If no entities have incomplete data, returns the character message:
#' "There are no incomplete entities in the data."
#'
#' If a time variable is supplied (either via `index` or from panel metadata),
#' the function checks for duplicate entity-time combinations. If duplicates are found,
#' they are stored in `details$entity_time_duplicates`. A message is printed only when
#' the identifiers were explicitly provided (i.e., not taken from panel attributes).
#'
#' The returned data.frame (if any) has class `"panel_description"` and attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing additional information:
#'         `count_entities_total`, `count_entities_incomplete`, `entities_incomplete`,
#'         `excluded_rows` (if any), and (if time was supplied and duplicates exist)
#'         `entity_time_duplicates`.}
#' }
#'
#' @seealso
#' [summarize_missing()], [describe_patterns()]
#'
#' @examples
#' data(production)
#' describe_incomplete(production, index = "firm")
#' describe_incomplete(production, index = c("firm", "year"), detail = TRUE)
#'
#' @export
describe_incomplete <- function(
  data,
  index = NULL,
  detail = FALSE
) {
  # --- Initialisation: entity and time from index or metadata ---
  user_index <- index
  entity_time_from_metadata <- FALSE

  if (inherits(data, "panel_data")) {
    metadata <- attr(data, "metadata")
    if (is.null(metadata) || is.null(metadata$entity)) {
      stop("Object has class 'panel_data' but missing 'entity' in metadata.")
    }
    time_meta <- if (!is.null(metadata$time)) metadata$time else NULL

    if (is.null(index)) {
      entity_var <- metadata$entity
      time_var <- time_meta
    } else {
      if (length(index) == 1) {
        entity_var <- index[1]
        time_var <- NULL
      } else if (length(index) == 2) {
        entity_var <- index[1]
        time_var <- index[2]
      } else {
        stop("'index' must be a character vector of length 1 or 2")
      }
    }
    entity_from_metadata <- is.null(user_index) && !is.null(metadata$entity)
    time_from_metadata <- is.null(user_index) &&
      !is.null(time_meta) &&
      (length(user_index) < 2 || is.null(user_index[2]))
    entity_time_from_metadata <- entity_from_metadata &&
      (is.null(time_var) || time_from_metadata)
  } else {
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame")
    }
    if (is.null(index)) {
      stop("For regular data.frames, 'index' must be provided")
    }
    if (length(index) == 1) {
      entity_var <- index[1]
      time_var <- NULL
    } else if (length(index) == 2) {
      entity_var <- index[1]
      time_var <- index[2]
    } else {
      stop("'index' must be a character vector of length 1 or 2")
    }
  }

  # --- Validate existence ---
  if (!entity_var %in% names(data)) {
    stop('entity variable "', entity_var, '" not found in data')
  }
  if (!is.null(time_var) && !time_var %in% names(data)) {
    stop('time variable "', time_var, '" not found in data')
  }
  if (!is.null(time_var) && time_var == entity_var) {
    stop("entity and time variables cannot be the same")
  }

  # --- Remove rows with NA in entity or time (if time provided) ---
  excluded_rows <- NULL
  na_entity <- is.na(data[[entity_var]])
  na_time <- if (!is.null(time_var)) {
    is.na(data[[time_var]])
  } else {
    rep(FALSE, nrow(data))
  }

  if (any(na_entity)) {
    message(
      "Missing values in entity variable '",
      entity_var,
      "' found. Excluding ",
      sum(na_entity),
      " rows."
    )
  }
  if (!is.null(time_var) && any(na_time)) {
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

  # --- Duplicate check (if time provided) ---
  dup_combinations <- NULL
  if (!is.null(time_var)) {
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
  }

  # --- Validate detail ---
  if (!is.logical(detail) || length(detail) != 1) {
    stop("'detail' must be a single logical value, not ", class(detail)[1])
  }

  # Check if entity variable has observations
  if (length(data[[entity_var]]) == 0) {
    stop("entity variable has no observations")
  }

  # If time provided, warn if panel unbalanced? (optional, but kept from original)
  if (!is.null(time_var)) {
    time_counts <- table(data[[entity_var]], data[[time_var]])
    if (!all(time_counts == 1)) {
      warning("The panel is unbalanced.")
    }
  }

  # Get unique entities preserving original type and order
  unique_entities <- unique(data[[entity_var]])

  # Variables to analyze: all except entity and (if present) time
  exclude <- entity_var
  if (!is.null(time_var)) {
    exclude <- c(exclude, time_var)
  }
  vars <- setdiff(names(data), exclude)

  if (length(vars) == 0) {
    stop("no variables to analyze (only entity and time variables found)")
  }

  # Initialize result data frame
  result <- data.frame(
    entity = unique_entities,
    na_count = 0L,
    variables = 0L,
    stringsAsFactors = FALSE
  )
  names(result)[1] <- entity_var

  if (detail) {
    for (v in vars) {
      result[[v]] <- 0L
    }
  }

  # Compute missing statistics per entity
  for (i in seq_along(unique_entities)) {
    ent <- unique_entities[i]
    idx <- data[[entity_var]] == ent
    ent_data <- data[idx, vars, drop = FALSE]

    vars_with_na <- sum(vapply(ent_data, function(x) any(is.na(x)), logical(1)))
    total_na <- sum(vapply(ent_data, function(x) sum(is.na(x)), integer(1)))

    result$variables[i] <- vars_with_na
    result$na_count[i] <- total_na

    if (detail) {
      for (v in vars) {
        result[[v]][i] <- sum(is.na(ent_data[[v]]))
      }
    }
  }

  # Keep only incomplete entities
  incomplete <- result[result$variables > 0, ]
  incomplete_ids <- result[[entity_var]][result$variables > 0]

  if (nrow(incomplete) == 0) {
    return("There are no incomplete entities in the data.")
  }

  # Sort by variables (desc) then na_count (desc)
  incomplete <- incomplete[order(-incomplete$variables, -incomplete$na_count), ]
  rownames(incomplete) <- NULL

  # Build metadata and details
  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    entity = entity_var,
    time = time_var,
    detail = detail
  )
  details <- list(
    count_entities_total = length(unique_entities),
    count_entities_incomplete = nrow(incomplete),
    entities_incomplete = incomplete_ids
  )
  if (!is.null(excluded_rows)) {
    details$excluded_rows <- excluded_rows
  }
  if (!is.null(dup_combinations)) {
    details$entity_time_duplicates <- dup_combinations
  }

  attr(incomplete, "metadata") <- metadata
  attr(incomplete, "details") <- details
  class(incomplete) <- c("panel_description", "data.frame")

  return(incomplete)
}
