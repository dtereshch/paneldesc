#' Missing Values Summary for Panel Data
#'
#' This function calculates summary statistics for missing values (NAs) in panel data,
#' providing both overall and detailed period-specific missing value counts.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param selection A character vector specifying which variables to analyze for missing values.
#'        If not specified, all variables in the data.frame will be used.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#' @param detailed A logical flag indicating whether to return detailed period-specific NA counts.
#'        Default = FALSE.
#' @param digits An integer indicating the number of decimal places to round the share column.
#'        Default = 3.
#'
#' @return A data.frame with missing value summary statistics.
#'
#' @details
#' When `detailed = FALSE` (default), returns a data.frame with the following columns:
#' \describe{
#'   \item{\code{variable}}{The name of the analyzed variable}
#'   \item{\code{na_count}}{Total number of missing values (NAs) for the variable}
#'   \item{\code{na_share}}{Proportion of observations that are missing (0 to 1), rounded to specified digits}
#'   \item{\code{entities}}{Number of entities/groups with at least one missing value for this variable}
#'   \item{\code{periods}}{Number of time periods with at least one missing value for this variable}
#' }
#'
#' When `detailed = TRUE`, additional columns are included:
#' \describe{
#'   \item{\code{[period1]}}{Column with NA counts for period1 (name matches the time period value)}
#'   \item{\code{[period2]}}{Column with NA counts for period2 (name matches the time period value)}
#'   \item{...}{Additional columns for each unique time period in the data}
#' }
#'
#' Before analysis, rows with missing values (`NA`) in the `group` or `time` variables are removed.
#' Messages indicate how many rows were excluded due to each variable. The excluded rows are stored in
#' `details$excluded_rows` for further inspection.
#'
#' The function checks for duplicate group-time combinations. In a properly structured panel dataset,
#' each entity (group) should have at most one observation per time period. If duplicates are found,
#' they are stored in `details$entity_time_duplicates`. A message is printed only when the identifiers
#' were explicitly provided (i.e., not taken from `panel_data` attributes).
#'
#' The returned data.frame has class `"panel_summary"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing additional information:
#'         `count_variables_with_na`, `count_variables_without_na`, `count_variables` (total analyzed),
#'         `variables_with_na`, `variables_without_na`, `excluded_rows` (if any), and, if duplicates exist,
#'         `entity_time_duplicates`.}
#' }
#'
#' @seealso
#' [describe_incomplete()], [describe_balance()], [describe_periods()], [summarize_numeric()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage with statistics for all variables
#' summarize_missing(production, group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' summarize_missing(panel_data)
#'
#' # Detailed output with period-specific NA counts
#' summarize_missing(production, group = "firm", time = "year", detailed = TRUE)
#'
#' @export
summarize_missing <- function(
  data,
  selection = NULL,
  group = NULL,
  time = NULL,
  detailed = FALSE,
  digits = 3
) {
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
  if (!is.null(selection) && !is.character(selection)) {
    stop(
      "'selection' must be a character vector or NULL, not ",
      class(selection)[1]
    )
  }

  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string, not ", class(time)[1])
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  if (!time %in% names(data)) {
    stop('variable "', time, '" not found in data')
  }

  if (time == group) {
    stop("'time' and 'group' cannot be the same variable")
  }

  # --- NEW CHECK: ensure selection does not include group or time (if provided) ---
  if (!is.null(selection)) {
    if (group %in% selection) {
      stop("'selection' cannot be the same as 'group' variable ('", group, "')")
    }
    if (time %in% selection) {
      stop("'selection' cannot be the same as 'time' variable ('", time, "')")
    }
  }
  # --------------------------------------------------------------------------------

  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("'detailed' must be a single logical value, not ", class(detailed)[1])
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

  # Harmonized digits validation
  if (!is.numeric(digits) || length(digits) != 1) {
    stop("'digits' must be a single non-negative integer")
  }
  if (digits < 0 || digits != round(digits)) {
    stop("'digits' must be a non-negative integer")
  }
  digits <- as.integer(digits)

  # Helper function for rounding
  round_if_needed <- function(x, digits) {
    if (is.numeric(x) && !all(is.na(x))) {
      round(x, digits)
    } else {
      x
    }
  }

  # Track if any messages were printed
  messages_printed <- FALSE

  # If selection is not specified, use all variables except group and time
  if (is.null(selection)) {
    selection <- setdiff(names(data), c(group, time))

    if (length(selection) == 0) {
      stop("no variables found to analyze (besides group and time variables)")
    }

    message(
      "Analyzing all variable(s): ",
      paste(selection, collapse = ", ")
    )
    messages_printed <- TRUE
  }

  # Validate selection
  missing_vars <- selection[!selection %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(
      "the following variables were not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Remove group and time from selection if included (redundant after check, but safe)
  selection <- setdiff(selection, c(group, time))
  if (length(selection) == 0) {
    stop("no variables to analyze (excluding group and time variables)")
  }

  # Get unique time periods and sort them
  time_values <- as.character(data[[time]])
  unique_periods <- unique(time_values)

  # Sort time periods if they appear numeric
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", unique_periods))) {
    ordered_periods <- as.character(sort(as.numeric(unique_periods)))
  } else {
    ordered_periods <- sort(unique_periods)
  }

  # Get unique groups
  unique_groups <- unique(as.character(data[[group]]))

  # Total counts for attributes
  total_obs <- nrow(data)
  total_entities <- length(unique_groups)
  total_periods <- length(ordered_periods)

  # Initialize result list
  results <- list()

  # Calculate missing value statistics for each variable
  for (var in selection) {
    # Total NA count
    na_count <- sum(is.na(data[[var]]))

    # NA share (proportion) - rounded to specified digits
    na_share <- ifelse(total_obs > 0, na_count / total_obs, 0)
    na_share <- round_if_needed(na_share, digits)

    # Entities with at least one NA
    if (na_count > 0) {
      entity_has_na <- tapply(data[[var]], data[[group]], function(x) {
        any(is.na(x))
      })
      entities_with_na <- sum(entity_has_na, na.rm = TRUE)
    } else {
      entities_with_na <- 0
    }

    # Periods with at least one NA
    if (na_count > 0) {
      period_has_na <- tapply(data[[var]], data[[time]], function(x) {
        any(is.na(x))
      })
      periods_with_na <- sum(period_has_na, na.rm = TRUE)
    } else {
      periods_with_na <- 0
    }

    result_row <- data.frame(
      variable = var,
      na_count = na_count,
      na_share = na_share,
      entities = entities_with_na,
      periods = periods_with_na,
      stringsAsFactors = FALSE
    )

    # Add detailed period-specific NA counts if requested
    if (detailed) {
      for (period in ordered_periods) {
        period_data <- data[time_values == period, var, drop = FALSE]
        period_na_count <- sum(is.na(period_data[[var]]))
        result_row[[period]] <- period_na_count
      }
    }

    results[[var]] <- result_row
  }

  # Combine all results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  # Identify variables with and without missing values
  vars_with_na <- result_df$variable[result_df$na_count > 0]
  vars_without_na <- result_df$variable[result_df$na_count == 0]

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    selection = selection,
    group = group,
    time = time,
    detailed = detailed,
    digits = digits
  )

  # Build details list
  details <- list(
    count_variables_with_na = length(vars_with_na),
    count_variables_without_na = length(vars_without_na),
    count_variables = length(selection),
    variables_with_na = vars_with_na,
    variables_without_na = vars_without_na
  )

  if (!is.null(excluded_rows)) {
    details$excluded_rows <- excluded_rows
  }

  if (!is.null(dup_combinations)) {
    details$entity_time_duplicates <- dup_combinations
  }

  # Set attributes
  attr(result_df, "metadata") <- metadata
  attr(result_df, "details") <- details
  class(result_df) <- c("panel_summary", "data.frame")

  # Add empty line before returning if messages were printed
  if (
    messages_printed || !is.null(excluded_rows) || !is.null(dup_combinations)
  ) {
    cat("\n")
  }

  return(result_df)
}
