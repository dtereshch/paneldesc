#' Transition Summary
#'
#' Calculates transition counts and shares between states of a categorical (factor) variable
#' across consecutive time periods within groups (e.g., firms, individuals) for panel data.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes
#'        (see [set_panel()]).
#' @param selection A character string specifying the factor variable to analyze transitions for.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#' @param format A character string specifying the output format: `"wide"` (default) or `"long"`.
#' @param digits An integer indicating the number of decimal places to round transition shares.
#'        Default = 3.
#'
#' @return A data.frame containing transition summaries.
#'
#' @details
#' The structure depends on `format`:
#'
#' \describe{
#'   \item{`format = "wide"`}{A transition matrix as a data.frame:
#'     \itemize{
#'       \item `from_to`: The originating state (row label).
#'       \item Columns for each destination state, containing the share of transitions from the
#'             row state to the column state (rounded to `digits`).
#'     }
#'   }
#'   \item{`format = "long"`}{A data.frame with columns:
#'     \itemize{
#'       \item `from`: Originating state.
#'       \item `to`: Destination state.
#'       \item `count`: Number of observed transitions from `from` to `to`.
#'       \item `share`: Proportion of transitions from `from` that go to `to` (rounded to `digits`).
#'     }
#'     All possible state combinations are included, even those with zero transitions.
#'   }
#' }
#'
#' @note
#' \itemize{
#'   \item The shares are **empirical transition proportions** based on observed consecutive
#'         time periods. They are not necessarily Markov transition probabilities in the strict
#'         sense because the function does not normalize for missing time periods (gaps in the panel).
#'   \item If there are gaps in the time variable within a group, transitions are only counted
#'         between observations that are consecutive in the sorted data; periods with missing
#'         observations are simply skipped.
#'   \item Missing values in the variable of interest (`selection`) are removed before analysis,
#'         so transitions from nonmissing to missing (or vice versa) are **excluded**.
#'   \item The variable of interest is coerced to a factor if it is not already one.
#'   \item At least two distinct levels are required in the factor to compute transitions.
#' }
#'
#' The returned data.frame has class `"panel_summary"` and the following attributes:
#' \describe{
#'   \item{`panel_info`}{Named character vector with elements `group_var` and `time_var`.}
#'   \item{`details`}{List containing additional information: `variable`, `format`, `digits`.}
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#' }
#'
#' @references
#' For Stata users: This corresponds to the `xttrans` command.
#'
#' @seealso
#' [decompose_factor()], [decompose_numeric()], [describe_patterns()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage (transition matrix)
#' summarize_transition(production,
#'                      selection = "industry",
#'                      group = "firm",
#'                      time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' summarize_transition(panel_data, selection = "industry")
#'
#' # Long format (counts and shares)
#' summarize_transition(production,
#'                      selection = "industry",
#'                      group = "firm",
#'                      time = "year",
#'                      format = "long")
#'
#' # Control rounding precision
#' summarize_transition(production,
#'                      selection = "industry",
#'                      group = "firm",
#'                      time = "year",
#'                      digits = 4)
#'
#' @export
summarize_transition <- function(
  data,
  selection,
  group = NULL,
  time = NULL,
  format = "wide",
  digits = 3
) {
  # Check for panel_data class and extract info
  if (inherits(data, "panel_data")) {
    panel_info <- attr(data, "panel_info")
    if (
      is.null(panel_info) ||
        is.null(panel_info["group_var"]) ||
        is.null(panel_info["time_var"])
    ) {
      stop(
        "Object has class 'panel_data' but missing or incomplete 'panel_info' attribute."
      )
    }
    group <- panel_info["group_var"]
    time <- panel_info["time_var"]
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

  # Check for duplicate variable selection (selection cannot be group or time)
  if (selection == group) {
    stop("'selection' cannot be the same as 'group' variable ('", group, "')")
  }

  if (selection == time) {
    stop("'selection' cannot be the same as 'time' variable ('", time, "')")
  }

  # Common validation
  if (!is.character(selection) || length(selection) != 1) {
    stop(
      "'selection' must be a single character string, not ",
      class(selection)[1]
    )
  }

  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string, not ", class(time)[1])
  }

  if (!selection %in% names(data)) {
    stop('variable "', selection, '" not found in data')
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  if (!time %in% names(data)) {
    stop('variable "', time, '" not found in data')
  }

  if (!is.character(format) || length(format) != 1) {
    stop("'format' must be a single character string, not ", class(format)[1])
  }

  if (!format %in% c("long", "wide")) {
    stop('format must be either "long" or "wide", not "', format, '"')
  }

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

  # Convert to data frame and ensure proper ordering
  df <- as.data.frame(data)

  # Check if variable is factor and convert if necessary
  if (!is.factor(df[[selection]])) {
    message(
      "Converting variable '",
      selection,
      "' to factor. Original class: ",
      class(df[[selection]])[1]
    )
    messages_printed <- TRUE
    df[[selection]] <- factor(df[[selection]])
  }

  # Check if factor has at least 2 levels
  if (length(levels(df[[selection]])) < 2) {
    stop(
      "variable '",
      selection,
      "' must have at least 2 levels to analyze transitions"
    )
  }

  # Convert group and time to character to handle different classes
  df[[group]] <- as.character(df[[group]])
  df[[time]] <- as.character(df[[time]])

  # Order data by group and time
  df <- df[order(df[[group]], df[[time]]), ]

  # Remove rows with NA in the variable of interest
  complete_cases <- !is.na(df[[selection]])
  if (sum(!complete_cases) > 0) {
    message(
      "Removing ",
      sum(!complete_cases),
      " rows with NA values in '",
      selection,
      "'"
    )
    messages_printed <- TRUE
    df <- df[complete_cases, ]
  }

  # Calculate transitions using base R
  transitions <- by(df, df[[group]], function(group_data) {
    if (nrow(group_data) > 1) {
      # Order within group by time
      group_data <- group_data[order(group_data[[time]]), ]
      from_values <- group_data[[selection]][-nrow(group_data)]
      to_values <- group_data[[selection]][-1]
      data.frame(
        from = from_values,
        to = to_values,
        group = group_data[[group]][1],
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  # Combine all transitions
  transition_df <- do.call(rbind, transitions)

  if (is.null(transition_df)) {
    stop(
      "no transitions found. Check if there are multiple time periods per group."
    )
  }

  # Ensure from and to are factors with the same levels
  all_levels <- levels(df[[selection]])
  transition_df$from <- factor(transition_df$from, levels = all_levels)
  transition_df$to <- factor(transition_df$to, levels = all_levels)

  # Calculate counts using table
  count_table <- table(transition_df$from, transition_df$to)
  count_df <- as.data.frame(count_table, stringsAsFactors = FALSE)
  names(count_df) <- c("from", "to", "count")

  # Remove zero counts
  count_df <- count_df[count_df$count > 0, ]

  # Calculate probabilities by from-state
  from_totals <- tapply(count_df$count, count_df$from, sum, na.rm = TRUE)

  # Create long format result
  long_result <- count_df
  long_result$share <- long_result$count /
    from_totals[as.character(long_result$from)]

  # Ensure all factor levels are represented in the output
  complete_grid <- expand.grid(
    from = all_levels,
    to = all_levels,
    stringsAsFactors = FALSE
  )
  long_result <- merge(
    complete_grid,
    long_result,
    by = c("from", "to"),
    all.x = TRUE
  )
  long_result$count[is.na(long_result$count)] <- 0
  long_result$share[is.na(long_result$share)] <- 0

  # Round transition shares to specified digits
  long_result$share <- round_if_needed(long_result$share, digits)

  # Order by from and to
  long_result <- long_result[order(long_result$from, long_result$to), ]
  rownames(long_result) <- NULL

  # Create wide format (transition matrix)
  wide_matrix <- matrix(
    long_result$share,
    nrow = length(all_levels),
    ncol = length(all_levels),
    byrow = FALSE,
    dimnames = list(from = all_levels, to = all_levels)
  )

  wide_result <- as.data.frame(wide_matrix)
  # Add from_to column with state labels
  wide_result <- cbind(from_to = all_levels, wide_result)
  rownames(wide_result) <- NULL

  # Return the requested format
  if (format == "wide") {
    result_df <- wide_result
  } else {
    result_df <- long_result
  }

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    selection = selection,
    group = group,
    time = time,
    format = format,
    digits = digits
  )

  # Build details list
  details <- list(
    variable = selection,
    format = format,
    digits = digits
  )

  # Set attributes in desired order
  attr(result_df, "panel_info") <- c(group_var = group, time_var = time)
  attr(result_df, "details") <- details
  attr(result_df, "metadata") <- metadata

  # Set class
  class(result_df) <- c("panel_summary", "data.frame")

  # Add empty line before returning data.frame if messages were printed
  if (messages_printed) {
    cat("\n")
  }

  return(result_df)
}
