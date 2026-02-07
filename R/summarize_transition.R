#' Transition Probabilities Summary
#'
#' This function calculates and summarizes transition probabilities between states of a categorical
#' variable across time periods for panel data.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param selection A character string specifying the factor variable to analyze transitions for.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'              Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'             Not required if data has panel attributes.
#' @param format A character string specifying the output format: "wide" or "long". Default = "wide".
#' @param digits An integer indicating the number of decimal places to round probabilities. Default = 3.
#'
#' @return A data.frame containing transition probability summary.
#'
#' @details
#' The output format depends on the `format` parameter:
#'
#' \strong{When `format = "wide"` (default):}
#' Returns a transition matrix as a data.frame with:
#' \describe{
#'   \item{\code{from_to}}{The "from" state labels (combining from and to indicators)}
#'   \item{\code{[state1]}}{Column with transition probabilities from each state to state1}
#'   \item{\code{[state2]}}{Column with transition probabilities from each state to state2}
#'   \item{...}{Additional columns for each possible "to" state}
#' }
#' Rows represent the "from" states, columns represent the "to" states.
#'
#' \strong{When `format = "long"`:}
#' Returns a data.frame with:
#' \describe{
#'   \item{\code{from}}{The originating state}
#'   \item{\code{to}}{The destination state}
#'   \item{\code{n}}{Number of transitions observed from `from` to `to`}
#'   \item{\code{prob}}{Transition probability (rounded to specified digits)}
#' }
#'
#' Includes all possible state combinations, even those with zero transitions.
#'
#' The data.frame has additional attributes:
#' \describe{
#'   \item{\code{panel_group}}{The grouping variable name}
#'   \item{\code{panel_time}}{The time variable name}
#'   \item{\code{panel_variable}}{The analyzed categorical variable name}
#'   \item{\code{panel_format}}{Output format ("wide" or "long")}
#'   \item{\code{panel_digits}}{Number of decimal places used for rounding}
#' }
#'
#' @references
#' For Stata users: This corresponds to the `xttrans` command.
#'
#' @seealso
#' [summarize_data()], [summarize_panel()], [describe_participation()]
#'
#' @examples
#' data(production)
#'
#' # Analyze transitions in wide format (default)
#' summarize_transition(production, selection = "industry", group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' summarize_transition(panel_data, selection = "industry")
#'
#' # Analyze transitions in long format
#' summarize_transition(production, selection = "industry", group = "firm", time = "year", format = "long")
#'
#' # Customize rounding
#' summarize_transition(production, selection = "industry", group = "firm", time = "year", digits = 4)
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
  # Check if data has panel attributes
  has_panel_attrs <- !is.null(attr(data, "panel_group")) &&
    !is.null(attr(data, "panel_time"))

  if (has_panel_attrs) {
    # Extract group and time from attributes
    group <- attr(data, "panel_group")
    time <- attr(data, "panel_time")
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

  if (!is.numeric(digits) || length(digits) != 1) {
    stop("'digits' must be a single numeric value, not ", class(digits)[1])
  }

  # Validate format argument
  if (!format %in% c("long", "wide")) {
    stop("format must be either 'long' or 'wide'")
  }

  # Validate digits argument
  if (
    !is.na(digits) &&
      (!is.numeric(digits) || digits < 0 || digits != round(digits))
  ) {
    stop("'digits' must be a non-negative integer or NA for no rounding")
  }

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
  names(count_df) <- c("from", "to", "n")

  # Remove zero counts
  count_df <- count_df[count_df$n > 0, ]

  # Calculate probabilities by from-state
  from_totals <- tapply(count_df$n, count_df$from, sum, na.rm = TRUE)

  # Create long format result
  long_result <- count_df
  long_result$prob <- long_result$n /
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
  long_result$n[is.na(long_result$n)] <- 0
  long_result$prob[is.na(long_result$prob)] <- 0

  # Round probabilities to specified digits
  long_result$prob <- round(long_result$prob, digits = digits)

  # Order by from and to
  long_result <- long_result[order(long_result$from, long_result$to), ]
  rownames(long_result) <- NULL

  # Create wide format (transition matrix)
  wide_matrix <- matrix(
    long_result$prob,
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

  # Add standardized attributes
  attr(result_df, "panel_group") <- group
  attr(result_df, "panel_time") <- time
  attr(result_df, "panel_variable") <- selection
  attr(result_df, "panel_format") <- format
  attr(result_df, "panel_digits") <- digits

  return(result_df)
}
