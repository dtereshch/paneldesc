#' Transition Probability Analysis
#'
#' This function calculates transition probabilities between states of a categorical
#' variable across time periods for panel data.
#'
#' @param data A data.frame containing panel data.
#' @param variable A character string specifying the factor variable to analyze transitions for.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#' @param time A character string specifying the name of the time variable.
#' @param format A character string specifying the output format: "wide" or "long". Default = "wide".
#' @param digits An integer indicating the number of decimal places to round probabilities. Default = 3.
#'
#' @return A data.frame in either wide or long format containing transition probabilities.
#'
#' @references
#' For Stata users: This corresponds to the `xttrans` command.
#'
#' @seealso
#' [decompose_variation()], [explore_participation()]
#'
#' @examples
#' data(production)
#'
#' # Analyze transitions in wide format (default)
#' describe_transition(production, variable = "industry", group = "firm", time = "year")
#'
#' # Analyze transitions in long format
#' describe_transition(production, variable = "industry", group = "firm", time = "year", format = "long")
#'
#' # Specify number of decimal places
#' describe_transition(production, variable = "industry", group = "firm", time = "year", digits = 4)
#'
#' @export
describe_transition <- function(
  data,
  variable,
  group = NULL,
  time = NULL,
  format = "wide",
  digits = 3
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
  }

  if (!is.character(variable) || length(variable) != 1) {
    stop(
      "'variable' must be a single character string, not ",
      class(variable)[1]
    )
  }

  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string, not ", class(time)[1])
  }

  if (!variable %in% names(data)) {
    stop('variable "', variable, '" not found in data')
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

  if (!is.numeric(digits) || length(digits) != 1 || digits < 0) {
    stop(
      "'digits' must be a single non-negative integer, not ",
      class(digits)[1]
    )
  }

  data <- .check_and_convert_data_robust(data, arg_name = "data")

  # Validate format argument
  if (!format %in% c("long", "wide")) {
    stop("format must be either 'long' or 'wide'")
  }

  # Validate digits argument
  if (
    !is.numeric(digits) ||
      length(digits) != 1 ||
      digits < 0 ||
      digits != round(digits)
  ) {
    stop("'digits' must be a single non-negative integer")
  }

  # Validate group and time for data frames
  if (is.null(group) || is.null(time)) {
    stop("both 'group' and 'time' must be specified")
  }

  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string")
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string")
  }

  if (!group %in% names(data)) {
    stop("variable '", group, "' not found in data")
  }

  if (!time %in% names(data)) {
    stop("variable '", time, "' not found in data")
  }

  # Convert to data frame and ensure proper ordering
  df <- as.data.frame(data)

  # Check if variable is factor and convert if necessary
  if (!is.factor(df[[variable]])) {
    warning(
      "Variable '",
      variable,
      "' is not a factor. Converting to factor."
    )
    df[[variable]] <- factor(df[[variable]])
  }

  # Check if factor has at least 2 levels
  if (length(levels(df[[variable]])) < 2) {
    stop(
      "variable '",
      variable,
      "' must have at least 2 levels to analyze transitions"
    )
  }

  # Convert group and time to character to handle different classes
  df[[group]] <- as.character(df[[group]])
  df[[time]] <- as.character(df[[time]])

  # Order data by group and time
  df <- df[order(df[[group]], df[[time]]), ]

  # Remove rows with NA in the variable of interest
  complete_cases <- !is.na(df[[variable]])
  if (sum(!complete_cases) > 0) {
    warning(
      "Removing ",
      sum(!complete_cases),
      " rows with NA values in '",
      variable,
      "'"
    )
    df <- df[complete_cases, ]
  }

  # Calculate transitions using base R
  transitions <- by(df, df[[group]], function(group_data) {
    if (nrow(group_data) > 1) {
      # Order within group by time
      group_data <- group_data[order(group_data[[time]]), ]
      from_values <- group_data[[variable]][-nrow(group_data)]
      to_values <- group_data[[variable]][-1]
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
  all_levels <- levels(df[[variable]])
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
  # Add empty heading for first column
  wide_result <- cbind(" " = all_levels, wide_result)
  rownames(wide_result) <- NULL

  # Return the requested format
  if (format == "wide") {
    return(wide_result)
  } else {
    return(long_result)
  }
}
