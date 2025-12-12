#' Explore transition probabilities for panel data
#'
#' This function replicates Stata's xttrans command, calculating transition probabilities
#' between states of a categorical variable across time periods for panel data.
#'
#' @param data A data frame containing panel data
#' @param variable Character string specifying the factor variable to analyze transitions for
#' @param group Character string specifying the entity/group variable (e.g., individual ID)
#' @param time Character string specifying the time variable
#' @param format Character string specifying the output format: "wide (default)" or "long"
#'
#' @return A data frame in either wide or long format containing transition probabilities
#'
#' @seealso [decompose_variation()], [explore_participation()]
#'
#' @examples
#' data(production)
#'
#' # Analyze transitions in wide format (default)
#' describe_transition(production, variable = "status", group = "firm", time = "year")
#'
#' # Analyze transitions in long format
#' describe_transition(production, variable = "status", group = "firm", time = "year", format = "long")
#'
#' @export
describe_transition <- function(
  data,
  variable,
  group = NULL,
  time = NULL,
  format = "wide"
) {
  # Input validation
  if (!is.character(variable) || length(variable) != 1) {
    stop("'variable' must be a single character string")
  }

  if (!variable %in% names(data)) {
    stop("Variable '", variable, "' not found in the data")
  }

  # Validate format argument
  if (!format %in% c("long", "wide")) {
    stop("format must be either 'long' or 'wide'")
  }

  # Validate group and time for data frames
  if (is.null(group) || is.null(time)) {
    stop("Both 'group' and 'time' must be specified")
  }

  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string")
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string")
  }

  if (!group %in% names(data)) {
    stop("Group variable '", group, "' not found in the data")
  }

  if (!time %in% names(data)) {
    stop("Time variable '", time, "' not found in the data")
  }

  # Convert to data frame and ensure proper ordering
  df <- as.data.frame(data)

  # Check if variable is factor and convert if necessary
  if (!is.factor(df[[variable]])) {
    warning("Variable '", variable, "' is not a factor. Converting to factor.")
    df[[variable]] <- factor(df[[variable]])
  }

  # Check if factor has at least 2 levels
  if (length(levels(df[[variable]])) < 2) {
    stop(
      "Variable '",
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
      "No transitions found. Check if there are multiple time periods per group."
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
