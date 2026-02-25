#' Transition Summary
#'
#' Calculates transition counts and shares between states of a categorical (factor) variable
#' across consecutive time periods within groups (e.g., firms, individuals) for panel data.
#'
#' @param data A data.frame containing panel data in a long format.
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
#' Before analysis, rows with missing values (`NA`) in the `group` or `time` variables are removed.
#' Messages indicate how many rows were excluded due to each variable. The excluded rows are stored in
#' `details$excluded_rows` for further inspection.
#'
#' The function checks for duplicate group-time combinations. In a properly structured panel dataset,
#' each entity (group) should have at most one observation per time period. If duplicates are found:
#' \itemize{
#'   \item The distinct duplicate combinations are stored in `details$entity_time_duplicates`.
#'   \item If, for any duplicate combination, the values of the `selection` variable differ,
#'         an error is raised because the state at that time point is ambiguous.
#'   \item If all duplicate combinations have identical `selection` values, the data is
#'         collapsed to one row per group‑time (taking the first occurrence) before
#'         computing transitions. A message is printed (only when the identifiers were
#'         explicitly provided, not taken from `panel_data` attributes) indicating the
#'         number of collapsed duplicate rows.
#' }
#'
#' The shares are **empirical transition proportions** based on observed consecutive
#' time periods. They are not necessarily Markov transition probabilities in the strict
#' sense because the function does not normalize for missing time periods (gaps in the panel).
#'
#' Missing values in the variable of interest (`selection`) are removed before analysis,
#' so transitions from nonmissing to missing (or vice versa) are **excluded**.
#' The variable of interest is coerced to a factor if it is not already one.
#' At least two distinct levels are required in the factor to compute transitions.
#'
#' The returned data.frame has class `"panel_summary"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing additional information: `categories` (vector of levels
#'         of the analyzed variable), `excluded_rows` (if any), and, if duplicates were found,
#'         `entity_time_duplicates`.}
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

  if (time == group) {
    stop("'time' and 'group' cannot be the same variable")
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

  # --- Remove rows with NA in group or time ---
  excluded_rows <- NULL
  na_group <- is.na(df[[group]])
  na_time <- is.na(df[[time]])

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
    excluded_rows <- df[na_group | na_time, , drop = FALSE]
    df <- df[!(na_group | na_time), , drop = FALSE]
    rownames(df) <- NULL
  }
  # ----------------------------------------------------------------

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

  # --- Check for duplicate group-time combinations ---
  dup_combinations <- NULL
  dup_rows <- duplicated(df[c(group, time)]) |
    duplicated(df[c(group, time)], fromLast = TRUE)
  if (any(dup_rows)) {
    dup_combinations <- unique(df[dup_rows, c(group, time), drop = FALSE])
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

    # For each duplicate combination, check if selection values are consistent
    inconsistent <- FALSE
    for (i in seq_len(nrow(dup_combinations))) {
      grp <- dup_combinations[i, group]
      tm <- dup_combinations[i, time]
      rows <- df[[group]] == grp & df[[time]] == tm
      sel_vals <- df[[selection]][rows]
      if (length(unique(sel_vals)) > 1) {
        inconsistent <- TRUE
        break
      }
    }

    if (inconsistent) {
      stop(
        "Duplicate group-time combinations with differing values of '",
        selection,
        "' found. The state at these time points is ambiguous. ",
        "Please resolve duplicates before proceeding."
      )
    } else {
      # All duplicates have identical selection values; collapse to first row per group-time
      df <- df[!duplicated(df[c(group, time)]), ]
      if (!group_time_from_metadata) {
        message(
          "Duplicate rows with identical '",
          selection,
          "' values collapsed."
        )
        messages_printed <- TRUE
      }
    }
  }
  # ----------------------------------------------------

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

  # Order data by group and time
  df <- df[order(df[[group]], df[[time]]), ]

  # Calculate transitions using base R
  transitions <- by(df, df[[group]], function(group_data) {
    if (nrow(group_data) > 1) {
      # Order within group by time (already sorted)
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
    categories = all_levels
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
