#' Panel Data Variation Decomposition
#'
#' This function calculates descriptive statistics for panel data and decomposes
#' variance into between and within components.
#'
#' @param data A data.frame containing panel data.
#' @param selection A character vector specifying which numeric variables to analyze.
#'   If not specified, all numeric variables in the data.frame will be used.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'   Required for variance decomposition.
#' @param time A character string specifying the name of the time variable in panel data.
#'   Optional but recommended for panel data validation. If not specified, only group structure
#'   is used for variance decomposition.
#' @param detailed A logical flag indicating whether to return detailed Stata-like output.
#'   Default = TRUE.
#' @param digits An integer indicating the number of decimal places to round statistics.
#'   Default = 3.
#'
#' @return If detailed = TRUE, returns a data.frame with the following columns for each variable:
#'   \item{variable}{The name of the variable}
#'   \item{decomposition}{Type of decomposition: overall, between, or within}
#'   \item{mean}{Mean value (only for overall decomposition)}
#'   \item{sd}{Standard deviation}
#'   \item{min}{Minimum value}
#'   \item{max}{Maximum value}
#'   \item{n}{Number of observations or groups}
#'
#'   If detailed = FALSE, returns a simplified data.frame with columns:
#'   \item{variable}{The name of the variable}
#'   \item{mean}{Overall mean}
#'   \item{sd_overall}{Overall standard deviation}
#'   \item{sd_between}{Between-group standard deviation}
#'   \item{sd_within}{Within-group standard deviation}
#'
#'   The result includes attributes with metadata about the panel structure.
#'
#' @references
#' For Stata users: This corresponds to the `xtsum` command.
#'
#' @seealso
#' [describe()], [plot_heterogeneity()], [explore_participation()], [describe_transition()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage with statistics for all numeric variables
#' decompose_variation(production, group = "firm")
#'
#' # With time variable for panel validation
#' decompose_variation(production, group = "firm", time = "year")
#'
#' # Simplified output
#' decompose_variation(production, group = "firm", time = "year", detailed = FALSE)
#'
#' # Show statistics for a single variable
#' decompose_variation(production, selection = "sales", group = "firm", time = "year")
#'
#' # Show statistics for multiple variables
#' decompose_variation(production, selection = c("capital", "labor"),
#'                     group = "firm", time = "year")
#'
#' # Show statistics with two digits rounding
#' decompose_variation(production, group = "firm", time = "year", digits = 2)
#'
#' # Show statistics with no rounding
#' decompose_variation(production, group = "firm", time = "year", digits = 999999)
#'
#' @export
decompose_variation <- function(
  data,
  selection = NULL,
  group,
  time = NULL,
  detailed = TRUE,
  digits = 3
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
  }

  if (!is.null(selection) && !is.character(selection)) {
    stop(
      "'selection' must be a character vector or NULL, not ",
      class(selection)[1]
    )
  }

  # Group is required
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string")
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  # Time is optional but validated if provided
  if (!is.null(time)) {
    if (!is.character(time) || length(time) != 1) {
      stop("'time' must be a single character string or NULL")
    }

    if (!time %in% names(data)) {
      stop('time variable "', time, '" not found in data')
    }

    if (time == group) {
      stop("'time' and 'group' cannot be the same variable")
    }
  }

  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("'detailed' must be a single logical value, not ", class(detailed)[1])
  }

  if (!is.numeric(digits) || length(digits) != 1) {
    stop("'digits' must be a single numeric value, not ", class(digits)[1])
  }

  data_df <- .check_and_convert_data_robust(data, arg_name = "data")

  # Validate digits parameter
  if (
    !is.na(digits) &&
      (!is.numeric(digits) || digits < 0 || digits != round(digits))
  ) {
    stop("'digits' must be a non-negative integer or NA for no rounding")
  }

  # Validate group parameter
  if (group == "" || length(group) == 0) {
    stop("'group' must be a non-empty character string")
  }

  if (!group %in% names(data_df)) {
    stop(
      "variable '",
      group,
      "' not found in data. Available variables: ",
      paste(names(data_df), collapse = ", ")
    )
  }

  # Panel structure validation and diagnostics
  panel_info <- list(
    group_var = group,
    time_var = if (!is.null(time)) time else NA_character_,
    n_groups = length(unique(data_df[[group]])),
    is_balanced = NA,
    has_duplicates = FALSE,
    time_class = if (!is.null(time)) class(data_df[[time]]) else NA_character_
  )

  if (!is.null(time)) {
    # Check for duplicate group-time combinations
    group_vector <- data_df[[group]]
    time_vector <- data_df[[time]]
    combos <- paste(group_vector, time_vector, sep = "|")

    if (any(duplicated(combos))) {
      n_duplicates <- sum(duplicated(combos))
      panel_info$has_duplicates <- TRUE
      warning(
        "Found ",
        n_duplicates,
        " duplicate group-time combinations. ",
        "Panel data should have at most one observation per group-time pair."
      )
    }

    # Check for balanced panel (same time points for all groups)
    time_by_group <- split(time_vector, group_vector)
    unique_time_sets <- lapply(time_by_group, unique)
    time_set_lengths <- sapply(unique_time_sets, length)

    if (length(unique(time_set_lengths)) == 1) {
      # All groups have same number of time points
      all_time_sets <- unique(unique_time_sets)
      if (length(all_time_sets) == 1) {
        panel_info$is_balanced <- TRUE
      } else {
        panel_info$is_balanced <- FALSE
        warning(
          "Panel appears unbalanced: groups have different time points ",
          "even though they have the same number of observations."
        )
      }
    } else {
      panel_info$is_balanced <- FALSE
      warning(
        "Panel is unbalanced: groups have different numbers of time points (",
        paste(range(time_set_lengths), collapse = "-"),
        " observations per group)."
      )
    }

    # Check time ordering if it's numeric or Date-like
    if (
      is.numeric(time_vector) ||
        inherits(time_vector, "Date") ||
        inherits(time_vector, "POSIXt")
    ) {
      # Check for irregular time intervals within groups
      irregular_groups <- sapply(time_by_group, function(times) {
        if (length(times) > 1) {
          sorted_times <- sort(unique(times))
          intervals <- diff(sorted_times)
          return(length(unique(intervals)) > 1)
        }
        return(FALSE)
      })

      if (any(irregular_groups)) {
        n_irregular <- sum(irregular_groups)
        panel_info$irregular_groups <- n_irregular
        warning(
          "Irregular time intervals detected for ",
          n_irregular,
          " group(s)."
        )
      }
    }

    # Store number of unique time periods
    panel_info$n_periods <- length(unique(time_vector))
  } else {
    message(
      "Note: No time variable specified. Panel time structure not validated."
    )
  }

  # If selection is not specified, use all numeric variables
  if (is.null(selection)) {
    numeric_vars <- vapply(data_df, is.numeric, FUN.VALUE = logical(1))
    selection <- names(data_df)[numeric_vars]

    # Remove the group and time variables from selection if they are numeric
    vars_to_remove <- c(group)
    if (!is.null(time)) {
      vars_to_remove <- c(vars_to_remove, time)
    }
    selection <- selection[!selection %in% vars_to_remove]

    if (length(selection) == 0) {
      stop(
        "no numeric variables found in the dataset (excluding group and time variables)"
      )
    }

    message(
      "Analyzing all numeric variables: ",
      paste(selection, collapse = ", ")
    )
  }

  # Validate selection
  missing_vars <- selection[!selection %in% names(data_df)]
  if (length(missing_vars) > 0) {
    stop(
      "the following variables were not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Check if specified columns are numeric
  non_numeric_vars <- selection[
    !vapply(data_df[selection], is.numeric, FUN.VALUE = logical(1))
  ]
  if (length(non_numeric_vars) > 0) {
    stop(
      "the following variables are not numeric: ",
      paste(non_numeric_vars, collapse = ", ")
    )
  }

  # Check group variable
  group_vector <- data_df[[group]]
  if (length(group_vector) == 0) {
    stop("group variable '", group, "' has zero length")
  }

  n_groups <- length(unique(group_vector))
  if (n_groups > 10000) {
    warning(
      "Large number of groups (",
      n_groups,
      "). This may impact performance."
    )
  }

  # Helper function to calculate panel statistics for one variable
  decompose_variation_1 <- function(
    data,
    varname,
    group,
    time,
    detailed_output,
    digits_val
  ) {
    # Remove rows with NA in the variable, group, or time (if specified)
    if (!is.null(time)) {
      complete_cases <- complete.cases(
        data[[varname]],
        data[[group]],
        data[[time]]
      )
    } else {
      complete_cases <- complete.cases(data[[varname]], data[[group]])
    }
    df <- data[complete_cases, , drop = FALSE]

    if (nrow(df) == 0) {
      if (detailed_output) {
        return(data.frame(
          variable = character(),
          decomposition = character(),
          mean = numeric(),
          sd = numeric(),
          min = numeric(),
          max = numeric(),
          n = numeric(),
          stringsAsFactors = FALSE
        ))
      } else {
        return(data.frame(
          variable = varname,
          mean = NA_real_,
          sd_overall = NA_real_,
          sd_between = NA_real_,
          sd_within = NA_real_
        ))
      }
    }

    # Convert group to character for consistent handling
    group_vec <- as.character(df[[group]])
    x <- df[[varname]]

    # Calculate overall statistics
    overall_mean <- mean(x, na.rm = TRUE)
    overall_sd <- sd(x, na.rm = TRUE)
    min_val <- min(x, na.rm = TRUE)
    max_val <- max(x, na.rm = TRUE)
    n_obs <- length(x)

    # Calculate between variance (variation in group means)
    group_means <- tapply(x, group_vec, mean, na.rm = TRUE)
    between_sd <- sd(group_means, na.rm = TRUE)
    between_min <- min(group_means, na.rm = TRUE)
    between_max <- max(group_means, na.rm = TRUE)
    n_groups_var <- length(group_means)

    # Calculate within variance (variation around group means)
    group_means_expanded <- group_means[match(group_vec, names(group_means))]
    within_sd <- sd(x - group_means_expanded, na.rm = TRUE)
    within_min <- min(x - group_means_expanded, na.rm = TRUE)
    within_max <- max(x - group_means_expanded, na.rm = TRUE)

    # Calculate average observations per group
    obs_per_group <- table(group_vec)
    avg_obs_per_group <- mean(obs_per_group, na.rm = TRUE)

    # Apply rounding if digits is specified
    round_if_needed <- function(value) {
      if (!is.na(digits_val) && is.numeric(value)) {
        round(value, digits_val)
      } else {
        value
      }
    }

    overall_mean <- round_if_needed(overall_mean)
    overall_sd <- round_if_needed(overall_sd)
    min_val <- round_if_needed(min_val)
    max_val <- round_if_needed(max_val)
    between_sd <- round_if_needed(between_sd)
    between_min <- round_if_needed(between_min)
    between_max <- round_if_needed(between_max)
    within_sd <- round_if_needed(within_sd)
    within_min <- round_if_needed(within_min)
    within_max <- round_if_needed(within_max)
    avg_obs_per_group <- round_if_needed(avg_obs_per_group)

    if (detailed_output) {
      # Create Stata-like output with overall, between, and within rows
      result <- data.frame(
        variable = c(varname, varname, varname),
        decomposition = c("overall", "between", "within"),
        mean = c(overall_mean, NA, NA),
        sd = c(overall_sd, between_sd, within_sd),
        min = c(min_val, between_min, within_min),
        max = c(max_val, between_max, within_max),
        n = c(n_obs, n_groups_var, avg_obs_per_group),
        stringsAsFactors = FALSE
      )
    } else {
      # Simplified output with one row per variable
      result <- data.frame(
        variable = varname,
        mean = overall_mean,
        sd_overall = overall_sd,
        sd_between = between_sd,
        sd_within = within_sd
      )
    }

    return(result)
  }

  # Calculate statistics for each variable
  results <- lapply(selection, function(varname) {
    decompose_variation_1(data_df, varname, group, time, detailed, digits)
  })

  # Combine all results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  # Add panel structure information as attributes
  attr(result_df, "panel_info") <- panel_info
  attr(result_df, "detailed") <- detailed
  attr(result_df, "digits") <- digits

  # Add panel summary message
  if (!is.null(time)) {
    panel_summary <- paste0(
      "Panel: ",
      panel_info$n_groups,
      " groups, ",
      panel_info$n_periods,
      " time periods, ",
      ifelse(panel_info$is_balanced, "balanced", "unbalanced")
    )
    if (panel_info$has_duplicates) {
      panel_summary <- paste0(panel_summary, ", with duplicates")
    }
    attr(result_df, "panel_summary") <- panel_summary
  }

  return(result_df)
}
