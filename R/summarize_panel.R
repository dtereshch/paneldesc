#' Panel Data Summary Statistics
#'
#' This function calculates summary statistics for panel data and decomposes
#' variance into between and within components.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param selection A character vector specifying which numeric variables to analyze.
#'   If not specified, all numeric variables in the data.frame will be used.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'              Required for variance summary. Not required if data has panel attributes.
#' @param detailed A logical flag indicating whether to return detailed Stata-like output.
#'   Default = TRUE.
#' @param digits An integer indicating the number of decimal places to round statistics.
#'   Default = 3.
#'
#' @return A data.frame with panel data summary statistics.
#'
#' @details
#' When `detailed = TRUE` (default), returns a data.frame with the following columns:
#' \describe{
#'   \item{\code{variable}}{The name of the analyzed variable}
#'   \item{\code{decomposition}}{Type of decomposition: "overall", "between", or "within"}
#'   \item{\code{mean}}{Mean value (only for "overall" row)}
#'   \item{\code{sd}}{Standard deviation}
#'   \item{\code{min}}{Minimum value}
#'   \item{\code{max}}{Maximum value}
#'   \item{\code{n}}{Number of observations or groups}
#' }
#'
#' When `detailed = FALSE`, returns a simplified data.frame with columns:
#' \describe{
#'   \item{\code{variable}}{The name of the variable}
#'   \item{\code{mean}}{Overall mean}
#'   \item{\code{sd_overall}}{Overall standard deviation}
#'   \item{\code{sd_between}}{Between-group standard deviation}
#'   \item{\code{sd_within}}{Within-group standard deviation}
#' }
#'
#' The data.frame has additional attributes:
#' \describe{
#'   \item{\code{group_var}}{The grouping variable name}
#'   \item{\code{n_groups}}{Number of unique groups}
#'   \item{\code{detailed}}{Logical indicating detailed output}
#'   \item{\code{digits}}{Number of decimal places used for rounding}
#' }
#'
#' @references
#' For Stata users: This corresponds to the `xtsum` command.
#'
#' @seealso
#' [summarize_data()], [plot_heterogeneity()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage with statistics for all numeric variables
#' summarize_panel(production, group = "firm")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' summarize_panel(panel_data)
#'
#' # Simplified output
#' summarize_panel(production, group = "firm", detailed = FALSE)
#'
#' # Show statistics for a single variable
#' summarize_panel(production, selection = "sales", group = "firm")
#'
#' # Show statistics for multiple variables
#' summarize_panel(production, selection = c("capital", "labor"), group = "firm")
#'
#' # Show statistics with two digits rounding
#' summarize_panel(production, group = "firm", digits = 2)
#'
#' # Show statistics with no rounding
#' summarize_panel(production, group = "firm", digits = 999999)
#'
#' @export
summarize_panel <- function(
  data,
  selection = NULL,
  group = NULL,
  detailed = TRUE,
  digits = 3
) {
  # Check if data has panel attributes
  has_panel_attrs <- !is.null(attr(data, "panel_group")) &&
    !is.null(attr(data, "panel_time"))

  if (has_panel_attrs) {
    # Extract group from attributes
    group <- attr(data, "panel_group")
  } else {
    # Handle regular data.frame
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame, not ", class(data)[1])
    }

    if (is.null(group)) {
      stop("For regular data.frames, 'group' argument must be provided")
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
    stop("'group' must be a single character string")
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("'detailed' must be a single logical value, not ", class(detailed)[1])
  }

  if (!is.numeric(digits) || length(digits) != 1) {
    stop("'digits' must be a single numeric value, not ", class(digits)[1])
  }

  data_df <- data # Using original data as .check_and_convert_data_robust is not defined

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

  # If selection is not specified, use all numeric variables
  if (is.null(selection)) {
    numeric_vars <- vapply(data_df, is.numeric, FUN.VALUE = logical(1))
    selection <- names(data_df)[numeric_vars]

    # Remove the group variable from selection if it's numeric
    if (group %in% selection) {
      selection <- selection[selection != group]
    }

    if (length(selection) == 0) {
      stop("no numeric variables found in the dataset")
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
  summarize_panel_1 <- function(
    data,
    varname,
    group,
    detailed_output,
    digits_val
  ) {
    # Remove rows with NA in the variable or group
    complete_cases <- complete.cases(data[[varname]], data[[group]])
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
    # For within: we need to calculate (x - group_mean) + overall_mean
    # This transforms the deviations to be comparable to original scale
    group_means_expanded <- group_means[match(group_vec, names(group_means))]
    deviations <- x - group_means_expanded

    # Transform deviations to the original scale by adding overall mean
    # This is what Stata's xtsum does for within min/max
    within_transformed <- deviations + overall_mean

    within_sd <- sd(deviations, na.rm = TRUE)
    within_min <- min(within_transformed, na.rm = TRUE)
    within_max <- max(within_transformed, na.rm = TRUE)

    # Calculate average observations per group
    obs_per_group <- table(group_vec)
    avg_obs_per_group <- mean(obs_per_group, na.rm = TRUE)

    # Apply rounding if digits is specified
    round_if_needed <- function(value) {
      if (!is.na(digits_val) && is.numeric(value) && !is.na(value)) {
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
    summarize_panel_1(data_df, varname, group, detailed, digits)
  })

  # Combine all results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  # Add data source information as attribute
  attr(result_df, "group_var") <- group
  attr(result_df, "n_groups") <- n_groups
  attr(result_df, "detailed") <- detailed
  attr(result_df, "digits") <- digits

  return(result_df)
}
