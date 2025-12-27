#' Panel Data Variation Decomposition
#'
#' This function calculates descriptive statistics for panel data and decomposes
#' variance into between and within components.
#'
#' @param data A data.frame containing panel data.
#' @param variables A character vector specifying which numeric variables to analyze.
#'   If not specified, all numeric variables in the data.frame will be used.
#' @param group A character string specifying the name of the entity/group variable in panel data.
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
#' @references
#' For Stata users: This corresponds to the `xtsum` command.
#'
#' @seealso
#' [describe()], [plot_heterogeneity()], [explore_participation()], [describe_transition()]
#'
#' @examples
#' data(production)
#'
#' # Using default detailed = TRUE (Stata-like output) with default rounding
#' decompose_variation(production, group = "firm")
#'
#' # Simplified output with custom rounding to 2 decimal places
#' decompose_variation(production, variables = "sales", group = "firm",
#'                    detailed = FALSE, digits = 2)
#'
#' # Detailed output with no rounding
#' decompose_variation(production,
#'                    variables = c("sales", "capital"),
#'                    group = "firm",
#'                    digits = NA)
#'
#' @export
decompose_variation <- function(
  data,
  variables = NULL,
  group = NULL,
  detailed = TRUE,
  digits = 3
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop(
      "decompose_variation: 'data' must be a data.frame, not ",
      class(data)[1]
    )
  }

  if (!is.null(variables) && !is.character(variables)) {
    stop(
      "decompose_variation: 'variables' must be a character vector or NULL, not ",
      class(variables)[1]
    )
  }

  if (!is.character(group) || length(group) != 1) {
    stop(
      "decompose_variation: 'group' must be a single character string, not ",
      class(group)[1]
    )
  }

  if (!group %in% names(data)) {
    stop('decompose_variation: variable "', group, '" not found in data')
  }

  if (!is.logical(detailed) || length(detailed) != 1) {
    stop(
      "decompose_variation: 'detailed' must be a single logical value, not ",
      class(detailed)[1]
    )
  }

  if (!is.numeric(digits) || length(digits) != 1) {
    stop(
      "decompose_variation: 'digits' must be a single numeric value, not ",
      class(digits)[1]
    )
  }

  data_df <- .check_and_convert_data_robust(data, arg_name = "data")

  # Validate digits parameter
  if (
    !is.na(digits) &&
      (!is.numeric(digits) || digits < 0 || digits != round(digits))
  ) {
    stop(
      "decompose_variation: 'digits' must be a non-negative integer or NA for no rounding"
    )
  }

  # Validate group parameter
  if (
    is.null(group) || !is.character(group) || length(group) == 0 || group == ""
  ) {
    stop("decompose_variation: 'group' must be a non-empty character string")
  }

  if (!group %in% names(data_df)) {
    stop(
      "decompose_variation: variable '",
      group,
      "' not found in data. Available variables: ",
      paste(names(data_df), collapse = ", ")
    )
  }

  # If variables is not specified, use all numeric variables
  if (is.null(variables)) {
    numeric_vars <- vapply(data_df, is.numeric, FUN.VALUE = logical(1))
    variables <- names(data_df)[numeric_vars]

    # Remove the group variable from variables if it's numeric
    if (group %in% variables) {
      variables <- variables[variables != group]
    }

    # Remove other potential ID variables
    id_like_vars <- c("id", "ID", "Id", "year", "time", "period", "date")
    variables <- variables[!variables %in% id_like_vars]

    if (length(variables) == 0) {
      stop("decompose_variation: no numeric variables found in the dataset")
    }

    message(
      "Note: analyzing all numeric variables: ",
      paste(variables, collapse = ", ")
    )
  }

  # Validate variables
  missing_vars <- variables[!variables %in% names(data_df)]
  if (length(missing_vars) > 0) {
    stop(
      "decompose_variation: the following variables were not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Check if specified columns are numeric
  non_numeric_vars <- variables[
    !vapply(data_df[variables], is.numeric, FUN.VALUE = logical(1))
  ]
  if (length(non_numeric_vars) > 0) {
    stop(
      "decompose_variation: the following variables are not numeric: ",
      paste(non_numeric_vars, collapse = ", ")
    )
  }

  # Check group variable
  group_vector <- data_df[[group]]
  if (length(group_vector) == 0) {
    stop("decompose_variation: group variable '", group, "' has zero length")
  }

  n_groups <- length(unique(group_vector))
  if (n_groups > 10000) {
    warning(
      "Note: large number of groups (",
      n_groups,
      "). This may impact performance. (occurred in decompose_variation)"
    )
  }

  # Helper function to calculate panel statistics for one variable
  decompose_variation_1 <- function(
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
  results <- lapply(variables, function(varname) {
    decompose_variation_1(data_df, varname, group, detailed, digits)
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
