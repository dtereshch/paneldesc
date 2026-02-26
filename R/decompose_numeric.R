#' Panel Data Numeric Variable Decomposition
#'
#' This function decomposes variance of numeric variables
#' into between and within components in panel data.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param selection A character vector specifying which numeric variables to analyze.
#'        If not specified, all numeric variables in the data.frame will be used.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time An optional character string specifying the name of the time variable.
#'        If provided, the function checks for duplicate group-time combinations.
#'        Not required if data has panel attributes.
#' @param format A character string specifying the output format: "long" or "wide".
#'        Default = "long".
#' @param detailed A logical flag indicating whether to return detailed Stata-like output.
#'        Default = TRUE.
#' @param digits An integer indicating the number of decimal places to round statistics.
#'        Default = 3.
#'
#' @return A data.frame with panel data decomposition statistics.
#'
#' @details
#' The output format is controlled by two parameters: `format` and `detailed`.
#'
#' When `format = "long"` and `detailed = TRUE` (default), returns a data.frame with:
#' \describe{
#'   \item{\code{variable}}{The name of the analyzed variable}
#'   \item{\code{dimension}}{Type of decomposition: "overall", "between", or "within"}
#'   \item{\code{mean}}{Mean value (only for "overall" row)}
#'   \item{\code{std}}{Standard deviation}
#'   \item{\code{min}}{Minimum value}
#'   \item{\code{max}}{Maximum value}
#'   \item{\code{count}}{Number of observations or groups}
#' }
#'
#' When `format = "long"` and `detailed = FALSE`, returns a data.frame with:
#' \describe{
#'   \item{\code{variable}}{The name of the variable}
#'   \item{\code{dimension}}{Type of decomposition: "overall", "between", or "within"}
#'   \item{\code{mean}}{Mean value}
#'   \item{\code{std}}{Standard deviation}
#' }
#'
#' When `format = "wide"` and `detailed = TRUE`, returns a data.frame with:
#' \describe{
#'   \item{\code{variable}}{The name of the variable}
#'   \item{\code{mean}}{Overall mean}
#'   \item{\code{std_overall}}{Overall standard deviation}
#'   \item{\code{min_overall}}{Overall minimum}
#'   \item{\code{max_overall}}{Overall maximum}
#'   \item{\code{count_overall}}{Number of observations}
#'   \item{\code{std_between}}{Between-group standard deviation}
#'   \item{\code{min_between}}{Minimum of group means}
#'   \item{\code{max_between}}{Maximum of group means}
#'   \item{\code{count_between}}{Number of groups}
#'   \item{\code{std_within}}{Within-group standard deviation}
#'   \item{\code{min_within}}{Within-group minimum (transformed)}
#'   \item{\code{max_within}}{Within-group maximum (transformed)}
#'   \item{\code{count_within}}{Average observations per group}
#' }
#'
#' When `format = "wide"` and `detailed = FALSE`, returns a data.frame with:
#' \describe{
#'   \item{\code{variable}}{The name of the variable}
#'   \item{\code{mean}}{Overall mean}
#'   \item{\code{std_overall}}{Overall standard deviation}
#'   \item{\code{std_between}}{Between-group standard deviation}
#'   \item{\code{std_within}}{Within-group standard deviation}
#' }
#'
#' Before any analysis, rows with missing values (`NA`) in the `group` or `time`
#' (if provided) variables are removed. Messages indicate how many rows were
#' excluded due to each variable. The excluded rows are stored in
#' `details$excluded_rows` for further inspection.
#'
#' If a time variable is supplied (either by the user or from `panel_data` metadata),
#' the function checks for duplicate group-time combinations. In a properly structured
#' panel dataset, each entity (group) should have at most one observation per time period.
#' If duplicates are found, they are stored in `details$entity_time_duplicates`.
#' A message is printed only when the identifiers were explicitly provided (i.e., not taken
#' from `panel_data` attributes).
#'
#' The returned data.frame has class `"panel_summary"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing additional information: `count_groups`,
#'         `excluded_rows` (if any), and, if time was supplied and duplicates exist,
#'         `entity_time_duplicates`.}
#' }
#'
#' @references
#' For Stata users: This corresponds to the `xtsum` command.
#'
#' @seealso
#' [summarize_numeric()], [plot_heterogeneity()], [decompose_factor()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage with statistics for all numeric variables
#' decompose_numeric(production, group = "firm")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' decompose_numeric(panel_data)
#'
#' # Show statistics with time variable to check duplicates
#' decompose_numeric(production, group = "firm", time = "year")
#'
#' @export
decompose_numeric <- function(
  data,
  selection = NULL,
  group = NULL,
  time = NULL,
  format = "long",
  detailed = TRUE,
  digits = 3
) {
  # --- Consistent initialisation ---
  user_group <- group
  user_time <- time
  group_time_from_metadata <- FALSE

  if (inherits(data, "panel_data")) {
    metadata <- attr(data, "metadata")
    if (is.null(metadata) || is.null(metadata$group)) {
      stop(
        "Object has class 'panel_data' but missing or incomplete 'metadata' attribute."
      )
    }
    if (is.null(group)) {
      group <- metadata$group
    }
    if (is.null(time) && !is.null(metadata$time)) {
      time <- metadata$time
    }

    group_from_metadata <- is.null(user_group) && !is.null(metadata$group)
    time_from_metadata <- is.null(user_time) && !is.null(metadata$time)
    group_time_from_metadata <- group_from_metadata &&
      (is.null(time) || time_from_metadata)
  } else {
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame")
    }
    if (is.null(group)) {
      stop("For regular data.frames, 'group' argument must be provided")
    }
  }

  # --- Remove rows with NA in group or time (if time provided) ---
  excluded_rows <- NULL
  na_group <- is.na(data[[group]])
  na_time <- if (!is.null(time)) is.na(data[[time]]) else rep(FALSE, nrow(data))

  if (any(na_group)) {
    message(
      "Missing values in ",
      group,
      " variable found. Excluding ",
      sum(na_group),
      " rows."
    )
  }
  if (!is.null(time) && any(na_time)) {
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

  # --- Check for duplicate group-time combinations (only if time is provided) ---
  dup_combinations <- NULL
  if (!is.null(time)) {
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
  }
  # ----------------------------------------------------------------

  # Validate selection, format, detailed, digits (unchanged) ...
  if (!is.null(selection) && !is.character(selection)) {
    stop(
      "'selection' must be a character vector or NULL, not ",
      class(selection)[1]
    )
  }

  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("'detailed' must be a single logical value, not ", class(detailed)[1])
  }

  if (!is.character(format) || length(format) != 1) {
    stop("'format' must be a single character string, not ", class(format)[1])
  }

  if (!format %in% c("long", "wide")) {
    stop('format must be either "long" or "wide", not "', format, '"')
  }

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

  # Track if any messages were printed (for selection auto-detection)
  messages_printed <- FALSE

  # If selection is not specified, use all numeric variables
  if (is.null(selection)) {
    numeric_vars <- vapply(data, is.numeric, FUN.VALUE = logical(1))
    selection <- names(data)[numeric_vars]
    # Remove group variable if it's numeric
    if (group %in% selection) {
      selection <- selection[selection != group]
    }
    # Remove time variable if it's numeric (and provided)
    if (!is.null(time) && time %in% selection) {
      selection <- selection[selection != time]
    }

    if (length(selection) == 0) {
      stop("no numeric variables found in the dataset")
    }

    message(
      "Analyzing all numeric variables: ",
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

  # Check if specified columns are numeric
  non_numeric_vars <- selection[
    !vapply(data[selection], is.numeric, FUN.VALUE = logical(1))
  ]
  if (length(non_numeric_vars) > 0) {
    stop(
      "the following variables are not numeric: ",
      paste(non_numeric_vars, collapse = ", ")
    )
  }

  # --- NEW CHECK: ensure group variable is not in selection ---
  if (group %in% selection) {
    stop("'selection' cannot be the same as 'group' variable ('", group, "')")
  }
  # ------------------------------------------------------------

  # Check group variable
  group_vector <- data[[group]]
  if (length(group_vector) == 0) {
    stop("group variable '", group, "' has zero length")
  }

  count_groups <- length(unique(group_vector))
  if (count_groups > 10000) {
    warning(
      "Large number of groups (",
      count_groups,
      "). This may impact performance."
    )
  }

  # Helper function to calculate panel statistics for one variable (unchanged)
  decompose_numeric_1 <- function(
    data,
    varname,
    group,
    format_output,
    detailed_output,
    digits_val
  ) {
    complete_cases <- complete.cases(data[[varname]], data[[group]])
    df <- data[complete_cases, , drop = FALSE]

    if (nrow(df) == 0) {
      if (format_output == "long" && detailed_output) {
        return(data.frame(
          variable = character(),
          dimension = character(),
          mean = numeric(),
          std = numeric(),
          min = numeric(),
          max = numeric(),
          count = numeric(),
          stringsAsFactors = FALSE
        ))
      } else if (format_output == "long" && !detailed_output) {
        return(data.frame(
          variable = character(),
          dimension = character(),
          mean = numeric(),
          std = numeric(),
          stringsAsFactors = FALSE
        ))
      } else if (format_output == "wide" && detailed_output) {
        return(data.frame(
          variable = varname,
          mean = NA_real_,
          std_overall = NA_real_,
          min_overall = NA_real_,
          max_overall = NA_real_,
          count_overall = NA_integer_,
          std_between = NA_real_,
          min_between = NA_real_,
          max_between = NA_real_,
          count_between = NA_integer_,
          std_within = NA_real_,
          min_within = NA_real_,
          max_within = NA_real_,
          count_within = NA_real_
        ))
      } else {
        # wide and !detailed
        return(data.frame(
          variable = varname,
          mean = NA_real_,
          std_overall = NA_real_,
          std_between = NA_real_,
          std_within = NA_real_
        ))
      }
    }

    group_vec <- as.character(df[[group]])
    x <- df[[varname]]

    overall_mean <- mean(x, na.rm = TRUE)
    overall_std <- sd(x, na.rm = TRUE)
    min_val <- min(x, na.rm = TRUE)
    max_val <- max(x, na.rm = TRUE)
    count_obs <- length(x)

    group_means <- tapply(x, group_vec, mean, na.rm = TRUE)
    between_std <- sd(group_means, na.rm = TRUE)
    between_min <- min(group_means, na.rm = TRUE)
    between_max <- max(group_means, na.rm = TRUE)
    count_groups_var <- length(group_means)

    group_means_expanded <- group_means[match(group_vec, names(group_means))]
    deviations <- x - group_means_expanded
    within_transformed <- deviations + overall_mean

    within_std <- sd(deviations, na.rm = TRUE)
    within_min <- min(within_transformed, na.rm = TRUE)
    within_max <- max(within_transformed, na.rm = TRUE)

    obs_per_group <- table(group_vec)
    avg_obs_per_group <- mean(obs_per_group, na.rm = TRUE)

    overall_mean <- round_if_needed(overall_mean, digits_val)
    overall_std <- round_if_needed(overall_std, digits_val)
    min_val <- round_if_needed(min_val, digits_val)
    max_val <- round_if_needed(max_val, digits_val)
    between_std <- round_if_needed(between_std, digits_val)
    between_min <- round_if_needed(between_min, digits_val)
    between_max <- round_if_needed(between_max, digits_val)
    within_std <- round_if_needed(within_std, digits_val)
    within_min <- round_if_needed(within_min, digits_val)
    within_max <- round_if_needed(within_max, digits_val)
    avg_obs_per_group <- round_if_needed(avg_obs_per_group, digits_val)

    if (format_output == "long") {
      if (detailed_output) {
        result <- data.frame(
          variable = c(varname, varname, varname),
          dimension = c("overall", "between", "within"),
          mean = c(overall_mean, NA, NA),
          std = c(overall_std, between_std, within_std),
          min = c(min_val, between_min, within_min),
          max = c(max_val, between_max, within_max),
          count = c(count_obs, count_groups_var, avg_obs_per_group),
          stringsAsFactors = FALSE
        )
      } else {
        result <- data.frame(
          variable = c(varname, varname, varname),
          dimension = c("overall", "between", "within"),
          mean = c(overall_mean, NA, NA),
          std = c(overall_std, between_std, within_std),
          stringsAsFactors = FALSE
        )
      }
    } else {
      # wide
      if (detailed_output) {
        result <- data.frame(
          variable = varname,
          mean = overall_mean,
          std_overall = overall_std,
          min_overall = min_val,
          max_overall = max_val,
          count_overall = count_obs,
          std_between = between_std,
          min_between = between_min,
          max_between = between_max,
          count_between = count_groups_var,
          std_within = within_std,
          min_within = within_min,
          max_within = within_max,
          count_within = avg_obs_per_group,
          stringsAsFactors = FALSE
        )
      } else {
        result <- data.frame(
          variable = varname,
          mean = overall_mean,
          std_overall = overall_std,
          std_between = between_std,
          std_within = within_std,
          stringsAsFactors = FALSE
        )
      }
    }
    return(result)
  }

  # Calculate statistics for each variable
  results <- lapply(selection, function(varname) {
    decompose_numeric_1(data, varname, group, format, detailed, digits)
  })

  # Combine all results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    selection = selection,
    group = group,
    time = time,
    format = format,
    detailed = detailed,
    digits = digits
  )

  # Build details list
  details <- list(
    count_groups = count_groups
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
