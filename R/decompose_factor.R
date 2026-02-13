#' Panel Data Factor Variable Decomposition
#'
#' This function performs one-way tabulations and decomposes counts into
#' between and within components for categorical (factor) variables in panel data.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param selection A character vector specifying which categorical (factor) variables to analyze.
#'        If not specified, all factor variables in the data.frame will be used.
#' @param group A character string specifying the name of the entity/group variable.
#'        Not required if data has panel attributes.
#' @param format A character string specifying the output format: "wide" or "long". Default = "wide".
#' @param digits An integer indicating the number of decimal places to round shares.
#'        Default = 3.
#'
#' @return A data.frame with categorical panel data decomposition statistics. Format depends on
#'         the `format` argument.
#'
#' @details
#' The output format is controlled by the `format` parameter:
#'
#' When `format = "wide"` (default), returns a data.frame with:
#' \describe{
#'   \item{\code{variable}}{The name of the analyzed variable}
#'   \item{\code{category}}{The category level of the variable}
#'   \item{\code{n_overall}}{Overall frequency (person-time observations)}
#'   \item{\code{share_overall}}{Overall share (n_overall / total_obs)}
#'   \item{\code{n_between}}{Between-group frequency (number of groups ever having this category)}
#'   \item{\code{share_between}}{Between-group share (n_between / total_groups)}
#'   \item{\code{share_within}}{Within-group share (average share of time groups have this category)}
#' }
#'
#' When `format = "long"`, returns a data.frame with:
#' \describe{
#'   \item{\code{variable}}{The name of the analyzed variable}
#'   \item{\code{category}}{The category level of the variable}
#'   \item{\code{dimension}}{Type of decomposition: "overall", "between", or "within"}
#'   \item{\code{n}}{Frequency count (NA for within dimension)}
#'   \item{\code{share}}{Share proportion (0 to 1)}
#' }
#'
#' The data.frame has additional attributes:
#' \describe{
#'   \item{\code{panel_group}}{The grouping variable name}
#'   \item{\code{panel_n_groups}}{Number of unique groups}
#'   \item{\code{panel_format}}{Output format ("wide" or "long")}
#'   \item{\code{panel_digits}}{Number of decimal places used for rounding}
#' }
#'
#' @references
#' For Stata users: This corresponds to the `xttab` command.
#'
#' @seealso
#' [decompose_numeric()], [summarize_transition()], [summarize_numeric()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage with statistics for all factor variables (wide format)
#' decompose_factor(production, group = "firm")
#'
#' # Long format output
#' decompose_factor(production, group = "firm", format = "long")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' decompose_factor(panel_data)
#'
#' # Show statistics for a single categorical variable
#' decompose_factor(production, selection = "industry", group = "firm")
#'
#' # Show statistics for multiple categorical variables
#' decompose_factor(production, selection = c("industry", "year"), group = "firm")
#'
#' # Show statistics with two digits rounding
#' decompose_factor(production, group = "firm", digits = 2)
#'
#' # Effectively no rounding (use large digit value)
#' decompose_factor(production, group = "firm", digits = 999999)
#'
#' @export
decompose_factor <- function(
  data,
  selection = NULL,
  group = NULL,
  format = "wide",
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
    stop("'group' must be a single character string, not ", class(group)[1])
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  # Validate format argument
  if (!is.character(format) || length(format) != 1) {
    stop("'format' must be a single character string, not ", class(format)[1])
  }

  if (!format %in% c("wide", "long")) {
    stop('format must be either "wide" or "long", not "', format, '"')
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

  # Validate group parameter
  if (group == "" || length(group) == 0) {
    stop("'group' must be a non-empty character string")
  }

  if (!group %in% names(data)) {
    stop(
      "variable '",
      group,
      "' not found in data. Available variables: ",
      paste(names(data), collapse = ", ")
    )
  }

  # Track if any messages were printed
  messages_printed <- FALSE

  # If selection is not specified, use only factor variables (not character)
  if (is.null(selection)) {
    # Check for factor variables
    is_factor <- vapply(
      data,
      is.factor,
      FUN.VALUE = logical(1)
    )

    # Exclude group variable from selection
    is_factor[group] <- FALSE

    selection <- names(data)[is_factor]

    if (length(selection) == 0) {
      stop("no factor variables found in the dataset")
    }

    message(
      "Analyzing all factor variables: ",
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

  # When selection is explicitly provided, convert all to factors
  # Check if any variables need conversion
  for (var in selection) {
    if (!is.factor(data[[var]])) {
      message(
        "Converting variable '",
        var,
        "' to factor. Original class: ",
        class(data[[var]])[1]
      )
      messages_printed <- TRUE
      data[[var]] <- factor(data[[var]])
    }
  }

  # Convert group to character for consistent handling
  data[[group]] <- as.character(data[[group]])

  # Get total number of groups
  n_groups <- length(unique(data[[group]]))

  # Helper function to calculate categorical statistics for one variable
  decompose_factor_1 <- function(df, varname, grp, format_output, digits_val) {
    # Remove rows with NA in the variable or group
    complete_cases <- complete.cases(df[[varname]], df[[grp]])
    df_clean <- df[complete_cases, , drop = FALSE]

    if (nrow(df_clean) == 0) {
      if (format_output == "wide") {
        return(data.frame(
          variable = character(),
          category = character(),
          n_overall = integer(),
          share_overall = numeric(),
          n_between = integer(),
          share_between = numeric(),
          share_within = numeric(),
          stringsAsFactors = FALSE
        ))
      } else {
        # long format
        return(data.frame(
          variable = character(),
          category = character(),
          dimension = character(),
          n = integer(),
          share = numeric(),
          stringsAsFactors = FALSE
        ))
      }
    }

    # Ensure the variable is treated as factor (should already be factor)
    if (!is.factor(df_clean[[varname]])) {
      df_clean[[varname]] <- factor(df_clean[[varname]])
    }

    # Get all categories
    categories <- levels(df_clean[[varname]])

    # Calculate overall statistics (person-time)
    overall_counts <- table(df_clean[[varname]])
    total_obs <- sum(overall_counts)

    # Calculate between statistics (groups ever having category)
    # For each group, check which categories it ever had
    group_data <- split(df_clean[[varname]], df_clean[[grp]])

    # Count groups that ever had each category
    between_counts <- sapply(categories, function(cat) {
      sum(sapply(group_data, function(gdata) {
        any(as.character(gdata) == cat)
      }))
    })

    # Calculate within shares (proportions)
    # For each category, calculate average share across groups that ever had it
    within_shares <- sapply(categories, function(cat) {
      # Get groups that ever had this category
      groups_with_cat <- which(sapply(group_data, function(gdata) {
        any(as.character(gdata) == cat)
      }))

      if (length(groups_with_cat) == 0) {
        return(0)
      }

      # For each group that ever had the category, calculate what share
      # of its observations have this category
      group_shares <- sapply(groups_with_cat, function(i) {
        gdata <- group_data[[i]]
        sum(as.character(gdata) == cat) / length(gdata)
      })

      # Average these shares
      mean(group_shares)
    })

    # Calculate shares (proportions, not percentages)
    share_overall <- as.numeric(overall_counts / total_obs)
    share_between <- as.numeric(between_counts / n_groups)

    # Apply rounding
    share_overall <- round_if_needed(share_overall, digits_val)
    share_between <- round_if_needed(share_between, digits_val)
    within_shares <- round_if_needed(within_shares, digits_val)

    if (format_output == "wide") {
      # Wide format with one row per category
      result <- data.frame(
        variable = rep(varname, length(categories)),
        category = categories,
        n_overall = as.integer(overall_counts),
        share_overall = share_overall,
        n_between = as.integer(between_counts),
        share_between = share_between,
        share_within = within_shares,
        stringsAsFactors = FALSE
      )
    } else {
      # Long format with decomposition rows
      # Overall rows
      overall_rows <- data.frame(
        variable = rep(varname, length(categories)),
        category = categories,
        dimension = rep("overall", length(categories)),
        n = as.integer(overall_counts),
        share = share_overall,
        stringsAsFactors = FALSE
      )

      # Between rows
      between_rows <- data.frame(
        variable = rep(varname, length(categories)),
        category = categories,
        dimension = rep("between", length(categories)),
        n = as.integer(between_counts),
        share = share_between,
        stringsAsFactors = FALSE
      )

      # Within rows
      within_rows <- data.frame(
        variable = rep(varname, length(categories)),
        category = categories,
        dimension = rep("within", length(categories)),
        n = NA_integer_, # No direct n for within
        share = within_shares,
        stringsAsFactors = FALSE
      )

      result <- rbind(overall_rows, between_rows, within_rows)
      rownames(result) <- NULL
    }

    return(result)
  }

  # Calculate statistics for each variable
  results <- lapply(selection, function(varname) {
    decompose_factor_1(data, varname, group, format, digits)
  })

  # Combine all results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  # For long format, order by variable first, then by category
  if (format == "long") {
    # First ensure categories are ordered as factors (preserving level order)
    result_df$category <- factor(
      result_df$category,
      levels = unique(result_df$category)
    )

    # Order by variable and category
    result_df <- result_df[order(result_df$variable, result_df$category), ]
    rownames(result_df) <- NULL
  }

  # Add standardized attributes
  attr(result_df, "panel_group") <- group
  attr(result_df, "panel_n_groups") <- n_groups
  attr(result_df, "panel_format") <- format
  attr(result_df, "panel_digits") <- digits

  # Add empty line before returning data.frame if messages were printed
  if (messages_printed) {
    cat("\n")
  }

  return(result_df)
}
