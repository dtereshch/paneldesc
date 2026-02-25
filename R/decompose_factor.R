#' Panel Data Factor Variable Decomposition
#'
#' This function performs one-way tabulations and decomposes counts into
#' between and within components for categorical (factor) variables in panel data.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param selection A character vector specifying which categorical (factor) variables to analyze.
#'        If not specified, all factor variables in the data.frame will be used.
#' @param group A character string specifying the name of the entity/group variable.
#'        Not required if data has panel attributes.
#' @param time An optional character string specifying the name of the time variable.
#'        If provided, the function checks for duplicate group-time combinations.
#'        Not required if data has panel attributes.
#' @param format A character string specifying the output format: "wide" or "long". Default = "wide".
#' @param digits An integer indicating the number of decimal places to round shares.
#'        Default = 3.
#'
#' @return A data.frame with categorical panel data decomposition statistics.
#'
#' @details
#' The output format is controlled by the `format` parameter:
#'
#' When `format = "wide"` (default), returns a data.frame with columns:
#' \describe{
#'   \item{\code{variable}}{The name of the analyzed variable}
#'   \item{\code{category}}{The category level of the variable}
#'   \item{\code{count_overall}}{Overall frequency (person-time observations)}
#'   \item{\code{share_overall}}{Overall share (count_overall / total_obs)}
#'   \item{\code{count_between}}{Between-group frequency (number of groups ever having this category)}
#'   \item{\code{share_between}}{Between-group share (count_between / total_groups)}
#'   \item{\code{share_within}}{Within-group share (average share of time groups have this category)}
#' }
#'
#' When `format = "long"`, returns a data.frame with columns:
#' \describe{
#'   \item{\code{variable}}{The name of the analyzed variable}
#'   \item{\code{category}}{The category level of the variable}
#'   \item{\code{dimension}}{Type of decomposition: "overall", "between", or "within"}
#'   \item{\code{count}}{Frequency count (NA for within dimension)}
#'   \item{\code{share}}{Share proportion (0 to 1)}
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
#' # Show statistics with time variable to check duplicates
#' decompose_factor(production, group = "firm", time = "year")
#'
#' @export
decompose_factor <- function(
  data,
  selection = NULL,
  group = NULL,
  time = NULL,
  format = "wide",
  digits = 3
) {
  # --- Consistent initialisation: user input overrides metadata ---
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
    # group: use metadata only if user did not supply it
    if (is.null(group)) {
      group <- metadata$group
    }
    # time: use metadata only if user did not supply it and metadata has time
    if (is.null(time) && !is.null(metadata$time)) {
      time <- metadata$time
    }

    # Determine if identifiers came from metadata
    group_from_metadata <- is.null(user_group) && !is.null(metadata$group)
    time_from_metadata <- is.null(user_time) && !is.null(metadata$time)
    # Combined flag for duplicate messages (used only when time is not NULL)
    group_time_from_metadata <- group_from_metadata &&
      (is.null(time) || time_from_metadata)
  } else {
    # Regular data frame
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame")
    }
    if (is.null(group)) {
      stop("For regular data.frames, 'group' argument must be provided")
    }
    # time may be NULL
  }

  # Common validation for group
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  # Validate time if provided
  if (!is.null(time)) {
    if (!is.character(time) || length(time) != 1) {
      stop("'time' must be a single character string, not ", class(time)[1])
    }
    if (!time %in% names(data)) {
      stop('variable "', time, '" not found in data')
    }
    if (time == group) {
      stop("'time' and 'group' must be different variables")
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

  # Validate selection, format, digits (unchanged) ...
  if (!is.null(selection) && !is.character(selection)) {
    stop(
      "'selection' must be a character vector or NULL, not ",
      class(selection)[1]
    )
  }

  if (!is.character(format) || length(format) != 1) {
    stop("'format' must be a single character string, not ", class(format)[1])
  }

  if (!format %in% c("wide", "long")) {
    stop('format must be either "wide" or "long", not "', format, '"')
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

  # If selection is not specified, use only factor variables (not character)
  if (is.null(selection)) {
    is_factor <- vapply(data, is.factor, FUN.VALUE = logical(1))
    is_factor[group] <- FALSE
    if (!is.null(time) && time %in% names(data)) {
      is_factor[time] <- FALSE
    }
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

  # Convert selected variables to factor if needed
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
  count_groups <- length(unique(data[[group]]))

  # Helper function to calculate categorical statistics for one variable (unchanged)
  decompose_factor_1 <- function(df, varname, grp, format_output, digits_val) {
    complete_cases <- complete.cases(df[[varname]], df[[grp]])
    df_clean <- df[complete_cases, , drop = FALSE]

    if (nrow(df_clean) == 0) {
      if (format_output == "wide") {
        return(data.frame(
          variable = character(),
          category = character(),
          count_overall = integer(),
          share_overall = numeric(),
          count_between = integer(),
          share_between = numeric(),
          share_within = numeric(),
          stringsAsFactors = FALSE
        ))
      } else {
        return(data.frame(
          variable = character(),
          category = character(),
          dimension = character(),
          count = integer(),
          share = numeric(),
          stringsAsFactors = FALSE
        ))
      }
    }

    if (!is.factor(df_clean[[varname]])) {
      df_clean[[varname]] <- factor(df_clean[[varname]])
    }

    categories <- levels(df_clean[[varname]])
    overall_counts <- table(df_clean[[varname]])
    total_obs <- sum(overall_counts)

    group_data <- split(df_clean[[varname]], df_clean[[grp]])

    between_counts <- sapply(categories, function(cat) {
      sum(sapply(group_data, function(gdata) {
        any(as.character(gdata) == cat)
      }))
    })

    within_shares <- sapply(categories, function(cat) {
      groups_with_cat <- which(sapply(group_data, function(gdata) {
        any(as.character(gdata) == cat)
      }))
      if (length(groups_with_cat) == 0) {
        return(0)
      }
      group_shares <- sapply(groups_with_cat, function(i) {
        gdata <- group_data[[i]]
        sum(as.character(gdata) == cat) / length(gdata)
      })
      mean(group_shares)
    })

    share_overall <- as.numeric(overall_counts / total_obs)
    share_between <- as.numeric(between_counts / count_groups)

    share_overall <- round_if_needed(share_overall, digits_val)
    share_between <- round_if_needed(share_between, digits_val)
    within_shares <- round_if_needed(within_shares, digits_val)

    if (format_output == "wide") {
      result <- data.frame(
        variable = rep(varname, length(categories)),
        category = categories,
        count_overall = as.integer(overall_counts),
        share_overall = share_overall,
        count_between = as.integer(between_counts),
        share_between = share_between,
        share_within = within_shares,
        stringsAsFactors = FALSE
      )
    } else {
      overall_rows <- data.frame(
        variable = rep(varname, length(categories)),
        category = categories,
        dimension = rep("overall", length(categories)),
        count = as.integer(overall_counts),
        share = share_overall,
        stringsAsFactors = FALSE
      )
      between_rows <- data.frame(
        variable = rep(varname, length(categories)),
        category = categories,
        dimension = rep("between", length(categories)),
        count = as.integer(between_counts),
        share = share_between,
        stringsAsFactors = FALSE
      )
      within_rows <- data.frame(
        variable = rep(varname, length(categories)),
        category = categories,
        dimension = rep("within", length(categories)),
        count = NA_integer_,
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
    result_df$category <- factor(
      result_df$category,
      levels = unique(result_df$category)
    )
    result_df <- result_df[order(result_df$variable, result_df$dimension), ]
    rownames(result_df) <- NULL
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
