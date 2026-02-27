#' Panel Data Factor Variable Decomposition
#'
#' This function performs one-way tabulations and decomposes counts into
#' between and within components for categorical (factor) variables in panel data.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 1 or 2 specifying the names of the
#'        entity and (optionally) time variables. The first element is the entity
#'        variable; if a second element is provided, it is used as the time variable.
#'        Not required if data has panel attributes.
#' @param select A character vector specifying which categorical (factor) variables to analyze.
#'        If not specified, all factor variables in the data.frame will be used.
#' @param format A character string specifying the output format: "wide" or "long".
#'        Default = "wide".
#' @param digits An integer indicating the number of decimal places to round shares.
#'        Default = 3.
#'
#' @return A data.frame with categorical panel data decomposition statistics, class `"panel_summary"`.
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
#'   \item{\code{count_between}}{Between-entity frequency (number of entities ever having this category)}
#'   \item{\code{share_between}}{Between-entity share (count_between / total_entities)}
#'   \item{\code{share_within}}{Within-entity share (average share of time entities have this category)}
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
#' Before any analysis, rows with missing values (`NA`) in the entity or (if provided)
#' time variables are removed. Messages indicate how many rows were excluded due to each variable.
#' The excluded rows are stored in `details$excluded_rows` for further inspection.
#'
#' If a time variable is supplied (either via `index` or from panel metadata),
#' the function checks for duplicate entity-time combinations. In a properly structured
#' panel dataset, each entity should have at most one observation per time period.
#' If duplicates are found, they are stored in `details$entity_time_duplicates`.
#' A message is printed only when the identifiers were explicitly provided (i.e., not taken
#' from panel attributes).
#'
#' The returned data.frame has class `"panel_summary"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing additional information: `count_entities`,
#'         `excluded_rows` (if any), and, if time was supplied and duplicates exist,
#'         `entity_time_duplicates`.}
#' }
#'
#' @references
#' For Stata users: This corresponds to the `xttab` command.
#'
#' @seealso
#' [decompose_numeric()], [summarize_transition()]
#'
#' @examples
#' data(production)
#' decompose_factor(production, index = "firm")
#' decompose_factor(production, index = c("firm", "year"), select = "industry")
#'
#' @export
decompose_factor <- function(
  data,
  index = NULL,
  select = NULL,
  format = "wide",
  digits = 3
) {
  # --- Initialisation: entity and time from index or metadata ---
  user_index <- index
  entity_time_from_metadata <- FALSE

  if (inherits(data, "panel_data")) {
    metadata <- attr(data, "metadata")
    if (is.null(metadata) || is.null(metadata$entity)) {
      stop("Object has class 'panel_data' but missing 'entity' in metadata.")
    }
    # time is optional in metadata
    time_meta <- if (!is.null(metadata$time)) metadata$time else NULL

    if (is.null(index)) {
      entity_var <- metadata$entity
      time_var <- time_meta
    } else {
      if (length(index) == 1) {
        entity_var <- index[1]
        time_var <- NULL
      } else if (length(index) == 2) {
        entity_var <- index[1]
        time_var <- index[2]
      } else {
        stop("'index' must be a character vector of length 1 or 2")
      }
    }
    # Determine if identifiers came from metadata (for duplicate message)
    entity_from_metadata <- is.null(user_index) && !is.null(metadata$entity)
    time_from_metadata <- is.null(user_index) &&
      !is.null(time_meta) &&
      (length(user_index) < 2 || is.null(user_index[2]))
    entity_time_from_metadata <- entity_from_metadata &&
      (is.null(time_var) || time_from_metadata)
  } else {
    # Regular data frame
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame")
    }
    if (is.null(index)) {
      stop("For regular data.frames, 'index' must be provided")
    }
    if (length(index) == 1) {
      entity_var <- index[1]
      time_var <- NULL
    } else if (length(index) == 2) {
      entity_var <- index[1]
      time_var <- index[2]
    } else {
      stop("'index' must be a character vector of length 1 or 2")
    }
  }

  # --- Validate existence of variables in data ---
  if (!entity_var %in% names(data)) {
    stop('entity variable "', entity_var, '" not found in data')
  }
  if (!is.null(time_var) && !time_var %in% names(data)) {
    stop('time variable "', time_var, '" not found in data')
  }
  if (!is.null(time_var) && time_var == entity_var) {
    stop("entity and time variables cannot be the same")
  }

  # --- Remove rows with NA in entity or time (if time provided) ---
  excluded_rows <- NULL
  na_entity <- is.na(data[[entity_var]])
  na_time <- if (!is.null(time_var)) {
    is.na(data[[time_var]])
  } else {
    rep(FALSE, nrow(data))
  }

  if (any(na_entity)) {
    message(
      "Missing values in entity variable '",
      entity_var,
      "' found. Excluding ",
      sum(na_entity),
      " rows."
    )
  }
  if (!is.null(time_var) && any(na_time)) {
    message(
      "Missing values in time variable '",
      time_var,
      "' found. Excluding ",
      sum(na_time),
      " rows."
    )
  }

  if (any(na_entity | na_time)) {
    excluded_rows <- data[na_entity | na_time, , drop = FALSE]
    data <- data[!(na_entity | na_time), , drop = FALSE]
    rownames(data) <- NULL
  }

  # --- Check for duplicate entity-time combinations (only if time provided) ---
  dup_combinations <- NULL
  if (!is.null(time_var)) {
    dup_rows <- duplicated(data[c(entity_var, time_var)]) |
      duplicated(data[c(entity_var, time_var)], fromLast = TRUE)
    if (any(dup_rows)) {
      dup_combinations <- unique(data[
        dup_rows,
        c(entity_var, time_var),
        drop = FALSE
      ])
      n_dup <- nrow(dup_combinations)
      if (!entity_time_from_metadata) {
        examples <- utils::head(dup_combinations, 5)
        example_strings <- paste0(
          examples[[entity_var]],
          "-",
          examples[[time_var]]
        )
        example_str <- paste(example_strings, collapse = ", ")
        message(
          n_dup,
          " duplicate entity-time combinations found. Examples: ",
          example_str
        )
      }
    }
  }

  # --- Validate select, format, digits ---
  if (!is.null(select) && !is.character(select)) {
    stop("'select' must be a character vector or NULL, not ", class(select)[1])
  }
  if (!is.character(format) || length(format) != 1) {
    stop("'format' must be a single character string, not ", class(format)[1])
  }
  if (!format %in% c("wide", "long")) {
    stop('format must be either "wide" or "long", not "', format, '"')
  }
  if (
    !is.numeric(digits) ||
      length(digits) != 1 ||
      digits < 0 ||
      digits != round(digits)
  ) {
    stop("'digits' must be a non-negative integer")
  }
  digits <- as.integer(digits)

  # Helper for rounding
  round_if_needed <- function(x, d) {
    if (is.numeric(x) && !all(is.na(x))) round(x, d) else x
  }

  messages_printed <- FALSE

  # --- Determine variables to analyze ---
  if (is.null(select)) {
    is_factor <- vapply(data, is.factor, FUN.VALUE = logical(1))
    is_factor[entity_var] <- FALSE
    if (!is.null(time_var) && time_var %in% names(data)) {
      is_factor[time_var] <- FALSE
    }
    select <- names(data)[is_factor]

    if (length(select) == 0) {
      stop("no factor variables found in the dataset")
    }
    message("Analyzing all factor variables: ", paste(select, collapse = ", "))
    messages_printed <- TRUE
  }

  # Validate selected variables
  missing_vars <- select[!select %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(
      "the following variables were not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Convert selected variables to factor if needed
  for (var in select) {
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

  # Ensure entity variable is not in select
  if (entity_var %in% select) {
    stop("'select' cannot contain the entity variable '", entity_var, "'")
  }

  # Convert entity to character for consistent handling
  data[[entity_var]] <- as.character(data[[entity_var]])

  # Total number of entities
  count_entities <- length(unique(data[[entity_var]]))

  # Helper function for one variable
  decompose_factor_1 <- function(df, varname, ent_var, format_out, d) {
    complete_cases <- complete.cases(df[[varname]], df[[ent_var]])
    df_clean <- df[complete_cases, , drop = FALSE]

    if (nrow(df_clean) == 0) {
      if (format_out == "wide") {
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

    # Split by entity
    entity_data <- split(df_clean[[varname]], df_clean[[ent_var]])

    between_counts <- sapply(categories, function(cat) {
      sum(sapply(entity_data, function(gd) any(as.character(gd) == cat)))
    })

    within_shares <- sapply(categories, function(cat) {
      entities_with_cat <- which(sapply(entity_data, function(gd) {
        any(as.character(gd) == cat)
      }))
      if (length(entities_with_cat) == 0) {
        return(0)
      }
      group_shares <- sapply(entities_with_cat, function(i) {
        gd <- entity_data[[i]]
        sum(as.character(gd) == cat) / length(gd)
      })
      mean(group_shares)
    })

    share_overall <- as.numeric(overall_counts / total_obs)
    share_between <- as.numeric(between_counts / count_entities)

    share_overall <- round_if_needed(share_overall, d)
    share_between <- round_if_needed(share_between, d)
    within_shares <- round_if_needed(within_shares, d)

    if (format_out == "wide") {
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

  # Apply to each selected variable
  results <- lapply(select, function(v) {
    decompose_factor_1(data, v, entity_var, format, digits)
  })
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  if (format == "long") {
    result_df$category <- factor(
      result_df$category,
      levels = unique(result_df$category)
    )
    result_df <- result_df[order(result_df$variable, result_df$dimension), ]
    rownames(result_df) <- NULL
  }

  # Build metadata and details
  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    select = select,
    entity = entity_var,
    time = time_var,
    format = format,
    digits = digits
  )
  details <- list(count_entities = count_entities)
  if (!is.null(excluded_rows)) {
    details$excluded_rows <- excluded_rows
  }
  if (!is.null(dup_combinations)) {
    details$entity_time_duplicates <- dup_combinations
  }

  attr(result_df, "metadata") <- metadata
  attr(result_df, "details") <- details
  class(result_df) <- c("panel_summary", "data.frame")

  if (
    messages_printed || !is.null(excluded_rows) || !is.null(dup_combinations)
  ) {
    cat("\n")
  }

  return(result_df)
}
