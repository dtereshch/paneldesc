#' Panel Data Numeric Variable Decomposition
#'
#' This function decomposes variance of numeric variables into between and within components
#' in panel data.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 1 or 2 specifying the names of the
#'        entity and (optionally) time variables. The first element is the entity
#'        variable; if a second element is provided, it is used as the time variable.
#'        Not required if data has panel attributes.
#' @param select A character vector specifying which numeric variables to analyze.
#'        If not specified, all numeric variables in the data.frame will be used.
#' @param format A character string specifying the output format: "long" or "wide".
#'        Default = "long".
#' @param detail A logical flag indicating whether to return detailed Stata-like output.
#'        Default = TRUE.
#' @param digits An integer indicating the number of decimal places to round statistics.
#'        Default = 3.
#'
#' @return A data.frame with panel data decomposition statistics, class `"panel_summary"`.
#'
#' @details
#' The output format is controlled by two parameters: `format` and `detail`.
#'
#' When `format = "long"` and `detail = TRUE` (default), returns a data.frame with:
#' \describe{
#'   \item{\code{variable}}{The name of the analyzed variable}
#'   \item{\code{dimension}}{Type of decomposition: "overall", "between", or "within"}
#'   \item{\code{mean}}{Mean value (only for "overall" row)}
#'   \item{\code{std}}{Standard deviation}
#'   \item{\code{min}}{Minimum value}
#'   \item{\code{max}}{Maximum value}
#'   \item{\code{count}}{Number of observations or entities}
#' }
#'
#' When `format = "long"` and `detail = FALSE`, returns a data.frame with:
#' \describe{
#'   \item{\code{variable}}{The name of the variable}
#'   \item{\code{dimension}}{Type of decomposition: "overall", "between", or "within"}
#'   \item{\code{mean}}{Mean value}
#'   \item{\code{std}}{Standard deviation}
#' }
#'
#' When `format = "wide"` and `detail = TRUE`, returns a data.frame with:
#' \describe{
#'   \item{\code{variable}}{The name of the variable}
#'   \item{\code{mean}}{Overall mean}
#'   \item{\code{std_overall}}{Overall standard deviation}
#'   \item{\code{min_overall}}{Overall minimum}
#'   \item{\code{max_overall}}{Overall maximum}
#'   \item{\code{count_overall}}{Number of observations}
#'   \item{\code{std_between}}{Between-entity standard deviation}
#'   \item{\code{min_between}}{Minimum of entity means}
#'   \item{\code{max_between}}{Maximum of entity means}
#'   \item{\code{count_between}}{Number of entities}
#'   \item{\code{std_within}}{Within-entity standard deviation}
#'   \item{\code{min_within}}{Within-entity minimum (transformed)}
#'   \item{\code{max_within}}{Within-entity maximum (transformed)}
#'   \item{\code{count_within}}{Average observations per entity}
#' }
#'
#' When `format = "wide"` and `detail = FALSE`, returns a data.frame with:
#' \describe{
#'   \item{\code{variable}}{The name of the variable}
#'   \item{\code{mean}}{Overall mean}
#'   \item{\code{std_overall}}{Overall standard deviation}
#'   \item{\code{std_between}}{Between-entity standard deviation}
#'   \item{\code{std_within}}{Within-entity standard deviation}
#' }
#'
#' Before any analysis, rows with missing values (`NA`) in the entity or (if provided)
#' time variables are removed. Messages indicate how many rows were excluded.
#' The excluded rows are stored in `details$excluded_rows` for further inspection.
#'
#' If a time variable is supplied (either via `index` or from panel metadata),
#' the function checks for duplicate entity-time combinations. If duplicates are found,
#' they are stored in `details$entity_time_duplicates`. A message is printed only when
#' the identifiers were explicitly provided (i.e., not taken from panel attributes).
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
#' For Stata users: This corresponds to the `xtsum` command.
#'
#' @seealso
#' [decompose_factor()], [summarize_transition()]
#'
#' @examples
#' data(production)
#' decompose_numeric(production, index = "firm")
#' decompose_numeric(production, index = c("firm", "year"))
#'
#' @export
decompose_numeric <- function(
  data,
  index = NULL,
  select = NULL,
  format = "long",
  detail = TRUE,
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
    entity_from_metadata <- is.null(user_index) && !is.null(metadata$entity)
    time_from_metadata <- is.null(user_index) &&
      !is.null(time_meta) &&
      (length(user_index) < 2 || is.null(user_index[2]))
    entity_time_from_metadata <- entity_from_metadata &&
      (is.null(time_var) || time_from_metadata)
  } else {
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

  # --- Validate existence ---
  if (!entity_var %in% names(data)) {
    stop('entity variable "', entity_var, '" not found in data')
  }
  if (!is.null(time_var) && !time_var %in% names(data)) {
    stop('time variable "', time_var, '" not found in data')
  }
  if (!is.null(time_var) && time_var == entity_var) {
    stop("entity and time variables cannot be the same")
  }

  # --- Remove rows with NA in entity or time ---
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

  # --- Duplicate check (if time provided) ---
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

  # --- Validate parameters ---
  if (!is.null(select) && !is.character(select)) {
    stop("'select' must be a character vector or NULL, not ", class(select)[1])
  }
  if (!is.logical(detail) || length(detail) != 1) {
    stop("'detail' must be a single logical value")
  }
  if (!is.character(format) || length(format) != 1) {
    stop("'format' must be a single character string")
  }
  if (!format %in% c("long", "wide")) {
    stop('format must be either "long" or "wide"')
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

  round_if_needed <- function(x, d) {
    if (is.numeric(x) && !all(is.na(x))) round(x, d) else x
  }

  messages_printed <- FALSE

  # --- Determine numeric variables ---
  if (is.null(select)) {
    numeric_vars <- vapply(data, is.numeric, FUN.VALUE = logical(1))
    select <- names(data)[numeric_vars]
    # Remove entity and time if they appear
    select <- setdiff(select, c(entity_var, time_var))
    if (length(select) == 0) {
      stop("no numeric variables found in the dataset")
    }
    message("Analyzing all numeric variables: ", paste(select, collapse = ", "))
    messages_printed <- TRUE
  } else {
    missing_vars <- select[!select %in% names(data)]
    if (length(missing_vars) > 0) {
      stop("variables not found: ", paste(missing_vars, collapse = ", "))
    }
    non_num <- select[!vapply(data[select], is.numeric, FUN.VALUE = logical(1))]
    if (length(non_num) > 0) {
      stop(
        "the following variables are not numeric: ",
        paste(non_num, collapse = ", ")
      )
    }
    if (entity_var %in% select) {
      stop("'select' cannot contain the entity variable '", entity_var, "'")
    }
    if (!is.null(time_var) && time_var %in% select) {
      stop("'select' cannot contain the time variable '", time_var, "'")
    }
  }

  # Total number of entities
  count_entities <- length(unique(data[[entity_var]]))
  if (count_entities > 10000) {
    warning(
      "Large number of entities (",
      count_entities,
      "). May impact performance."
    )
  }

  # Helper for one variable
  decompose_numeric_1 <- function(
    df,
    varname,
    ent_var,
    format_out,
    detail_out,
    d
  ) {
    complete_cases <- complete.cases(df[[varname]], df[[ent_var]])
    df_clean <- df[complete_cases, , drop = FALSE]

    if (nrow(df_clean) == 0) {
      if (format_out == "long") {
        if (detail_out) {
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
        } else {
          return(data.frame(
            variable = character(),
            dimension = character(),
            mean = numeric(),
            std = numeric(),
            stringsAsFactors = FALSE
          ))
        }
      } else {
        if (detail_out) {
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
            count_within = NA_real_,
            stringsAsFactors = FALSE
          ))
        } else {
          return(data.frame(
            variable = varname,
            mean = NA_real_,
            std_overall = NA_real_,
            std_between = NA_real_,
            std_within = NA_real_,
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    ent_vec <- as.character(df_clean[[ent_var]])
    x <- df_clean[[varname]]

    overall_mean <- mean(x, na.rm = TRUE)
    overall_std <- sd(x, na.rm = TRUE)
    min_val <- min(x, na.rm = TRUE)
    max_val <- max(x, na.rm = TRUE)
    count_obs <- length(x)

    ent_means <- tapply(x, ent_vec, mean, na.rm = TRUE)
    between_std <- sd(ent_means, na.rm = TRUE)
    between_min <- min(ent_means, na.rm = TRUE)
    between_max <- max(ent_means, na.rm = TRUE)
    count_ent <- length(ent_means)

    # Within transformation
    ent_means_exp <- ent_means[match(ent_vec, names(ent_means))]
    deviations <- x - ent_means_exp
    within_transformed <- deviations + overall_mean
    within_std <- sd(deviations, na.rm = TRUE)
    within_min <- min(within_transformed, na.rm = TRUE)
    within_max <- max(within_transformed, na.rm = TRUE)

    obs_per_ent <- table(ent_vec)
    avg_obs <- mean(obs_per_ent, na.rm = TRUE)

    # Rounding
    overall_mean <- round_if_needed(overall_mean, d)
    overall_std <- round_if_needed(overall_std, d)
    min_val <- round_if_needed(min_val, d)
    max_val <- round_if_needed(max_val, d)
    between_std <- round_if_needed(between_std, d)
    between_min <- round_if_needed(between_min, d)
    between_max <- round_if_needed(between_max, d)
    within_std <- round_if_needed(within_std, d)
    within_min <- round_if_needed(within_min, d)
    within_max <- round_if_needed(within_max, d)
    avg_obs <- round_if_needed(avg_obs, d)

    if (format_out == "long") {
      if (detail_out) {
        result <- data.frame(
          variable = c(varname, varname, varname),
          dimension = c("overall", "between", "within"),
          mean = c(overall_mean, NA, NA),
          std = c(overall_std, between_std, within_std),
          min = c(min_val, between_min, within_min),
          max = c(max_val, between_max, within_max),
          count = c(count_obs, count_ent, avg_obs),
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
      if (detail_out) {
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
          count_between = count_ent,
          std_within = within_std,
          min_within = within_min,
          max_within = within_max,
          count_within = avg_obs,
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

  results <- lapply(select, function(v) {
    decompose_numeric_1(data, v, entity_var, format, detail, digits)
  })
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  metadata <- list(
    function_name = as.character(match.call()[[1]]),
    select = select,
    entity = entity_var,
    time = time_var,
    format = format,
    detail = detail,
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
