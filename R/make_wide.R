#' Convert Panel Data from Long to Wide Format
#'
#' This function reshapes panel data from long format to wide format,
#' creating separate columns for each time period.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the
#'        entity and time variables.
#'        If not specified and data is a `panel_data` object, the entity and time values
#'        will be extracted from the data.frame attributes.
#' @param static A character vector of variable names that are time-invariant.
#'        If specified, the function verifies invariance and excludes these
#'        variables from reshaping; they appear as single columns in the wide
#'        output.
#' @param spacer A character string to insert between variable names and time
#'        values in the wide format column names. Default = "_".
#' @param invert A logical flag indicating whether to put time values before
#'        variable names in column names. If `FALSE`, column names are
#'        `"variable_spacer_time"`; if `TRUE`, they are `"time_spacer_variable"`.
#'        Default = FALSE.
#'
#' @return A data.frame containing panel data in a wide format.
#'
#' @details
#' The data are reshaped to long format using `stats::reshape()`.
#'
#' The function works for standard atomic types (logical, integer, double,
#' complex, character, raw) and for factors. However, non-standard column types
#' such as `Date`, `POSIXct`, or custom S3/S4 classes may lose their special
#' attributes during reshaping.
#'
#' The returned object has class `"panel_data"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.
#'         If the input was a `panel_data` object, the original metadata elements
#'         (entity, time, and delta) are preserved.}
#'   \item{`details`}{List containing the names of reshaped variables and detected static variables.}
#' }
#'
#' @seealso
#' See also [make_panel()], [make_long()], [make_balanced()].
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' wide <- make_wide(production, index = c("firm", "year"))
#'
#' # With panel_data object
#' panel <- make_panel(production, index = c("firm", "year"))
#' wide2 <- make_wide(panel)
#'
#' # Custom spacer and inverted order
#' wide3 <- make_wide(production, index = c("firm", "year"),
#'                    spacer = ".", invert = TRUE)
#'
#' # Accessing attributes
#' attr(wide3, "metadata")
#' attr(wide3, "details")
#'
#' @importFrom utils head
#' @importFrom stats reshape
#' @export
make_wide <- function(
  data,
  index = NULL,
  static = NULL,
  spacer = "_",
  invert = FALSE
) {
  # --- Input validation ---
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1], call. = FALSE)
  }
  if (!is.character(spacer) || length(spacer) != 1) {
    stop("'spacer' must be a single character string", call. = FALSE)
  }
  if (!is.logical(invert) || length(invert) != 1) {
    stop("'invert' must be a single logical value", call. = FALSE)
  }

  msg_printed <- FALSE
  entity_var <- NULL
  time_var <- NULL
  keep_panel_class <- FALSE
  panel_metadata <- NULL
  panel_details <- NULL
  entity_time_from_metadata <- FALSE

  # --- Extract index from metadata if applicable ---
  if (inherits(data, "panel_data")) {
    meta <- attr(data, "metadata")
    if (!is.null(meta) && !is.null(meta$entity) && !is.null(meta$time)) {
      if (is.null(index)) {
        entity_var <- meta$entity
        time_var <- meta$time
        keep_panel_class <- TRUE
        panel_metadata <- meta
        panel_details <- attr(data, "details")
        entity_time_from_metadata <- TRUE
      } else {
        # User provided index overrides
        if (length(index) != 2 || !is.character(index)) {
          stop("'index' must be a character vector of length 2", call. = FALSE)
        }
        entity_var <- index[1]
        time_var <- index[2]
      }
    } else {
      # No valid metadata, fall back to user index
      if (is.null(index)) {
        stop("For regular data.frames, 'index' must be provided", call. = FALSE)
      }
      if (length(index) != 2 || !is.character(index)) {
        stop("'index' must be a character vector of length 2", call. = FALSE)
      }
      entity_var <- index[1]
      time_var <- index[2]
    }
  } else {
    # Regular data frame
    if (is.null(index)) {
      stop("For regular data.frames, 'index' must be provided", call. = FALSE)
    }
    if (length(index) != 2 || !is.character(index)) {
      stop("'index' must be a character vector of length 2", call. = FALSE)
    }
    entity_var <- index[1]
    time_var <- index[2]
  }

  # --- Validate existence ---
  if (!entity_var %in% names(data)) {
    stop('entity variable "', entity_var, '" not found in data', call. = FALSE)
  }
  if (!time_var %in% names(data)) {
    stop('time variable "', time_var, '" not found in data', call. = FALSE)
  }
  if (time_var == entity_var) {
    stop("entity and time variables cannot be the same", call. = FALSE)
  }

  # --- Remove rows with NA in entity or time ---
  na_entity <- is.na(data[[entity_var]])
  na_time <- is.na(data[[time_var]])
  if (any(na_entity)) {
    message(
      sum(na_entity),
      " rows with missing values in '",
      entity_var,
      "' variable found and excluded."
    )
    msg_printed <- TRUE
  }
  if (any(na_time)) {
    message(
      sum(na_time),
      " rows with missing values in '",
      time_var,
      "' variable found and excluded."
    )
    msg_printed <- TRUE
  }
  if (any(na_entity | na_time)) {
    data <- data[!(na_entity | na_time), , drop = FALSE]
    rownames(data) <- NULL
  }

  # --- Duplicate check (unless from metadata) ---
  if (!entity_time_from_metadata) {
    dup_rows <- duplicated(data[c(entity_var, time_var)]) |
      duplicated(data[c(entity_var, time_var)], fromLast = TRUE)
    if (any(dup_rows)) {
      dup_combinations <- unique(data[
        dup_rows,
        c(entity_var, time_var),
        drop = FALSE
      ])
      n_dup <- nrow(dup_combinations)
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
      msg_printed <- TRUE
    }
  }

  # --- Helper function to check time-invariance (ignoring NAs) ---
  check_invariant <- function(var, data, entity_var) {
    vals <- data[[var]]
    entities <- data[[entity_var]]
    # For each entity, get unique non-NA values; if any entity has >1 unique, not invariant
    uniq_per_entity <- tapply(vals, entities, function(x) unique(x[!is.na(x)]))
    any_multiple <- any(sapply(uniq_per_entity, length) > 1)
    return(!any_multiple)
  }

  # --- Validate and process 'static' ---
  if (!is.null(static)) {
    if (!is.character(static)) {
      stop("'static' must be a character vector", call. = FALSE)
    }
    if (any(static %in% c(entity_var, time_var))) {
      stop("Static variables cannot be entity or time variables", call. = FALSE)
    }
    missing_static <- setdiff(static, names(data))
    if (length(missing_static) > 0) {
      stop(
        "Static variable(s) not found in data: ",
        paste(missing_static, collapse = ", "),
        call. = FALSE
      )
    }
    # Check invariance for each static variable
    invariant_check <- sapply(static, function(v) {
      check_invariant(v, data, entity_var)
    })
    if (!all(invariant_check)) {
      non_invariant <- static[!invariant_check]
      stop(
        "The following static variables are not time-invariant: ",
        paste(non_invariant, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # --- Detect all time-invariant variables (for details) ---
  all_vars <- setdiff(names(data), c(entity_var, time_var))
  invariant_vars <- all_vars[sapply(
    all_vars,
    check_invariant,
    data = data,
    entity_var = entity_var
  )]

  # --- Detect and report time-invariant variables (user messages) ---
  if (is.null(static)) {
    if (length(invariant_vars) > 0) {
      message(
        "Time-invariant variables detected: ",
        paste(invariant_vars, collapse = ", "),
        ". Consider using 'static' argument."
      )
      msg_printed <- TRUE
    }
  } else {
    # Check variables not in static
    other_vars <- setdiff(all_vars, static)
    additional <- other_vars[sapply(
      other_vars,
      check_invariant,
      data = data,
      entity_var = entity_var
    )]
    if (length(additional) > 0) {
      message(
        "Additional time-invariant variables detected: ",
        paste(additional, collapse = ", "),
        ". Consider updating 'static' argument."
      )
      msg_printed <- TRUE
    }
  }

  # --- Prepare data for reshaping ---
  # If static is provided, we extract those columns (one row per entity) and
  # drop them from the data frame that goes into reshape().
  if (!is.null(static)) {
    # Get unique entity-static pairs (first occurrence per entity)
    static_data <- data[
      !duplicated(data[[entity_var]]),
      c(entity_var, static),
      drop = FALSE
    ]
    # Remove static columns from the data passed to reshape
    data_for_reshape <- data[, setdiff(names(data), static), drop = FALSE]
  } else {
    data_for_reshape <- data
  }

  # Identify varying variables in the reduced data (all except entity and time)
  varying_vars <- setdiff(names(data_for_reshape), c(entity_var, time_var))

  # --- Perform reshape ---
  if (length(varying_vars) == 0) {
    # No time-varying variables
    if (is.null(static)) {
      stop(
        "No variables to reshape (only entity and time found)",
        call. = FALSE
      )
    } else {
      # Only static columns exist; return entity + static
      wide <- static_data
      rownames(wide) <- NULL
    }
  } else {
    # Reshape the reduced data
    wide <- stats::reshape(
      data_for_reshape,
      direction = "wide",
      idvar = entity_var,
      timevar = time_var,
      v.names = varying_vars,
      sep = "__TEMP__"
    )
    # Rename columns: replace __TEMP__ with spacer, optionally invert
    new_names <- names(wide)
    for (i in seq_along(new_names)) {
      nm <- new_names[i]
      if (grepl("__TEMP__", nm)) {
        parts <- strsplit(nm, "__TEMP__")[[1]]
        var_part <- parts[1]
        time_part <- parts[2]
        if (invert) {
          new_names[i] <- paste0(time_part, spacer, var_part)
        } else {
          new_names[i] <- paste0(var_part, spacer, time_part)
        }
      }
    }
    names(wide) <- new_names
    rownames(wide) <- NULL

    # If static columns were provided, merge them back
    if (!is.null(static)) {
      wide <- merge(wide, static_data, by = entity_var, all.x = TRUE)
      # Reorder columns: entity first, then time-varying, then static
      time_varying_cols <- setdiff(names(wide), c(entity_var, static))
      wide <- wide[, c(entity_var, time_varying_cols, static), drop = FALSE]
    }
  }

  # --- Build metadata attribute ---
  if (keep_panel_class) {
    new_metadata <- panel_metadata
    new_metadata$function_name <- "make_wide"
    new_metadata$spacer <- spacer
    new_metadata$invert <- invert
    if (!is.null(static)) new_metadata$static <- static
  } else {
    new_metadata <- list(
      function_name = "make_wide",
      entity = entity_var,
      time = time_var,
      spacer = spacer,
      invert = invert
    )
    if (!is.null(static)) new_metadata$static <- static
  }

  # --- Build details from scratch (do NOT preserve input details) ---
  # reshaped = variables that were actually reshaped (time-varying)
  # static_detected = all variables that are time-invariant (whether provided by user or auto-detected)
  new_details <- list(
    reshaped = varying_vars,
    static_detected = invariant_vars
  )

  # --- Attach attributes and class ---
  attr(wide, "metadata") <- new_metadata
  attr(wide, "details") <- new_details
  class(wide) <- c("panel_data", class(wide))

  if (msg_printed) {
    cat("\n")
  }
  return(wide)
}
