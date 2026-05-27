#' Convert Panel Data from Long to Wide Format
#'
#' This function reshapes panel data from long format to wide format,
#' creating separate columns for each time period.
#'
#' @param data A data.frame containing panel data in a long format.
#' @param index A character vector of length 2 specifying the names of the
#'        entity and time variables. Not required if data has panel attributes.
#' @param spacer A character string to insert between variable names and time
#'        values in the wide format column names. Default = `"_"`.
#' @param invert A logical flag indicating whether to put time values before
#'        variable names in column names. If `FALSE` (default), column names
#'        are `"variable_spacer_time"`; if `TRUE`, they are `"time_spacer_variable"`.
#'
#' @return A data frame in wide format, with one row per entity.
#'
#' @details
#' The function performs the following steps:
#' * If `data` has panel attributes and `index` is not specified, the entity
#'   and time variables are taken from the metadata.
#' * Rows with missing values in entity or time variables are removed.
#' * Duplicate entity‑time combinations are detected and reported (unless they
#'   originate from panel attributes).
#' * The data are reshaped to wide format using `stats::reshape()`.
#'
#' The returned object has class `"panel_data"` and two additional attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name, the entity and time
#'         variables, the `spacer`, and the `invert` setting. If the input was a
#'         `panel_data` object, the original metadata elements (`delta`, etc.)
#'         are preserved.}
#'   \item{`details`}{Preserved from the input if it was a `panel_data` object;
#'         otherwise an empty list.}
#' }
#'
#' @note
#' The function works for standard atomic types (logical, integer, double,
#' complex, character, raw) and for factors. However, non‑standard column types
#' such as `Date`, `POSIXct`, or custom S3/S4 classes may lose their special
#' attributes during reshaping. Duplicate entity-time combinations must be
#' resolved beforehand; the function will issue a message but does not aggregate.
#'
#' @seealso
#' See also [make_panel()], [make_long()], [make_balanced()], [make_demeaned()].
#'
#' @examples
#' data(production)
#'
#' # Basic conversion
#' wide <- make_wide(production, index = c("firm", "year"))
#' head(wide)
#'
#' # With panel_data object
#' panel <- make_panel(production, index = c("firm", "year"))
#' wide2 <- make_wide(panel)
#'
#' # Custom spacer and inverted order
#' wide3 <- make_wide(production, index = c("firm", "year"),
#'                    spacer = ".", invert = TRUE)
#' names(wide3)
#'
#' @export
make_wide <- function(data, index = NULL, spacer = "_", invert = FALSE) {
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
      # Note: reshape will error if duplicates exist; we let it handle.
    }
  }

  # --- Prepare for reshaping ---
  varying_vars <- setdiff(names(data), c(entity_var, time_var))

  if (length(varying_vars) == 0) {
    stop("No variables to reshape (only entity and time found)", call. = FALSE)
  }

  # --- Perform reshape using base R reshape() ---
  # Use a temporary separator that is very unlikely to appear in column names.
  # stats::reshape with direction = "wide" creates names like "var.time".
  # We'll rename afterward.
  wide <- stats::reshape(
    data,
    direction = "wide",
    idvar = entity_var,
    timevar = time_var,
    v.names = varying_vars,
    sep = "__TEMP__"
  )

  # --- Rename columns: replace __TEMP__ with spacer, optionally invert ---
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

  # --- Build metadata attribute ---
  if (keep_panel_class) {
    new_metadata <- panel_metadata
    new_metadata$function_name <- "make_wide"
    new_metadata$spacer <- spacer
    new_metadata$invert <- invert
  } else {
    new_metadata <- list(
      function_name = "make_wide",
      entity = entity_var,
      time = time_var,
      spacer = spacer,
      invert = invert
    )
  }

  # --- Build details attribute (preserve original if any) ---
  if (keep_panel_class && !is.null(panel_details)) {
    new_details <- panel_details
  } else {
    new_details <- list()
  }

  # --- Attach attributes and class ---
  attr(wide, "metadata") <- new_metadata
  attr(wide, "details") <- new_details
  class(wide) <- c("panel_data", class(wide))

  if (msg_printed) {
    cat("\n")
  }
  return(wide)
}
