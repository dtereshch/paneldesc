#' Convert Panel Data from Wide to Long Format
#'
#' This function reshapes panel data from wide format to long format,
#' stacking time-varying columns into rows based on the pattern of column names.
#'
#' @param data A data.frame containing panel data in a wide format.
#' @param index A character vector of length 2 specifying the name of the
#'        entity column (first element) and the name to give to the new time
#'        column in the long format (second element).
#' @param spacer A character string used to separate variable names and time
#'        values in the wide column names. Default = "_".
#' @param invert A logical flag indicating the order of components in column
#'        names. If `FALSE` (default), column names are
#'        `"variable_spacer_time"` (or `"variable"` + `time` when `spacer = ""`);
#'        if `TRUE`, they are `"time_spacer_variable"` (or `time` + `"variable"`
#'        when `spacer = ""`). Must match the structure of the input data.
#'
#' @return A data frame in long format, with one row per entity-time combination.
#'
#' @details
#' The function performs the following steps:
#' * If `data` has panel attributes (e.g., from `make_wide()`) and `index` is
#'   not specified, the entity column, time column name, `spacer`, and `invert`
#'   are taken from the metadata.
#' * Columns that do not contain the `spacer` (or do not match the expected
#'   pattern when `spacer = ""`) are treated as time‑constant and are replicated
#'   for each time period.
#' * Columns that match the pattern are split into variable names and time
#'   values; the set of unique time values defines the periods.
#' * The data are reshaped to long format using `stats::reshape()`.
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
#' When `spacer = ""`, the function assumes that all time‑varying columns have
#' a numeric suffix (if `invert = FALSE`) or numeric prefix (if `invert = TRUE`)
#' that represents the time period. Variable names may contain digits, but the
#' last contiguous block of digits is treated as the time suffix; for prefixes,
#' the first contiguous block of digits is treated as the time value. If a
#' column does not contain any digit, it is considered time‑constant.
#'
#' The function assumes that all time-varying columns follow a consistent naming
#' pattern and that every variable appears for exactly the same set of time
#' periods (balanced in the wide sense). If some variable‑time combinations are
#' missing, a message is printed and those variables are omitted.
#'
#' @seealso
#' See also [make_panel()], [make_wide()], [make_balanced()], [make_demeaned()].
#'
#' @examples
#' data(production)
#'
#' # First convert to wide, then back to long
#' wide <- make_wide(production, index = c("firm", "year"))
#' long <- make_long(wide)
#' head(long)
#'
#' # With custom spacer and invert
#' wide2 <- make_wide(production, index = c("firm", "year"), spacer = ".", invert = TRUE)
#' long2 <- make_long(wide2, spacer = ".", invert = TRUE)
#'
#' # Using panel attributes (no need to specify index/spacer/invert)
#' panel <- make_panel(production, index = c("firm", "year"))
#' wide3 <- make_wide(panel)
#' long3 <- make_long(wide3)
#'
#' # Using spacer = "" (no separator)
#' wide4 <- make_wide(production, index = c("firm", "year"), spacer = "")
#' long4 <- make_long(wide4, spacer = "")
#'
#' @export
make_long <- function(data, index = NULL, spacer = "_", invert = FALSE) {
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
  entity_col <- NULL
  time_col <- NULL
  keep_panel_class <- FALSE
  panel_metadata <- NULL
  panel_details <- NULL
  metadata_spacer <- NULL
  metadata_invert <- NULL

  # --- Extract metadata if data has panel attributes ---
  if (inherits(data, "panel_data")) {
    meta <- attr(data, "metadata")
    if (!is.null(meta)) {
      if (is.null(index)) {
        if (!is.null(meta$entity) && !is.null(meta$time)) {
          entity_col <- meta$entity
          time_col <- meta$time
        } else if (!is.null(meta$entity) && is.null(meta$time)) {
          entity_col <- meta$entity
          time_col <- "time"
        } else {
          stop(
            "Panel metadata missing required 'entity' information",
            call. = FALSE
          )
        }
        if (!is.null(meta$spacer)) {
          metadata_spacer <- meta$spacer
        }
        if (!is.null(meta$invert)) {
          metadata_invert <- meta$invert
        }
        keep_panel_class <- TRUE
        panel_metadata <- meta
        panel_details <- attr(data, "details")
      } else {
        if (length(index) != 2 || !is.character(index)) {
          stop("'index' must be a character vector of length 2", call. = FALSE)
        }
        entity_col <- index[1]
        time_col <- index[2]
      }
    } else {
      if (is.null(index)) {
        stop("For regular data.frames, 'index' must be provided", call. = FALSE)
      }
      if (length(index) != 2 || !is.character(index)) {
        stop("'index' must be a character vector of length 2", call. = FALSE)
      }
      entity_col <- index[1]
      time_col <- index[2]
    }
  } else {
    if (is.null(index)) {
      stop("For regular data.frames, 'index' must be provided", call. = FALSE)
    }
    if (length(index) != 2 || !is.character(index)) {
      stop("'index' must be a character vector of length 2", call. = FALSE)
    }
    entity_col <- index[1]
    time_col <- index[2]
  }

  # Override spacer/invert if not specified by user but present in metadata
  if (missing(spacer) && !is.null(metadata_spacer)) {
    spacer <- metadata_spacer
  }
  if (missing(invert) && !is.null(metadata_invert)) {
    invert <- metadata_invert
  }

  # --- Validate entity column ---
  if (!entity_col %in% names(data)) {
    stop('entity column "', entity_col, '" not found in data', call. = FALSE)
  }

  # --- Identify time-varying columns ---
  all_cols <- names(data)
  varying_candidates <- setdiff(all_cols, entity_col)

  # Parse column names to extract variable name and time value
  parsed <- list()
  time_values <- c()

  for (col in varying_candidates) {
    if (spacer == "") {
      # No separator: use regular expressions
      if (invert) {
        # Pattern: time (numeric prefix) + variable (rest)
        match <- regexpr("^\\d+", col)
        if (match == -1) {
          # No leading digits -> treat as constant column
          next
        }
        time_val <- regmatches(col, match)
        var_name <- substr(col, match + attr(match, "match.length"), nchar(col))
        if (var_name == "") var_name <- "value"
      } else {
        # Pattern: variable + numeric time suffix
        match <- regexpr("\\d+$", col)
        if (match == -1) {
          next
        }
        time_val <- regmatches(col, match)
        var_name <- substr(col, 1, match - 1)
        if (var_name == "") var_name <- "value"
      }
    } else {
      # Normal separator-based splitting
      if (!grepl(spacer, col, fixed = TRUE)) {
        next
      }
      parts <- strsplit(col, spacer, fixed = TRUE)[[1]]
      if (invert) {
        if (length(parts) < 2) {
          next
        }
        time_val <- parts[1]
        var_name <- paste(parts[-1], collapse = spacer)
      } else {
        if (length(parts) < 2) {
          next
        }
        var_name <- paste(parts[-length(parts)], collapse = spacer)
        time_val <- parts[length(parts)]
      }
    }
    parsed[[col]] <- list(var = var_name, time = time_val)
    time_values <- c(time_values, time_val)
  }

  if (length(parsed) == 0) {
    stop(
      "No time-varying columns found. Check 'spacer' and 'invert' settings.",
      call. = FALSE
    )
  }

  # Unique time values, try numeric sort if possible
  unique_times <- unique(time_values)
  if (all(grepl("^-?[0-9.]+$", unique_times))) {
    unique_times <- as.character(sort(as.numeric(unique_times)))
  } else {
    unique_times <- sort(unique_times)
  }

  # Group columns by variable name
  var_to_cols <- list()
  for (col in names(parsed)) {
    var <- parsed[[col]]$var
    var_to_cols[[var]] <- c(var_to_cols[[var]], col)
  }

  # Build varying list
  varying_list <- list()
  v.names <- c()
  for (var in names(var_to_cols)) {
    cols <- var_to_cols[[var]]
    time_to_col <- sapply(cols, function(c) parsed[[c]]$time, USE.NAMES = FALSE)
    names(time_to_col) <- cols
    ordered_cols <- sapply(unique_times, function(t) {
      match_col <- names(time_to_col)[time_to_col == t]
      if (length(match_col) == 0) NA else match_col[1]
    })
    if (any(is.na(ordered_cols))) {
      warning(
        "Variable '",
        var,
        "' is missing for some time periods. ",
        "Resulting long data will have NA for those entries.",
        call. = FALSE
      )
    }
    varying_list[[var]] <- ordered_cols
    v.names <- c(v.names, var)
  }

  # Keep only variables that have complete time coverage
  complete_vars <- sapply(varying_list, function(x) !any(is.na(x)))
  if (any(!complete_vars)) {
    incomplete <- names(varying_list)[!complete_vars]
    message(
      "Variables missing some time periods (will be omitted from reshape): ",
      paste(incomplete, collapse = ", ")
    )
    msg_printed <- TRUE
    varying_list <- varying_list[complete_vars]
    v.names <- v.names[complete_vars]
  }

  if (length(varying_list) == 0) {
    stop("No complete time-varying variables found", call. = FALSE)
  }

  varying <- lapply(varying_list, function(x) as.character(x))
  constant_cols <- setdiff(all_cols, c(entity_col, unlist(varying)))

  # --- Reshape using stats::reshape ---
  wide_subset <- data[,
    c(entity_col, constant_cols, unlist(varying)),
    drop = FALSE
  ]

  long <- stats::reshape(
    wide_subset,
    direction = "long",
    varying = varying,
    v.names = v.names,
    timevar = time_col,
    idvar = entity_col,
    times = unique_times,
    new.row.names = NULL
  )

  rownames(long) <- NULL

  # Try to convert time column to numeric if possible
  if (all(grepl("^-?[0-9.]+$", long[[time_col]]))) {
    long[[time_col]] <- as.numeric(long[[time_col]])
  }

  # Reorder columns
  var_cols <- v.names
  const_cols <- intersect(constant_cols, names(long))
  long <- long[, c(entity_col, time_col, var_cols, const_cols), drop = FALSE]

  # --- Build metadata ---
  if (keep_panel_class) {
    new_metadata <- panel_metadata
    new_metadata$function_name <- "make_long"
    new_metadata$spacer <- spacer
    new_metadata$invert <- invert
    new_metadata$entity <- entity_col
    new_metadata$time <- time_col
  } else {
    new_metadata <- list(
      function_name = "make_long",
      entity = entity_col,
      time = time_col,
      spacer = spacer,
      invert = invert
    )
  }

  if (keep_panel_class && !is.null(panel_details)) {
    new_details <- panel_details
  } else {
    new_details <- list()
  }

  attr(long, "metadata") <- new_metadata
  attr(long, "details") <- new_details
  class(long) <- c("panel_data", class(long))

  if (msg_printed) {
    cat("\n")
  }
  return(long)
}
