#' Convert Panel Data from Wide to Long Format
#'
#' This function reshapes panel data from wide format to long format,
#' stacking time-varying columns into rows based on the pattern of column names.
#'
#' @param data A data.frame containing panel data in a wide format (e.g., created
#'        by `make_wide()`).
#' @param index A character vector of length 2 specifying the name of the
#'        entity column (first element) and the name to give to the new time
#'        column in the long format (second element). Not required if data has
#'        panel attributes (e.g., from `make_wide()`).
#' @param spacer A character string used to separate variable names and time
#'        values in the wide column names. Default = `"_"`.
#' @param invert A logical flag indicating the order of components in column
#'        names. If `FALSE` (default), column names are
#'        `"variable_spacer_time"`; if `TRUE`, they are
#'        `"time_spacer_variable"`. Must match the structure of the input data.
#'
#' @return A data frame in long format, with one row per entity-time combination.
#'
#' @details
#' The function performs the following steps:
#' * If `data` has panel attributes (e.g., from `make_wide()`) and `index` is
#'   not specified, the entity column, time column name, `spacer`, and `invert`
#'   are taken from the metadata.
#' * Columns that do not contain the `spacer` (e.g., entity identifier,
#'   time‑constant covariates) are treated as time‑constant and are replicated
#'   for each time period.
#' * Columns that contain the `spacer` are split into variable names and time
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
#' The function assumes that all time-varying columns follow a consistent naming
#' pattern and that every variable appears for exactly the same set of time
#' periods (balanced in the wide sense). If some variable‑time combinations are
#' missing, a message is printed and those variables are omitted. The entity
#' column is never treated as time‑varying.
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
      # Try to obtain entity and time column names
      if (is.null(index)) {
        if (!is.null(meta$entity) && !is.null(meta$time)) {
          entity_col <- meta$entity
          time_col <- meta$time
        } else if (!is.null(meta$entity) && is.null(meta$time)) {
          # In wide format, meta$time might not be a column name but the time variable label
          # We'll need to create a time column name; use "time" as default
          entity_col <- meta$entity
          time_col <- "time"
        } else {
          stop(
            "Panel metadata missing required 'entity' information",
            call. = FALSE
          )
        }
        # Also recover spacer and invert if present (from make_wide)
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
        # User provided index, override
        if (length(index) != 2 || !is.character(index)) {
          stop("'index' must be a character vector of length 2", call. = FALSE)
        }
        entity_col <- index[1]
        time_col <- index[2]
      }
    } else {
      # No metadata, treat as regular data frame
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
    # Regular data frame
    if (is.null(index)) {
      stop("For regular data.frames, 'index' must be provided", call. = FALSE)
    }
    if (length(index) != 2 || !is.character(index)) {
      stop("'index' must be a character vector of length 2", call. = FALSE)
    }
    entity_col <- index[1]
    time_col <- index[2]
  }

  # Override spacer/invert with metadata values if they exist and not overridden by arguments?
  # User arguments take precedence over metadata? Usually arguments should override.
  # But if user did not specify spacer/invert and metadata has them, use metadata.
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
  # All columns except entity_col that contain spacer
  all_cols <- names(data)
  varying_candidates <- setdiff(all_cols, entity_col)

  # Split into variable groups
  # We need: for each underlying variable, a vector of column names (one per time period)
  # Also extract unique time values.

  # Parse each column name to extract base variable name and time value
  parsed <- list()
  time_values <- c()
  for (col in varying_candidates) {
    if (!grepl(spacer, col, fixed = TRUE)) {
      # Time-constant column, skip for now
      next
    }
    parts <- strsplit(col, spacer, fixed = TRUE)[[1]]
    if (invert) {
      # pattern: time_spacer_variable
      if (length(parts) < 2) {
        next
      }
      time_val <- parts[1]
      var_name <- paste(parts[-1], collapse = spacer)
    } else {
      # pattern: variable_spacer_time
      if (length(parts) < 2) {
        next
      }
      var_name <- paste(parts[-length(parts)], collapse = spacer)
      time_val <- parts[length(parts)]
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

  # Unique time values, preserve original order as much as possible (e.g., numeric sort)
  unique_times <- unique(time_values)
  # Try to sort numerically if all look like numbers
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

  # For each variable, ensure that we have exactly the same set of times
  # (fill missing with NA if not present)
  # Build the 'varying' argument for reshape: a list where each element is a vector
  # of column names for one variable, ordered by time.
  varying_list <- list()
  v.names <- c()
  for (var in names(var_to_cols)) {
    cols <- var_to_cols[[var]]
    # Map time to column
    time_to_col <- sapply(cols, function(c) parsed[[c]]$time, USE.NAMES = FALSE)
    names(time_to_col) <- cols
    # Order by unique_times
    ordered_cols <- sapply(unique_times, function(t) {
      match_col <- names(time_to_col)[time_to_col == t]
      if (length(match_col) == 0) NA else match_col[1]
    })
    if (any(is.na(ordered_cols))) {
      # Some times missing for this variable; we will have NAs after reshape
      # We can still proceed; reshape will handle NA columns? It expects all columns present.
      # Safer to fill with a placeholder column? Or we can add NA column? Easier: let reshape fail?
      # Instead, we can create a full matrix of columns by adding dummy NA columns.
      # But that's messy. We'll assume that wide data is balanced (all variables observed at all times).
      # If not, we could still use reshape by creating a list with NAs? reshape doesn't allow.
      # So we issue a warning.
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

  # Convert varying_list to a list of character vectors, dropping NAs? Actually reshape requires
  # that each element of varying list has same length. We must keep NAs as column names?
  # If we have missing column, we can't pass NA. Best to ensure all variables have all times.
  # To be safe, we'll keep only variables that have all times.
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

  # Prepare varying argument as a list of column name vectors
  varying <- lapply(varying_list, function(x) as.character(x))

  # Time-constant columns: all columns not in entity_col and not appearing in any varying column
  constant_cols <- setdiff(all_cols, c(entity_col, unlist(varying)))

  # --- Reshape using base R reshape() ---
  # We need to create a temporary data frame with only the columns we need.
  # reshape() requires that all varying columns exist.
  # We'll pass the full data, but specify varying and v.names.
  wide_subset <- data[,
    c(entity_col, constant_cols, unlist(varying)),
    drop = FALSE
  ]

  # Note: reshape with direction = "long" will create a new column named "time" by default,
  # but we can rename it. We'll use timevar = time_col.
  # It also adds a column "id" (the row index) if we don't provide it? Actually it creates
  # a column named after timevar and the row names become the original ids.
  # We'll set ids to NULL to avoid extra column.

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

  # Clean up: remove the automatically added row index column if present (reshape adds "row.names" as column? No)
  # Reset row names
  rownames(long) <- NULL

  # Ensure the time column has appropriate type (convert to numeric if possible)
  if (all(grepl("^-?[0-9.]+$", long[[time_col]]))) {
    long[[time_col]] <- as.numeric(long[[time_col]])
  }

  # Reorder columns: entity, time, then the original variable columns, then constants
  var_cols <- v.names
  const_cols <- intersect(constant_cols, names(long))
  long <- long[, c(entity_col, time_col, var_cols, const_cols), drop = FALSE]

  # --- Build metadata attribute ---
  if (keep_panel_class) {
    # Preserve original panel metadata and add conversion info
    new_metadata <- panel_metadata
    new_metadata$function_name <- "make_long"
    new_metadata$spacer <- spacer
    new_metadata$invert <- invert
    # Update entity and time (the time column name)
    new_metadata$entity <- entity_col
    new_metadata$time <- time_col
    # Keep delta if present
  } else {
    new_metadata <- list(
      function_name = "make_long",
      entity = entity_col,
      time = time_col,
      spacer = spacer,
      invert = invert
    )
  }

  # Build details attribute (preserve original if any)
  if (keep_panel_class && !is.null(panel_details)) {
    new_details <- panel_details
  } else {
    new_details <- list()
  }

  # --- Attach attributes and class ---
  attr(long, "metadata") <- new_metadata
  attr(long, "details") <- new_details
  class(long) <- c("panel_data", class(long))

  if (msg_printed) {
    cat("\n")
  }
  return(long)
}
