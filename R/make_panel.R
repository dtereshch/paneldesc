#' Panel Data Structure Setting and Balancing
#'
#' This function adds panel structure attributes to a data.frame, storing entity/group and time variable names,
#' and optionally checks the expected interval between time periods. It can also balance the panel by
#' keeping only entities present in all periods, keeping only periods where all entities are present,
#' or creating all entity‑time combinations (filling missing combinations with `NA`).
#'
#' @param data A data.frame containing panel data in a long format.
#' @param group A character string specifying the name of the entity/group variable.
#' @param time A character string specifying the name of the time variable.
#' @param interval An optional positive integer giving the expected interval between time periods.
#' @param balance One of `NULL` (default), `"entities"`, `"periods"`, or `"all"`. If not `NULL`,
#'        the panel is balanced according to the chosen method (see Details).
#'
#' @return The input data.frame with additional attributes, after possibly filtering or expanding rows.
#'
#' @details
#' This function adds attributes to a data.frame to mark it as panel data.
#' The returned object has class `"panel_data"` (in addition to its original class).
#' It includes the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used
#'         (`group`, `time`, `interval`, and `balance` if provided).}
#'   \item{`details`}{List with diagnostic vectors. By default it contains:
#'         \describe{
#'           \item{`entities`}{Unique values of the group variable.}
#'           \item{`periods`}{Sorted unique values of the time variable.}
#'         }
#'         If any rows were removed due to missing values in `group` or `time`, an additional component is included:
#'         \describe{
#'           \item{`excluded_rows`}{A data frame containing the rows that were removed because they
#'                 contained `NA` in either the group or time variable.}
#'         }
#'         If duplicates of the group-time combination are found, an additional vector is included:
#'         \describe{
#'           \item{`entity_time_duplicates`}{A data frame containing the distinct duplicate
#'                 combinations of group and time.}
#'         }
#'         If `interval` is supplied and missing periods are detected, two additional vectors are included:
#'         \describe{
#'           \item{`periods_restored`}{The full sequence of periods from `min(time)` to `max(time)` by `interval`.}
#'           \item{`periods_missing`}{The periods in `periods_restored` that are not present in the data.}
#'         }}
#' }
#'
#' The function first checks for missing values in the `group` and `time` variables.
#' Rows containing `NA` in either variable are removed, and messages report how many rows were excluded
#' due to each variable. The removed rows are stored in `details$excluded_rows`.
#'
#' It then checks for duplicate group-time combinations. In a properly structured panel dataset,
#' each entity (group) should have at most one observation per time period. If duplicates are found,
#' a message is printed with the number of distinct duplicate combinations and up to five examples.
#' The duplicate combinations are stored in `details$entity_time_duplicates`. **Note:** When a
#' `balance` method other than `NULL` is requested, duplicates are automatically removed (the first
#' occurrence is kept) and a message informs the user.
#'
#' When `interval` is specified, the function first attempts to convert the time variable to numeric
#' (if it is not already). If conversion fails, an error is raised. It then checks whether the observed
#' time points are compatible with a regular spacing of that interval. If any observed difference is not
#' a multiple of the interval, an error is raised. If all differences are multiples of the interval but
#' there are gaps, a message is printed listing the missing periods, and the analysis continues.
#' In this case, the restored full sequence and the missing periods are stored in
#' `details$periods_restored` and `details$periods_missing`, respectively.
#'
#' \strong{Balancing the panel}
#'
#' If `balance` is not `NULL`, the panel is balanced according to the chosen method.
#' The definition of **presence** follows that of [describe_patterns()]: an entity is considered present
#' in a time period if there exists at least one row with that combination and at least one non-`NA`
#' value in any substantive variable (i.e., all columns except `group` and `time`).
#'
#' \describe{
#'   \item{`balance = "entities"`}{Keep only those entities that are present in **all** time periods.
#'         Rows belonging to entities that miss at least one period are removed.}
#'   \item{`balance = "periods"`}{Keep only those time periods for which **all** entities are present.
#'         Rows with time values not satisfying this condition are removed.}
#'   \item{`balance = "all"`}{Create a row for every combination of entity and time period. If a combination
#'         already exists, the corresponding row(s) are kept (if duplicates exist, only the first is kept);
#'         if it does not exist, a new row is added with `NA` in all other columns.
#'         If `interval` is supplied, the full time sequence from `min(time)` to `max(time)` by `interval`
#'         is used, so that missing periods are also filled.}
#' }
#'
#' After balancing, the `details$entities` and `details$periods` components are updated to reflect
#' the new data. No additional vectors are added for the removed entities/periods, but the information
#' is implicitly contained in the filtered data.
#'
#' @seealso
#' [describe_dimensions()], [describe_balance()], [describe_periods()], [describe_patterns()]
#'
#' @examples
#' data(production)
#'
#' # Add panel attributes without interval check
#' panel_data <- make_panel(production, group = "firm", time = "year")
#'
#' # Check the attributes
#' attr(panel_data, "metadata")
#' attr(panel_data, "details")
#'
#' # Use with describe_dimensions()
#' describe_dimensions(panel_data)
#'
#' # With interval specification (assuming yearly data)
#' panel_data2 <- make_panel(production, group = "firm", time = "year", interval = 1)
#'
#' # Keep only firms present in every year
#' balanced_entities <- make_panel(production, group = "firm", time = "year",
#'                                 balance = "entities")
#'
#' # Keep only years where all firms are observed
#' balanced_periods <- make_panel(production, group = "firm", time = "year",
#'                                balance = "periods")
#'
#' # Create a fully balanced panel (all firm‑year combinations)
#' fully_balanced <- make_panel(production, group = "firm", time = "year",
#'                              balance = "all", interval = 1)
#'
#' @export
make_panel <- function(data, group, time, interval = NULL, balance = NULL) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
  }

  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string")
  }

  if (!group %in% names(data)) {
    stop('group variable "', group, '" not found in data')
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string")
  }

  if (!time %in% names(data)) {
    stop('time variable "', time, '" not found in data')
  }

  if (time == group) {
    stop("'time' and 'group' cannot be the same variable")
  }

  # Validate balance argument
  valid_balance <- c("entities", "periods", "all")
  if (!is.null(balance) && !balance %in% valid_balance) {
    stop("'balance' must be NULL, 'entities', 'periods', or 'all'")
  }

  # --- Check for missing values in group and time ---
  na_group <- is.na(data[[group]])
  na_time <- is.na(data[[time]])
  excluded_rows <- NULL

  if (any(na_group)) {
    n_na_group <- sum(na_group)
    message(
      "Missing values in ",
      group,
      " variable found. Excluding ",
      n_na_group,
      " rows."
    )
  }
  if (any(na_time)) {
    n_na_time <- sum(na_time)
    message(
      "Missing values in ",
      time,
      " variable found. Excluding ",
      n_na_time,
      " rows."
    )
  }

  if (any(na_group | na_time)) {
    excluded_rows <- data[na_group | na_time, , drop = FALSE]
    data <- data[!(na_group | na_time), , drop = FALSE]
    rownames(data) <- NULL
  }

  # --- Check for duplicate group-time combinations (after NA removal) ---
  dup_combinations <- NULL
  dup_rows <- duplicated(data[c(group, time)]) |
    duplicated(data[c(group, time)], fromLast = TRUE)
  if (any(dup_rows)) {
    dup_combinations <- unique(data[dup_rows, c(group, time), drop = FALSE])
    n_dup <- nrow(dup_combinations)
    examples <- utils::head(dup_combinations, 5)
    example_strings <- paste0(examples[[group]], "-", examples[[time]])
    example_str <- paste(example_strings, collapse = ", ")
    message(
      n_dup,
      " duplicate group-time combinations found. Examples: ",
      example_str
    )
    # If balancing is requested, we must remove duplicates to avoid ambiguity
    if (!is.null(balance)) {
      # Keep first occurrence for each combination
      data <- data[!duplicated(data[c(group, time)]), , drop = FALSE]
      message("Duplicates removed (first occurrence kept) for balancing.")
    }
  }

  # --- Interval validation (if provided) ---
  if (!is.null(interval)) {
    if (
      !is.numeric(interval) ||
        length(interval) != 1 ||
        interval <= 0 ||
        interval != round(interval)
    ) {
      stop("'interval' must be a positive integer")
    }

    # Attempt to coerce time variable to numeric (NA values already removed)
    time_vals_orig <- data[[time]]
    if (!is.numeric(time_vals_orig)) {
      time_numeric <- suppressWarnings(as.numeric(as.character(time_vals_orig)))
      if (anyNA(time_numeric)) {
        stop(
          "Cannot convert the time variable '",
          time,
          "' to numeric. ",
          "Please ensure it contains numbers or convert it manually."
        )
      }
      data[[time]] <- time_numeric
    }
  }

  # Helper to sort unique values preserving original class (for later use)
  sort_unique_preserve <- function(x) {
    ux <- unique(x)
    if (is.numeric(ux)) {
      sort(ux)
    } else if (inherits(ux, "Date") || inherits(ux, "POSIXt")) {
      sort(ux)
    } else if (is.factor(ux)) {
      char_lev <- as.character(ux)
      sorted_char <- sort(char_lev)
      factor(sorted_char, levels = sorted_char, ordered = is.ordered(ux))
    } else {
      sort(ux)
    }
  }

  # --- If balance is requested, perform the balancing ---
  if (!is.null(balance)) {
    # Identify substantive columns (all except group and time)
    data_cols <- setdiff(names(data), c(group, time))
    if (length(data_cols) == 0) {
      stop(
        "No data columns found (excluding group and time). Cannot determine presence."
      )
    }

    # Determine presence: for each entity‑time combination, check if any data column is non‑NA
    # We'll create a presence matrix using the current data (after NA removal and duplicate handling)
    unique_entities <- sort_unique_preserve(data[[group]])
    unique_times <- sort_unique_preserve(data[[time]])
    entity_char <- as.character(unique_entities)
    time_char <- as.character(unique_times)

    # Initialize presence matrix (all FALSE)
    presence <- matrix(
      FALSE,
      nrow = length(entity_char),
      ncol = length(time_char),
      dimnames = list(entity_char, time_char)
    )

    # Fill presence matrix
    for (i in seq_len(nrow(data))) {
      e <- as.character(data[[group]][i])
      t <- as.character(data[[time]][i])
      # Check if any data column in this row is non‑NA
      if (any(!is.na(data[i, data_cols, drop = TRUE]))) {
        presence[e, t] <- TRUE
      }
    }

    if (balance == "entities") {
      # Keep entities that are present in all time periods
      entities_present_all <- apply(presence, 1, all)
      keep_entities <- entity_char[entities_present_all]
      if (length(keep_entities) == 0) {
        stop("No entity is present in all time periods.")
      }
      data <- data[
        as.character(data[[group]]) %in% keep_entities,
        ,
        drop = FALSE
      ]
      rownames(data) <- NULL
    } else if (balance == "periods") {
      # Keep periods that are present for all entities
      periods_present_all <- apply(presence, 2, all)
      keep_periods <- time_char[periods_present_all]
      if (length(keep_periods) == 0) {
        stop("No time period has all entities present.")
      }
      data <- data[as.character(data[[time]]) %in% keep_periods, , drop = FALSE]
      rownames(data) <- NULL
    } else if (balance == "all") {
      # Create all entity‑time combinations
      # Determine the full time sequence
      if (!is.null(interval)) {
        # Use interval to create full sequence from min to max
        time_vals <- sort(unique(data[[time]]))
        full_time <- seq(
          from = min(time_vals),
          to = max(time_vals),
          by = interval
        )
        # Convert to character for merging (preserve original class later)
        full_time_char <- as.character(full_time)
      } else {
        full_time_char <- time_char
        full_time <- unique_times
      }

      # Create all combinations
      all_combos <- expand.grid(
        entity = entity_char,
        time = full_time_char,
        stringsAsFactors = FALSE
      )
      names(all_combos) <- c(group, time)

      # Merge with existing data (keep all rows from all_combos)
      # First, convert data's group/time to character for merging
      data_merge <- data
      data_merge[[group]] <- as.character(data_merge[[group]])
      data_merge[[time]] <- as.character(data_merge[[time]])

      merged <- merge(
        all_combos,
        data_merge,
        by = c(group, time),
        all.x = TRUE,
        sort = FALSE
      )

      # Restore original classes for group and time (if possible)
      # For group: use the class of original unique_entities
      if (is.factor(unique_entities)) {
        merged[[group]] <- factor(
          merged[[group]],
          levels = levels(unique_entities)
        )
      } else if (inherits(unique_entities, "Date")) {
        merged[[group]] <- as.Date(merged[[group]]) # may need more careful handling
      } else if (is.numeric(unique_entities)) {
        merged[[group]] <- as.numeric(merged[[group]])
      }
      # For time: similarly
      if (is.factor(unique_times)) {
        merged[[time]] <- factor(merged[[time]], levels = levels(unique_times))
      } else if (inherits(unique_times, "Date")) {
        # If interval used, full_time may be numeric; we need to convert back to Date if original was Date
        if (!is.null(interval) && inherits(unique_times, "Date")) {
          # full_time is numeric (from seq), so convert to Date
          merged[[time]] <- as.Date(merged[[time]], origin = "1970-01-01")
        } else {
          merged[[time]] <- as.Date(merged[[time]])
        }
      } else if (is.numeric(unique_times)) {
        merged[[time]] <- as.numeric(merged[[time]])
      }

      # Sort by group and time (preserve original order of groups as much as possible)
      # Use order on the character versions to respect factor levels if present
      group_char_merged <- as.character(merged[[group]])
      time_char_merged <- as.character(merged[[time]])
      ord <- order(
        factor(group_char_merged, levels = entity_char),
        factor(time_char_merged, levels = full_time_char)
      )
      data <- merged[ord, , drop = FALSE]
      rownames(data) <- NULL
    }
  }

  # --- Build metadata and details ---
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time,
    interval = interval,
    balance = balance
  )

  # Recompute entities and periods from the (possibly modified) data
  entities <- sort_unique_preserve(data[[group]])
  periods <- sort_unique_preserve(data[[time]])

  details <- list(
    entities = entities,
    periods = periods
  )

  if (!is.null(excluded_rows)) {
    details$excluded_rows <- excluded_rows
  }

  if (!is.null(dup_combinations)) {
    details$entity_time_duplicates <- dup_combinations
  }

  # If interval is given, perform regularity checks and compute missing periods
  if (!is.null(interval)) {
    # Use numeric periods (if time is not numeric, we already coerced above)
    time_vals <- data[[time]]
    if (!is.numeric(time_vals)) {
      # This should not happen because we coerced earlier, but just in case
      time_vals <- suppressWarnings(as.numeric(as.character(time_vals)))
      if (anyNA(time_vals)) {
        stop(
          "Time variable could not be converted to numeric for interval check."
        )
      }
    }
    obs_periods <- sort(unique(time_vals))
    time_diffs <- diff(obs_periods)
    if (!all(time_diffs %% interval == 0)) {
      stop(
        "The observed time points are not evenly spaced by multiples of the specified interval (",
        interval,
        "). ",
        "For example, differences such as ",
        paste(unique(time_diffs[time_diffs %% interval != 0]), collapse = ", "),
        " are not multiples of ",
        interval,
        "."
      )
    }

    full_seq <- seq(
      from = min(obs_periods),
      to = max(obs_periods),
      by = interval
    )
    missing <- setdiff(full_seq, obs_periods)

    if (length(missing) > 0) {
      details$periods_restored <- full_seq
      details$periods_missing <- missing

      msg <- paste(
        "Irregular time intervals detected. Missing periods:",
        paste(missing, collapse = ", ")
      )
      message(msg)
    }
  }

  # Add attributes and class
  attr(data, "metadata") <- metadata
  attr(data, "details") <- details
  class(data) <- c("panel_data", class(data))

  return(data)
}
