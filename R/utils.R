#' Validate and Convert Input to Plain Data Frame
#'
#' @description
#' This function ensures input data can be used as a plain data frame.
#'
#' @details
#' This function:
#' 1. Converts non-data.frame objects to data.frame using as.data.frame()
#' 2. Strips data.frame subclasses to create plain data.frames
#' 3. Removes pseries classes from all columns (common with pdata.frame objects)
#' 4. Provides informative error messages on failure
#'
#' @param data The input data object to check and convert.
#' @param arg_name A character string specifying the argument name to use in
#'   error messages. Default = "data".
#' @param convert_special_df A logical flag indicating whether special data.frame subclasses should be converted
#'   to plain data.frames. Default = TRUE.
#' @param warn_conversion A logical flag indicating whether warnings should be shown when converting
#'   non-data.frame objects or special subclasses to plain data.frame.
#'   Default = FALSE.
#' @param strip_pseries A logical flag indicating whether pseries classes should be stripped from columns.
#'   Default = TRUE.
#'
#' @return
#' A plain data.frame with all columns as regular vectors (no pseries classes).
#'
#' @keywords internal
#' @noRd
.check_and_convert_data_robust <- function(
  data,
  arg_name = "data",
  convert_special_df = TRUE,
  warn_conversion = FALSE,
  strip_pseries = TRUE
) {
  # Check for NULL
  if (is.null(data)) {
    stop("'", arg_name, "' cannot be NULL", call. = FALSE)
  }

  # Track original class for warning messages
  orig_class <- class(data)

  # Step 1: Convert to data.frame if not already
  if (!is.data.frame(data)) {
    df <- try(as.data.frame(data, stringsAsFactors = FALSE), silent = TRUE)

    if (inherits(df, "try-error")) {
      stop(
        "Cannot convert '",
        arg_name,
        "' to data.frame.\n",
        "  Original class: ",
        paste(orig_class, collapse = ", "),
        "\n",
        "  Error: ",
        as.character(df),
        call. = FALSE
      )
    }

    # Warn about conversion if requested
    if (warn_conversion && !identical(orig_class, class(df))) {
      warning(
        "Converted '",
        arg_name,
        "' from ",
        paste(orig_class, collapse = ", "),
        " to data.frame.",
        call. = FALSE,
        immediate. = TRUE
      )
    }
  } else {
    df <- data
  }

  # Step 2: Convert special data.frame subclasses to plain data.frames
  if (convert_special_df && length(class(df)) > 1) {
    # Check if it's a pdata.frame or other problematic subclass
    current_class <- class(df)
    is_pdataframe <- any(grepl("pdata", current_class, ignore.case = TRUE))

    # Warn about conversion if requested
    if (warn_conversion) {
      if (is_pdataframe) {
        warning(
          "pdata.frame detected. Converting to plain data.frame and ",
          "stripping pseries classes from columns.",
          call. = FALSE,
          immediate. = TRUE
        )
      } else if (!identical(current_class, "data.frame")) {
        warning(
          "Converting ",
          current_class[1],
          " to plain data.frame.",
          call. = FALSE,
          immediate. = TRUE
        )
      }
    }

    # Force to plain data.frame
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  }

  # Step 3: Strip pseries classes from columns if requested
  if (strip_pseries) {
    df <- .strip_pseries_from_dataframe(df, warn = warn_conversion)
  }

  # Step 4: Final validation
  if (!is.data.frame(df)) {
    stop(
      "Internal error: Failed to produce data.frame for '",
      arg_name,
      "'.\n",
      "  Result class: ",
      paste(class(df), collapse = ", "),
      call. = FALSE
    )
  }

  # Check for zero rows/columns - these warnings are separate and always shown
  # as they indicate potential issues with the data
  if (nrow(df) == 0) {
    warning("'", arg_name, "' has zero rows.", call. = FALSE, immediate. = TRUE)
  }

  if (ncol(df) == 0) {
    warning(
      "'",
      arg_name,
      "' has zero columns.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  return(df)
}

#' Strip pseries classes from all columns in a data frame
#'
#' @description
#' This function removes pseries classes from data frame columns.
#'
#' @param df A data frame.
#' @param warn A logical flag indicating whether warnings should be issued when stripping pseries classes.
#'   Default = FALSE.
#' @return The data frame with pseries classes removed from all columns.
#'
#' @keywords internal
#' @noRd
.strip_pseries_from_dataframe <- function(df, warn = FALSE) {
  if (!is.data.frame(df)) {
    return(df)
  }

  # Process each column
  for (i in seq_along(df)) {
    col_name <- names(df)[i]
    col_data <- df[[i]]

    # Check if column has pseries class
    col_class <- class(col_data)
    has_pseries <- inherits(col_data, "pseries") ||
      any(grepl("pseries", col_class))

    if (has_pseries) {
      # Warn if requested
      if (warn) {
        warning(
          "Stripping pseries class from column '",
          col_name,
          "'",
          call. = FALSE,
          immediate. = TRUE
        )
      }

      # Method 1: Try to preserve the underlying type
      # Get the base type without pseries
      base_class <- col_class[!grepl("pseries", col_class)]

      # Remove pseries class and attributes
      col_data <- unclass(col_data)
      attr(col_data, "index") <- NULL

      # Restore appropriate class if needed
      if (length(base_class) > 0) {
        # Remove any remaining "pseries" from class
        base_class <- base_class[!grepl("pseries", base_class)]
        if (length(base_class) > 0) {
          class(col_data) <- base_class
        } else {
          class(col_data) <- NULL
        }
      } else {
        class(col_data) <- NULL
      }

      df[[i]] <- col_data
    }
  }

  return(df)
}
