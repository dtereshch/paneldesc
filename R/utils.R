#' Validate and Convert Input to Plain Data Frame
#'
#' @description
#' Robust helper function that ensures input data can be used as a plain data frame.
#' Handles special data.frame subclasses like pdata.frame from the plm package
#' by stripping problematic classes (pseries) from all columns.
#'
#' @details
#' This function:
#' 1. Converts non-data.frame objects to data.frame using as.data.frame()
#' 2. Strips data.frame subclasses to create plain data.frames
#' 3. Removes pseries classes from all columns (common with pdata.frame objects)
#' 4. Provides informative error messages on failure
#'
#' The function uses only base R functions and has no external dependencies.
#'
#' @param data The input data object to check and convert.
#' @param arg_name Character string specifying the argument name to use in
#'   error messages. Defaults to `"data"`.
#' @param convert_special_df Should special data.frame subclasses be converted
#'   to plain data.frames? Default TRUE.
#' @param warn_conversion Should warnings be shown when converting
#'   non-data.frame objects or special subclasses to plain data.frame?
#'   Default FALSE (no warnings shown).
#' @param strip_pseries Should pseries classes be stripped from columns?
#'   Default TRUE. This is critical for pdata.frame objects.
#'
#' @return
#' A plain data.frame with all columns as regular vectors (no pseries classes).
#'
#' @section Special Handling for pdata.frame:
#' pdata.frame objects from the plm package have columns of class "pseries"
#' which have special comparison operators that break standard R operations.
#' This function detects and strips these classes to ensure compatibility.
#'
#' @examples
#' \dontrun{
#' # These examples would work if run inside the package
#'
#' # Returns mtcars unchanged (already a plain data.frame)
#' .check_and_convert_data_robust(mtcars)
#'
#' # Converts matrix to data.frame (no warning by default)
#' mat <- matrix(1:6, ncol = 2)
#' .check_and_convert_data_robust(mat)
#'
#' # Handles pdata.frame (strips pseries classes, no warning by default)
#' library(plm)
#' pdata <- pdata.frame(mtcars, index = c("cyl", "gear"))
#' .check_and_convert_data_robust(pdata)
#'
#' # With warnings enabled
#' .check_and_convert_data_robust(pdata, warn_conversion = TRUE)
#' }
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
#' Internal helper that removes pseries classes from data frame columns.
#' This is necessary because pseries objects from plm::pdata.frame have
#' special operators that break standard R operations.
#'
#' @param df A data frame
#' @param warn Should warnings be issued when stripping pseries classes?
#'   Default FALSE to avoid excessive warnings.
#' @return The data frame with pseries classes removed from all columns
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

#' Quick check for pseries in data frame columns
#'
#' @description
#' Utility function to check if any columns in a data frame have pseries class.
#'
#' @param df A data frame to check
#' @return Logical: TRUE if any column has pseries class, FALSE otherwise
#'
#' @keywords internal
#' @noRd
.has_pseries_columns <- function(df) {
  if (!is.data.frame(df)) {
    return(FALSE)
  }

  for (i in seq_along(df)) {
    if (inherits(df[[i]], "pseries")) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Safe conversion with backward compatibility
#'
#' @description
#' Wrapper for backward compatibility that calls the robust version
#' with default parameters.
#'
#' @param data Input data
#' @param arg_name Argument name for error messages
#' @param warn_conversion Should warnings be shown when converting?
#'   Default FALSE (no warnings).
#' @return Plain data.frame
#'
#' @keywords internal
#' @noRd
.safe_convert_dataframe <- function(
  data,
  arg_name = "data",
  warn_conversion = FALSE
) {
  # Alias for backward compatibility
  .check_and_convert_data_robust(
    data = data,
    arg_name = arg_name,
    convert_special_df = TRUE,
    warn_conversion = warn_conversion,
    strip_pseries = TRUE
  )
}

#' Simplified safe data frame conversion
#'
#' @description
#' Simplified version with fewer parameters for common use cases.
#' By default, does not show conversion warnings.
#'
#' @param data Input data
#' @param arg_name Argument name for error messages
#' @param warn_conversion Should warnings be shown when converting?
#'   Default FALSE (no warnings).
#' @return Plain data.frame
#'
#' @keywords internal
#' @noRd
.check_and_convert_data_safe <- function(
  data,
  arg_name = "data",
  warn_conversion = FALSE
) {
  # Check for NULL
  if (is.null(data)) {
    stop("'", arg_name, "' cannot be NULL", call. = FALSE)
  }

  # Step 1: Ensure it's a data.frame
  if (!is.data.frame(data)) {
    df <- try(as.data.frame(data, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(df, "try-error")) {
      stop(
        "Failed to convert '",
        arg_name,
        "' to data.frame.\n",
        "  Class: ",
        paste(class(data), collapse = ", "),
        "\n",
        "  Error: ",
        as.character(df),
        call. = FALSE
      )
    }
    if (warn_conversion) {
      warning(
        "Converted '",
        arg_name,
        "' to data.frame.",
        call. = FALSE,
        immediate. = TRUE
      )
    }
  } else {
    df <- data
  }

  # Step 2: Strip any subclasses (pdata.frame, etc.)
  if (length(class(df)) > 1) {
    # Check for pdata.frame specifically
    if (any(grepl("pdata", class(df), ignore.case = TRUE)) && warn_conversion) {
      warning(
        "pdata.frame detected. Converting to plain data.frame.",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  }

  # Step 3: Critical - strip pseries from columns
  for (i in seq_along(df)) {
    col_data <- df[[i]]
    if (inherits(col_data, "pseries")) {
      # Completely remove pseries class structure
      df[[i]] <- .force_plain_vector(col_data)
    }
  }

  return(df)
}

#' Force any vector to be plain (no pseries)
#'
#' @param x Any vector
#' @return Plain vector without pseries class
#'
#' @keywords internal
#' @noRd
.force_plain_vector <- function(x) {
  if (inherits(x, "pseries")) {
    # Get the underlying data
    result <- unclass(x)
    # Remove all pseries-related attributes
    attr(result, "index") <- NULL
    # Remove pseries from class
    if (!is.null(attr(result, "class"))) {
      attr(result, "class") <- attr(result, "class")[
        !grepl("pseries", attr(result, "class"))
      ]
      if (length(attr(result, "class")) == 0) {
        attr(result, "class") <- NULL
      }
    }
    return(result)
  }
  return(x)
}

#' Minimal data frame conversion (no warnings by default)
#'
#' @description
#' Minimal version for quick conversion with no warnings by default.
#'
#' @param data Input data
#' @param arg_name Argument name for error messages
#' @param warn_conversion Show conversion warnings? Default FALSE.
#' @return Plain data.frame
#'
#' @keywords internal
#' @noRd
.check_and_convert_data_minimal <- function(
  data,
  arg_name = "data",
  warn_conversion = FALSE
) {
  # Null check
  if (is.null(data)) {
    stop("'", arg_name, "' cannot be NULL", call. = FALSE)
  }

  # Already a data.frame (includes tibbles)
  if (is.data.frame(data)) {
    # Still need to check for pseries in columns
    data_class <- class(data)
    if (
      length(data_class) > 1 &&
        any(grepl("pdata", data_class, ignore.case = TRUE))
    ) {
      # It's a pdata.frame - need to convert
      if (warn_conversion) {
        warning(
          "pdata.frame detected. Converting to plain data.frame.",
          call. = FALSE,
          immediate. = TRUE
        )
      }
      df <- as.data.frame(data, stringsAsFactors = FALSE)
      # Strip pseries from columns
      for (i in seq_along(df)) {
        if (inherits(df[[i]], "pseries")) {
          df[[i]] <- .force_plain_vector(df[[i]])
        }
      }
      return(df)
    }
    return(data)
  }

  # Not a data.frame - try conversion
  if (warn_conversion) {
    warning(
      "Converting '",
      arg_name,
      "' to data.frame.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  converted <- try(as.data.frame(data, stringsAsFactors = FALSE), silent = TRUE)

  if (inherits(converted, "try-error")) {
    stop(
      "'",
      arg_name,
      "' must be a data.frame or convertible to data.frame.\n",
      "  Class: ",
      paste(class(data), collapse = ", "),
      "\n",
      "  Error: ",
      as.character(converted),
      call. = FALSE
    )
  }

  # Double-check
  if (!is.data.frame(converted)) {
    stop(
      "Conversion of '",
      arg_name,
      "' failed to produce a data.frame.\n",
      "  Got class: ",
      paste(class(converted), collapse = ", "),
      call. = FALSE
    )
  }

  return(converted)
}
