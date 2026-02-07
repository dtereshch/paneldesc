#' Print Data Frame with Attributes
#'
#' Utility function to print data.frames along with their attributes in a readable format.
#' This is particularly useful for functions in this package that return data.frames
#' with standardized panel data attributes.
#'
#' @param data A data.frame to print
#'
#' @return Invisibly returns the input data.frame.
#' Prints the results data.frame and the data.frames attributes.
#'
#' @examples
#' data(production)
#' result <- describe_panel(production, group = "firm", time = "year")
#' print_result(result)
#'
#' @export
print_result <- function(data) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame")
  }

  # Set width to 75% of getOption("width", 80)
  full_width <- getOption("width", 80)
  width <- floor(full_width * 0.75)

  # Print main header
  cat("\n")
  cat("PANEL DATA ANALYSIS RESULTS\n")
  cat(strrep("=", width), "\n")
  cat("\n")

  # Print results table section
  cat("RESULTS TABLE\n")
  cat(strrep("-", width), "\n")

  # Print the data frame
  print(data)
  cat("\n")

  # Print attributes section
  cat("ATTRIBUTES\n")
  cat(strrep("-", width), "\n")

  # Get attributes
  attrs <- attributes(data)

  # Remove class and row.names as they're standard
  if ("class" %in% names(attrs)) {
    attrs$class <- NULL
  }
  if ("row.names" %in% names(attrs)) {
    attrs$row.names <- NULL
  }

  # Show all attributes
  if (length(attrs) > 0) {
    # Print each attribute in a compact format
    for (attr_name in names(attrs)) {
      attr_value <- attrs[[attr_name]]

      # Format the attribute value
      if (is.null(attr_value)) {
        formatted_value <- "NULL"
      } else if (is.character(attr_value) && length(attr_value) == 1) {
        formatted_value <- paste0('"', attr_value, '"')
      } else if (is.numeric(attr_value) && length(attr_value) == 1) {
        formatted_value <- as.character(attr_value)
      } else if (is.logical(attr_value) && length(attr_value) == 1) {
        formatted_value <- ifelse(attr_value, "TRUE", "FALSE")
      } else {
        # For complex objects, show a summary
        if (length(attr_value) == 1) {
          formatted_value <- paste0("<", class(attr_value)[1], ">")
        } else {
          formatted_value <- paste0(
            "<",
            class(attr_value)[1],
            " [",
            length(attr_value),
            "]>"
          )
        }
      }

      # Print attribute in compact format
      cat(attr_name, ": ", formatted_value, "\n", sep = "")
    }
  } else {
    cat("No attributes found\n")
  }

  cat("\n")

  # Invisibly return the data frame
  invisible(data)
}
