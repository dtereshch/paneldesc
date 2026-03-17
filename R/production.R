#' Simulated Unbalanced Panel Data for Cobb-Douglas Production Function Analysis
#'
#' A simulated dataset containing firm-level panel data with industry affiliation,
#' entry, exit, random missing values, and ownership information.
#' The data follows industry-specific production structures with occasional
#' industry and ownership changes.
#'
#' @format A data frame with 180 rows (30 firms × 6 years) and 7 variables:
#' \describe{
#'   \item{firm}{integer; firm identifier (1 to 30)}
#'   \item{year}{integer; year identifier (1 to 6)}
#'   \item{industry}{factor; industry affiliation with three levels:
#'                   "Industry 1", "Industry 2", "Industry 3". Some firms change
#'                   industry over time.}
#'   \item{sales}{numeric; firm sales/output generated from a Cobb-Douglas
#'                production function with industry-specific parameters and
#'                technology shocks. Contains random missing values (~2%).}
#'   \item{capital}{numeric; capital input, log‑normally distributed with
#'                  firm-specific effects and industry-specific time trends.
#'                  Contains random missing values (~2%).}
#'   \item{labor}{numeric; labor input, log‑normally distributed with
#'                firm-specific effects and industry-specific time trends.
#'                Contains random missing values (~2%).}
#'   \item{ownership}{factor; ownership type with three levels:
#'                    "private", "public", "mixed". The variable is stable over
#'                    time but changes with a probability of 5% per year.}
#' }
#'
#' @details
#' The dataset exhibits several realistic features of firm-level panel data:
#' \itemize{
#'   \item 50% of firms (15 firms) have complete data for all 6 years.
#'   \item 50% of firms (15 firms) have entry and exit patterns with different
#'         start and end years.
#'   \item Three industry categories with different production function parameters.
#'   \item About 20% of firms change industry affiliation at least once.
#'   \item Ownership changes occur with 5% probability per year.
#'   \item Industry-specific Cobb‑Douglas parameters:
#'     \itemize{
#'       \item Industry 1: α = 0.25, β = 0.65, A = 2.0 (labor‑intensive)
#'       \item Industry 2: α = 0.35, β = 0.55, A = 2.2 (balanced, high productivity)
#'       \item Industry 3: α = 0.30, β = 0.60, A = 1.8 (standard)
#'     }
#'   \item Additional random missing values (approx. 2%) in sales, capital, and labor.
#'   \item Firm-specific effects and industry-specific time trends in inputs.
#'   \item Technology shocks affecting output.
#' }
#'
#' @source Simulated data for econometric analysis and demonstration purposes
#'
#' @examples
#' data(production)
#' head(production)
#' table(production$ownership)
"production"
