#' Capelin table
#'
#' A dataset containing the number of capelin per age, year and length group
#'
#'
#' @format A data frame with 1536 rows and 11 variables:
#' \describe{
#'   \item{length.group}{integer, length category index}
#'   \item{1}{numeric, number of fish of age 1}
#'   \item{2}{numeric, number of fish of age 2}
#'   \item{3}{numeric, number of fish of age 3}
#'   \item{4}{numeric, number of fish of age 4}
#'   \item{5}{numeric, number of fish of age 5 and older}
#'   \item{sum(10e9)}{numeric, sum over all age groups}
#'   \item{biomass(10e3t)}{numeric, biomass, product of sum and meanweight}
#'   \item{meanweight(g)}{numeric, meanweight of fish in grams}
#'   \item{meanlength(cm)}{numeric, meanlength of fish in cm}
#'   \item{year}{integer, year}
#' }
#' @source \url{https://www.imr.no}
"cap"
