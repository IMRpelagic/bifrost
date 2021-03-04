#' Initial maturity parameters for capelin example
#'
#' A dataset containing p1,p2,p3 and nu initial parameters for running example
#' with min_age = 3 and max_age = 4 on capelin maturity.
#'
#'
#' @format A data frame with 47 rows and 6 variables:
#' \describe{
#'   \item{year}{integer, year}
#'   \item{agegr}{character, age group. Either "2-3" or "3-4".}
#'   \item{p1}{numeric, initial value for p1}
#'   \item{p2}{numeric, initial value for p2}
#'   \item{p3}{numeric, initial value for p3}
#'   \item{nu}{numeric, initial value for nu}
#'   \item{unknown}{integer, 0 or 1, ask Sam Subbey what this is}
#' }
#' @source \url{https://www.imr.no}
"maturityInitialParameters"
