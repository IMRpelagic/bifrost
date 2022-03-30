#' Catch data for capelin
#'
#' A dataset containing the quantity of capelin caught per age and year. The catch is measured in million tons.
#'
#'
#' @format A data frame with 240 rows and 14 variables:
#' \describe{
#'   \item{age}{integer, age of capelin}
#'   \item{year}{integer, catch year}
#'   \item{winter05}{numeric, catch in million tons in winter x 0.5}
#'   \item{winter03}{numeric, catch in million tons in winter x 0.3}
#'   \item{winter02}{numeric, catch in million tons in winter x 0.2}
#'   \item{spring01}{numeric, catch in million tons in spring x 0.1}
#'   \item{spring05}{numeric, catch in million tons in spring x 0.5}
#'   \item{spring04}{numeric, catch in million tons in spring x 0.4}
#'   \item{fill0}{numeric, 0, no fish caught in summer months}
#'   \item{fill1}{numeric, 0, no fish caught in summer months}
#'   \item{fill2}{numeric, 0, no fish caught in summer months}
#'   \item{fill3}{numeric, 0, no fish caught in summer months}
#'   \item{autumn08}{numeric, catch in million tons in autumn x 0.8}
#'   \item{autumn02}{numeric, catch in million tons in autumn x 0.2}
#' }
#' @source \url{https://www.imr.no}
"catch"
