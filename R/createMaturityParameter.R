#' Set up parameter list for maturity
#'
#' @param parameter data.frame, initial values for p1,p2,p3 and nu by year
#' @param year integer, year
#'
#' @return list, parameter object for running maturity estimation by TMB
#' @export
#'
#' @examples
#' data(maturityInitialParameters)
#' createMaturityParameters(parameter=maturityInitialParameters, year = 2017)
createMaturityParameters <- function(parameter, year){
  if(!is.data.frame(parameter)) stop("parameter is not a data.frame.")
  if(!(is.numeric(year)|is.integer(year))) stop("year is not an integer.")
  if(!all(c("p1","p2","p3","nu","year") %in% names(parameter)))
    stop("parameter data frame does not contain necessary columns:\np1, p2, p3, nu and year")
  i <- which(parameter$year == year)
  return(list(
    lnp1=log(parameter[i,"p1"]),
    lnp2=log(parameter[i,"p2"]),
    lnp3=log(parameter[i,"p3"]),
    lnnu=log(parameter[i,"nu"]))
  )
}

