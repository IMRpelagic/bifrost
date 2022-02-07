#' Function for running maturity estimations year by year
#'
#' @param cap data.frame, cap table. Should contain the following column names:
#' meanlength(cm), 1,2,3,4,5 and year, where 1-4 are ages and 5 is a 5+ group.
#' @param catch data.frame, catches data. Should contain 14 columns including
#' age, year and the remaining 12 for each month.
#' @param initPar data.frame, initial values for p1,p2,p3 and nu by year
#' @param min_age integer. Minimum age
#' @param max_age integer. Maximum age
#' @param plot logical, print plot (TRUE or FALSE)
#'
#' @return list/maturitybyyears object, containing estimation output and results
#' @export
#'
#' @examples
#' \dontrun{
#' data(cap)
#' data(catch)
#' data(maturityInitialParameters)
#' result <- runMaturityYearByYear(cap,catch,
#'                                 initPar = dplyr::filter(maturityInitialParameters,
#'                                                         agegr == "3-4"),
#'                                 plot = TRUE)
#' }
runMaturityYearByYear <- function(cap, catch, initPar, min_age = 3, max_age = 4,
                                  plot = TRUE){
  # ..Set up..
  start_year <- max(min(cap$year, catch$year, initPar$year))
  end_year <- min(max(cap$year, catch$year, initPar$year+1))
  # ..Run estimation year by year..
  est.list <- list()
  options(warn = -1)
  for(year in start_year:(end_year-1))
  {
    i <- as.integer(year-start_year+1)
    data <- createMaturityData(cap, catch, min_age = min_age, max_age = max_age,
                               start_year = year, end_year = year+1)
    par.list <- createMaturityParameters(initPar, year  = year)
    est.list[[i]] <- estimateMaturity(data, parameters = par.list, silent = TRUE)
    est.list[[i]]$year <- year
    est.list[[i]]$data.list <- data
    est.list[[i]]$par <- par.list
  }
  options(warn = 0)
  # ..Structure parameter results..
  est.pars <- data.frame(t(simplify2array(lapply(est.list, FUN = function(x){
    x$sumsdrep[c("p1","p2","p3","nu"),1:2]
    }),
    higher = FALSE)))
  names(est.pars) <- c("p1","p2","p3","nu", "se.p1","se.p2","se.p3","se.nu")
  est.pars$year <- start_year:(end_year-1)
  # ..Fixing problematic years by taking mean of neighbours..
  if(any(is.na(est.pars))){
    k <- unique(which(is.na(est.pars[,5:7]), arr.ind = TRUE)[,"row"])
    if(k[1] == 1){
      k <- k[-1]
      est.pars[1, 1:3] <- est.pars[2,1:3]
      est.pars[1, 5:7] <- est.pars[2,5:7]
    }
    if(k[length(k)] == nrow(est.pars)){
      k <- k[-length(k)]
      est.pars[nrow(est.pars), 1:3] <- est.pars[nrow(est.pars)-1,1:3]
      est.pars[nrow(est.pars), 5:7] <- est.pars[nrow(est.pars)-1,5:7]
    }
    est.pars[k, 1:3] <- (est.pars[k-1,1:3]+est.pars[k+1,1:3])/2
    est.pars[k, 5:7] <- sqrt((est.pars[k-1,5:7]^2+est.pars[k+1,5:7]^2)/4)
  }
  est.pars <- tibble::as_tibble(est.pars)

  # ..Structure return  list..
  return.list <- list()
  return.list$estimation <- est.list
  return.list$results <- est.pars
  class(return.list)<-c("maturitybyyears", "list")
  if(plot)
    print(plot(return.list))
  return(return.list)
}


#' Plot maturity by years
#'
#' @param x maturitybyyears
#' @param ...
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' data(cap)
#' data(catch)
#' data(maturityInitialParameters)
#' result <- runMaturityYearByYear(cap,catch,maturityInitialParameters, plot = TRUE)
#' plot(result)
#' }

plot.maturitybyyears <- function(x,...){
  # ..Plot..
  return(ggplot2::ggplot(tidyr::pivot_longer(x$results,
                                           cols = c("p1","p2","p3"),
                                           names_to = "parameter",
                                           values_to = "value"),
                       ggplot2::aes(x = year, y = value)) +
    ggplot2::geom_line(col = "blue", lwd = .9) +
    ggplot2::facet_wrap(~parameter, ncol = 1, scales= "free")+
    ggplot2::theme_bw() +
    ggplot2::ylab("Parameter value")+
    ggplot2::xlab("Year")+
    ggplot2::scale_x_continuous(expand =c(0,0))
  )
}
