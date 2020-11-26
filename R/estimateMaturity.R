#' Function for estimating maturity
#'
#' @param data list of data inputs
#' @param parameters list of parameters
#' @param ... additional arguments for TMB::MakeADFun
#'
#' @return list of TMB object and optimization object
#' @export
#'
#' @examples
#' #estMature(data = data.list, parameters = par.list, silent =TRUE)
estimateMaturity <- function(data, parameters, ...){
  # .. initialize TMB object ..
  obj <- TMB::MakeADFun(data = c(model = "mature", data),
                        parameters = parameters,
                        DLL = "bifrost_TMBExports", ...)
  # .. Optimize likelihood ..
  opt <- stats::nlminb(obj$par, obj$fn, obj$gr)

  # ..Return results..
  return.list <- list()
  return.list$obj <- obj
  return.list$opt <- opt
  return.list$data <- data
  return.list$parameters <- parameters
  return.list$sdrep <- TMB::sdreport(obj)
  return.list$sumsdrep <- TMB::summary.sdreport(return.list$sdrep)
  attr(return.list, "class") <- c("maturity","list")
  return(return.list)
}


#' Summary of estimation of Maturity
#'
#' @param x maturity, object from running estimation
#'
#' @return summary
#' @export
#'
#'
#'
summary.maturity <- function(x){
  summary.list <- list()
  class(summary.list)<-"summary.maturity"
  tab <- matrix(ncol = 4, nrow = nrow(x$sumsdrep[-(1:4),]))
  tab[,1:2] <- x$sumsdrep[-(1:4),]
  tab[,3] <- tab[,1]/tab[,2]
  tab[,4] <- 2 * pnorm(abs(tab[,3]), lower.tail = FALSE)
  colnames(tab) <- c("Estimate", "Std. Error", "Test score", "p-value*")
  rownames(tab) <- rownames(x$sumsdrep[-(1:4),])
  summary.list$result.tab <- tab
  summary.list$convergence.code <- x$opt$convergence
  summary.list$convergence.message <- x$opt$message
  summary.list$aic <- 2 * (x$opt$objective +length(x$opt$par))
  summary.list$maturitytable <-tibble::tibble(
    ml = x$data$meanlength,
    r = maturing(meanlength = ml,
                 p1 = tab["p1",1], p2 = tab["p2",1]))
  return(summary.list)
}

#' Print summary of maturity
#'
#' @param x object of type summary.maturity
#' @param ... addition arguments
#'
#' @return printout
#' @export
#'
#'
print.summary.maturity <- function(x,...) {
  print(x$result.tab)
  cat("\n* Using Gaussian approximation for p-values:\n")
  cat("\n-------------------------------------------",
      "\nConvergence code:     ", x$convergence.code,
      "\nCovergence message:   ", x$convergence.message,
      "\nAIC:                  ", x$aic,
      "\n-------------------------------------------\n\n"
  )

  print(
    ggplot2::ggplot(data = x$maturitytable,
                        ggplot2::aes(x = ml, y = r)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::xlab("Mean length") +
      ggplot2::ylab("Maturity rate") +
      ggplot2::theme_bw()
    )
}
