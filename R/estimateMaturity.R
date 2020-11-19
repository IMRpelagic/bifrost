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
  return.list$sdrep <- TMB::sdreport(obj)
  return.list$sumsdrep <- TMB::summary.sdreport(return.list$sdrep)
  return(return.list)
}
