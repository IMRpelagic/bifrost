library(bifrost)
x <- rnorm(1000)
d
obj <- TMB::MakeADFun(data = c(model = "mature",
                                         D),
                             parameters = parameters,
                             DLL = "bifrost_TMBExports", silent = TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr)

library(TMB)
