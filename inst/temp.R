library(bifrost)
x <- rnorm(1000)
d
normal_nll <- TMB::MakeADFun(data = list(model = "NormalNLL",
                                         x = x),
                             parameters = list(mm = 0,
                                               ss = 1),
                             DLL = "bifrost_TMBExports", silent = TRUE)

library(TMB)
