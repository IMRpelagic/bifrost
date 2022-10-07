#' Function for running CAPTOOL projection
#'
#' @description
#' Provide a data_list of inputs and run the projection from October 1st to April 1st.
#' Should reproduce the old captool (excel-based) software.
#'
#' @param data_list list of data input (see details)
#' @param nsim number of simulations
#' @param cap_cv capelin coefficient of variation (cv)
#' @param cod_cv cod coefficient of variation (cv)
#' @param plot boolean, should the projection be plotted?
#' @param consumControl numeric vector of length 18 of 0s or 1s.See details.
#'
#' @details
#' The 'data_list' should contain year, cap, catches, cod0, cod1, svalbard,
#' stochasticHistory and (optionally) captool. The year is the assessment year
#' (integer), cap is a table containing numbers at age (0-5) and length (32
#' length groups). Catches is a vector giving the catches in jan, feb and march.
#' The data frames cod0 and cod1 is the cod data for year Y and year Y+1
#' (projection) from the cod assessment. The data frame 'svalbard' contains the
#' historical svalbard porportions from the different years we are to sample
#' from divided by cod age. The data frame 'stochasticHistory' contains
#' historical samples of parameters we are to sample from, i.e. p1, p2, p3 (p3 =
#' M = natural mortality) and Cmax and Chalf. Lastly, if comparion to
#' captool (the excel-based software) is to be made, one can also provide the
#' projected quantiles from captool as an argument at these will be added to the
#' figure showing the projection.
#'
#' The input vector consumControl should be of length 18 containing 0s and 1s
#' to control when the consum should begin or end. The period 1.january to
#' 1.april is divided into 18 parts; 6 per month. E.g.
#' consumControl = c(rep(0,6), rep(1,12)) would make the consum by cod start
#' on Feburary 1st.
#'
#' In 2022 the Russian part of the survey was not conducted. Hence we included
#' the option to scale up the abundance of capelin in october by some scaling
#' factors based on the historical distribution of capelin in russian economic
#' zone relative to the Norwegian part. The simulation procedure will sample a
#' scaling factor from data_list$scaling.factors with equal probability in each
#' simulation.
#'
#'
#' @return list of output from the projection.
#' @export
#'
#' @examples
#' # captool(data_list)
#'
captool <- function(data_list, nsim=5e4, cap_cv=0.2, cod_cv=0.3, plot = TRUE,
                    consumControl = rep(1,18)){
  if(!all(c("year", "cap", "catches", "cod0", "cod1", "svalbard", "stochasticHistory") %in% names(data_list)))
    stop("The following is missing from the data_list object: \n",
         paste(c("year", "cap", "catches", "cod0", "cod1", "svalbard", "stochasticHistory")[which(
           !(c("year", "cap", "catches", "cod0", "cod1", "svalbard", "stochasticHistory") %in%
               names(data_list)))], collapse = ", "))
  if(length(consumControl)!=18 | !is.numeric(consumControl))
    stop("consumControl must be numeric of length 18." )
  if(is.null(data_list$scaling.factors)) data_list$scaling.factors <- 1
  if(length(data_list$scaling.factors)==1) data_list$scaling.factors <- rep(data_list$scaling.factors,2)
  #nsim=5e4; cap_cv=0.2; cod_cv=0.3
  cind1 <- sample(1:nrow(data_list$stochasticHistory), nsim, replace=T)
  cind2 <- sample(1:ncol(data_list$stochasticHistory[,-(1:10)]), nsim, replace=T)

  # --- 1oct - 1jan ----
  naa <- as.matrix(data_list$cap[,2:6])
  mw <- as.numeric(unlist(data_list$cap[,7]))
  ml <- as.numeric(unlist(data_list$cap[,9]))
  mat <- matrix(0, ncol = nsim, nrow = 4)
  p3 <- numeric(nsim)
  for(i in 1:nsim){
    p1 <- unlist(data_list$stochasticHistory$capelinP1)[cind1[i]]
    p2 <- unlist(data_list$stochasticHistory$capelinP2)[cind1[i]]
    p3[i] <- unlist(data_list$stochasticHistory[cind1[i],10+cind2[i]])
    M2 <- (mw*bifrost::maturing(ml, p1, p2))%*%  naa * sample(x=data_list$scaling.factors, size = 1)
    mat[1,i] <-sum( M2* stats::rnorm(length(M2), mean = 1, sd = cap_cv))
    for(t in 2:4){
      mat[t,i] <- mat[t-1,i] * exp(-p3[i])
    }
  }
  simulations <- as.data.frame(mat)
  names(simulations)<- paste0("sim", 1:nsim)
  simulations$date <- as.Date(paste(data_list$year+c(0,0,0,1), c(10:12, "01"),
                           "01", sep = "-"))
  simulations <- tidyr::pivot_longer(simulations, cols = 1:nsim,
                            names_to = "sim", values_to = "value")

  # Extract SSB at 1jan:
  M <- Cod <- K <- pred <- numeric(18)
  SSBmc <- matrix(0, nrow = nsim, ncol = 19)
  SSBmc[,1] <- simulations[lubridate::year(simulations$date) == data_list$year+1,]$value
  Zco <- (data_list$cod1$Fmult*data_list$cod1$exploit + data_list$cod1$M)/12
  CodSuit <- data_list$cod1$suit
  Oco <- data_list$cod1$maturity
  Wco <- data_list$cod1$stockW *1000

  catcheswithzeros <- numeric(3*6)
  # catches <-  0.205 * c(0, .3, .7)
  catcheswithzeros[seq(3,18, by  = 6)] <- data_list$catches

  Nco <- data_list$cod0$stock

  for(i in 1:nsim){
    Cmax0 <- data_list$stochasticHistory$maxCons[cind1[i]]
    Chalf0 <- data_list$stochasticHistory$halfConsBiomass[cind1[i]]#sample(halfConsCandidates, size = 1)
    SV <- unlist(data_list$svalbard[,sample(1:ncol(data_list$svalbard), size = 1)])
    nn <- length(Nco)
    NY <- rep(NA, nn)
    N <- Nco * c(1,1,rlnorm(n = nn-2, meanlog = log(1/sqrt(1+cod_cv^2)), sqrt(log(1+cod_cv^2))))
    NY[-1] <- N[-nn] * exp(-(data_list$cod0$M[-nn] +data_list$cod0$exploit[-nn]*data_list$cod0$Fmult[-nn]))
    NY[nn] <- NY[nn] +  N[nn] * exp(-(data_list$cod0$M[nn] +data_list$cod0$exploit[nn]*data_list$cod0$Fmult[nn]))
    NY[2:3] <- data_list$cod1$stock[1:2]
    Nco.sample <- NY[-1]
    predability <- rep(c(sum(Nco.sample * CodSuit *(1 - SV)*(1-Oco)*(Wco/1000)^(0.801)*exp(- (1-0.5) * Zco)),
                    sum(Nco.sample * CodSuit *(1 - SV)*(1-Oco)*(Wco/1000)^(0.801)*exp(- (2-0.5) * Zco)),
                    sum(Nco.sample * CodSuit *(1 - SV)*(1-Oco)*(Wco/1000)^(0.801)*exp(- (3-0.5) * Zco))),
               each = 6)

    for(t in 1:18){
      K[t] <- Cmax0/6 * predability[t] * SSBmc[i,t] / (Chalf0 + SSBmc[i,t])
      M[t] <- ifelse(consumControl[t]==1, -log(1-K[t]/SSBmc[i,t]), p3[i]/6)
      SSBmc[i,t+1] <- SSBmc[i,t] * (exp(-M[t])) - catcheswithzeros[t]
    }
  }
  #plot(pred,Cod/6)
  #abline(0,1)
  SSBmc.df <- as.data.frame(t(SSBmc[,1+seq(6,18,6)]))
  names(SSBmc.df)<- paste0("sim", 1:nsim)
  SSBmc.df$date <- seq(as.Date(paste0(data_list$year+1,"-02-01")),
                       as.Date(paste0(data_list$year+1,"-04-01")),
                       by = "month")
  #SSBmc.df

  SSBmc.df <- tidyr::pivot_longer(SSBmc.df, cols = 1:nsim,
                                  names_to = "sim", values_to = "value")
  return <- list()
  return$data_list <- data_list
  return$captooloptions <- list(nsim=nsim, cap_cv=cap_cv, cod_cv=cod_cv)
  return$simulations <- rbind(simulations, SSBmc.df)
  return$quantiles <- return$simulations %>% dplyr::group_by(date) %>% dplyr::summarize(
                              q50 = stats::median(value),
                              q75 = stats::quantile(value, .75),
                              q95 = stats::quantile(value, .95),
                              q25 = stats::quantile(value, .25),
                              q05 = stats::quantile(value, .05)) %>%
                              tidyr::pivot_longer(cols = 2:6,
                                                  names_to = "quant", values_to = "value")

  attr(return, "class") <- c("captoolSim","list")
  return$quanttable <- return$simulations %>% dplyr::group_by(date) %>% dplyr::summarize(
    q50 = stats::median(value),
    q75 = stats::quantile(value, .75),
    q95 = stats::quantile(value, .95),
    q25 = stats::quantile(value, .25),
    q05 = stats::quantile(value, .05))
  return$captool <- data_list$captool
  return$captool$date <- return$quanttable$date
  return$captool <- return$captool[,c(7,2:6)]
  return$year = data_list$year
  return$scaling.factors = scaling.factors
  if(!is.null(data_list$spawningsurvey))
    return$spawningsurvey = data_list$spawningsurvey
  if(plot)
    plotting_validation(return, save = FALSE)
  return(return)
}

#' Plotting output from captool
#'
#' @param return list, returned by captool function
#' @param path character, where to save the figure
#' @param save boolean, should the figure be saved?
#' @param compare compare to excel software output
#'
#' @return ggplot figure object
#' @export
#'
#' @examples
#' \dontrun{plotting_validation(run)}
plotting_validation <- function(return, path = "", save = FALSE, compare = FALSE){
  quantwide <- tidyr::pivot_wider(return$quantiles,
                                  id_cols = 1,
                                  names_from = "quant",
                                  values_from="value")

  # Catch-label
  catch.lab <- paste0(c(
    paste0("Catch: ", round(1000*sum(return$data_list$catches),1), " kt"),
    paste0("Jan: ", round(1000*return$data_list$catches[1],1)," kt"),
    paste0("Feb: ", round(1000*return$data_list$catches[2],1)," kt"),
    paste0("Mar: ", round(1000*return$data_list$catches[3],1)," kt")),
    collapse = "\n" )
  p <- ggplot2::ggplot(quantwide,
                    ggplot2::aes_string(x = "date", y = "q50"))+
      ggplot2::geom_ribbon( ggplot2::aes_string(ymin = "q05", ymax = "q95"),
                            fill = "darkgreen")+
      ggplot2::geom_ribbon( ggplot2::aes_string(ymin = "q25", ymax = "q75"),
                            fill = "red")+
      ggplot2::geom_line(col = "yellow", lwd = 2)+
      ggplot2::scale_y_continuous(name = "SSB",
                                  limits= c(0,1.05*max(dplyr::select(quantwide, -date), return$captool[,-1])),
                                  breaks = seq(0, 1.05*max(dplyr::select(quantwide,-date)), .1))+
      ggplot2::scale_x_date(breaks = seq(min(quantwide$date),
                                         max(quantwide$date),
                                         by = "1 months"),
                            date_labels = "%b-%Y")+
    ggplot2::geom_hline(yintercept = .2)+
    ggplot2::ggtitle(return$year) +
    ggplot2::geom_label(x= Inf, y = Inf,hjust = 1,vjust =1,
                        label = catch.lab,
                        label.size = NA)
  if(!is.null(return$data_list$captool) & compare){
   p <- p +
     ggplot2::geom_line(data=return$captool, ggplot2::aes_string(x = "date", y = "Y"), lty = 2, lwd = .9)+
     ggplot2::geom_line(data=return$captool, ggplot2::aes_string(x = "date", y = "`Y+1`"), lty = 2, lwd = .9)+
     ggplot2::geom_line(data=return$captool, ggplot2::aes_string(x = "date", y = "`Y+2`"), lty = 2, lwd = .9)+
     ggplot2::geom_line(data=return$captool, ggplot2::aes_string(x = "date", y = "`Y-1`"), lty = 2, lwd = .9)+
     ggplot2::geom_line(data=return$captool, ggplot2::aes_string(x = "date", y = "`Y-2`"), lty = 2, lwd = .9)
  }
  if(!is.null(return$data_list$spawningsurvey)){
    tmp <- tibble(
      x = as.Date(paste0(max(lubridate::year(quantwide$date)),"-03-12")),
      y = return$data_list$spawningsurvey[2],
      ymin=return$data_list$spawningsurvey[1],
      ymax=return$data_list$spawningsurvey[3])
    p <- p +
       ggplot2::geom_point(data = tmp, ggplot2::aes(x=x, y = y), col = "skyblue", size = 5) +
       ggplot2::geom_errorbar(data =tmp,
                              ggplot2::aes(x=x, y = y, ymin = ymin, ymax = ymax), col = "skyblue", width = 10, lwd = 1.2)
  }
  print(p)
  if(save)
    ggplot2::ggsave(p,paste0(path, "/prediction_",return$year, ".tiff"), device = "tiff",
             width = 10, height = 7, dpi= "retina")
  return(p)
}




#' Function for calculating squared distance between 5% quantile and blim
#'
#' @param totalcatch total catch
#' @param data_list data_list as input for captool function
#' @param catch.distribution vector of length 3 given ratio of catch in jan, feb and mar
#' @param blim Blim
#' @param seed integer, for not changing the random input
#' @param ... other arguments to be passed to captool function
#'
#' @return squared distance from q05 to blim
#' @export
#'
q05ofcatch <- function(totalcatch = 0, data_list, catch.distribution = c(0, 0.3, 0.7), blim = 0.2, seed = NULL, ...){
  if(is.null(seed))
    warning("No seed specified")
  data_list$catches <- catch.distribution * totalcatch
  set.seed(seed)
  run <- captool(data_list, plot = FALSE, ...)
  (blim-tail(run$quanttable[,"q05"], n=1))^2
}

#' Function for setting catch, when zero catch gives prediction above blim
#'
#' @param captool_run output from captool function
#' @param catchgrid grid of potential catch quotas when optimize = FALSE, else min and max of catchgrid values are used as limits for optim.
#' @param catch.distribution vector of length 3 given ratio of catch in jan, feb and mar
#' @param optimize boolean, if TRUE uses optim to find exact catch
#' @param blim Blim
#' @param seed integer, for not changing the random input
#' @param ...
#'
#' @return captool object
#' @export
#'
#' @examples
#' \dontrun{opt <- grid.search.catch(run)}
grid.search.catch <- function(captool_run,
                              catchgrid = seq(0,0.1,0.02),
                              catch.distribution = c(0,0.3,0.7),
                              optimize = TRUE,
                              blim = 0.2,seed = 1, ...){
  if(any(catchgrid<0))
    stop("Do not allow catches to be negative.")
  if(!optimize){
    dv <- sapply(catchgrid, q05ofcatch,
                 data_list =captool_run$data_list,
                 seed = seed,
                 nsim = captool_run$captooloptions$nsim,
                 cod_cv = captool_run$captooloptions$cod_cv,
                 cap_cv = captool_run$captooloptions$cap_cv)
    plot(catchgrid, dv, xlab = "Total catch",
         ylab = paste0("Square deviation from Blim = ", blim))
    abline(v=catchgrid[which.min(unlist(dv))], lty = 2, col = 2)
    abline(h=min(unlist(dv)), lty = 2, col = 2)

  captool_run$data_list$catches <-catchgrid[which.min(unlist(dv))] * catch.distribution
  }else{
    opt <- stats::optim(par = min(catchgrid),
                        fn = q05ofcatch,
                        lower = min(catchgrid),
                        upper = max(catchgrid),
                        method = "L-BFGS-B",
                        # Inputs to q05ofcatch:
                        data_list =captool_run$data_list,
                        seed = seed,
                        nsim = captool_run$captooloptions$nsim,
                        cod_cv = captool_run$captooloptions$cod_cv,
                        cap_cv = captool_run$captooloptions$cap_cv)
    if(opt$convergence==0){
      cat("Converged with total catch: ", round(opt$par*1000,1), " kt")
    }else{
      stop("Catch routine did not converge. Try changing the catchgrid limits.")
    }
    captool_run$exact_advice <- opt$par
    captool_run$data_list$catches <- floor(opt$par*1000)/1000 * catch.distribution
  }
  run <- captool(data_list = captool_run$data_list,
                 nsim = captool_run$captooloptions$nsim,
                 cod_cv = captool_run$captooloptions$cod_cv,
                 cap_cv = captool_run$captooloptions$cap_cv)
  if(optimize)
    run$exact_advice <- opt$par
  return(run)
}
