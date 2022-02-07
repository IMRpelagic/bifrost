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
#'
#' @return list of output from the projection.
#' @export
#'
#' @examples
#' # captool(data_list)
#'
captool <- function(data_list, nsim=5e4, cap_cv=0.2, cod_cv=0.3, plot = TRUE){
  if(!all(c("year", "cap", "catches", "cod0", "cod1", "svalbard", "stochasticHistory") %in% names(data_list)))
    stop("The following is missing from the data_list object: \n",
         paste(c("year", "cap", "catches", "cod0", "cod1", "svalbard", "stochasticHistory")[which(
           !(c("year", "cap", "catches", "cod0", "cod1", "svalbard", "stochasticHistory") %in%
               names(data_list)))], collapse = ", "))
  #nsim=5e4; cap_cv=0.2; cod_cv=0.3
  cind1 <- sample(1:nrow(data_list$stochasticHistory), nsim, replace=T)
  cind2 <- sample(1:ncol(data_list$stochasticHistory[,-(1:10)]), nsim, replace=T)

  # --- 1oct - 1jan ----
  naa <- as.matrix(data_list$cap[,2:6])
  mw <- as.numeric(unlist(data_list$cap[,7]))
  ml <- as.numeric(unlist(data_list$cap[,9]))
  mat <- matrix(0, ncol = nsim, nrow = 4)
  for(i in 1:nsim){
    p1 <- unlist(data_list$stochasticHistory$capelinP1)[cind1[i]]
    p2 <- unlist(data_list$stochasticHistory$capelinP2)[cind1[i]]
    p3 <- unlist(data_list$stochasticHistory[cind1[i],10+cind2[i]])
    M2 <- (mw*bifrost::maturing(ml, p1, p2))%*%  naa
    mat[1,i] <-sum( M2* stats::rnorm(length(M2), mean = 1, sd = cap_cv))
    for(t in 2:4){
      mat[t,i] <- mat[t-1,i] * exp(-p3)
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
  catches <-  0.205 * c(0, .3, .7)
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
      M[t] <- -log(1-K[t]/SSBmc[i,t])
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
  if(plot)
    plotting_validation(return, save = FALSE)
  return(return)
}

#' Plotting output from captool
#'
#' @param return list, returned by captool function
#' @param path character, where to save the figure
#' @param save boolean, should the figure be saved?
#'
#' @return ggplot figure object
#' @export
#'
#' @examples
plotting_validation <- function(return, path = "", save = FALSE){
  quantwide <- tidyr::pivot_wider(return$quantiles,
                                  id_cols = 1,
                                  names_from = "quant",
                                  values_from="value")

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
    ggplot2::ggtitle(return$year)
  if(!is.null(return$data_list$captool)){
   p <- p +
     ggplot2::geom_line(data=return$captool, ggplot2::aes_string(x = "date", y = "Y"), lty = 2, lwd = .9)+
     ggplot2::geom_line(data=return$captool, ggplot2::aes_string(x = "date", y = "`Y+1`"), lty = 2, lwd = .9)+
     ggplot2::geom_line(data=return$captool, ggplot2::aes_string(x = "date", y = "`Y+2`"), lty = 2, lwd = .9)+
     ggplot2::geom_line(data=return$captool, ggplot2::aes_string(x = "date", y = "`Y-1`"), lty = 2, lwd = .9)+
     ggplot2::geom_line(data=return$captool, ggplot2::aes_string(x = "date", y = "`Y-2`"), lty = 2, lwd = .9)
  }
  print(p)
  if(save)
    ggplot2::ggsave(p,paste0(path, "/prediction_",return$year, ".tiff"), device = "tiff",
             width = 10, height = 7, dpi= "retina")
}