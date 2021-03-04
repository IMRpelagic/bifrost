#' Calculate SSB from fit (all ages)
#'
#' @param fit maturity object,
#' @param perage logical, should the SSB be reported by age?
#'
#' @return SSB, either as numeric vector per age or total given by perage argument.
#' @export
#'
#' @examples \dontrun{
#' calcSSB(fit, perage = TRUE)
#' }
#'
calcSSB <- function(fit, perage = FALSE){
  # pick out latest year
  idx <- (fit$data$end_year-fit$data$start_year)*fit$data$length_l+
    1:fit$data$length_l
  NumLength <- fit$NumbersAtLength[idx, ]
  m <- fit$obj$report()$m # maturity
  mw <-fit$data$meanweight # weight
  ssbatage <- (m*mw) %*% (NumLength) # calculate SSB
  if(perage) return(ssbatage)
  else return(c(SSB = sum(ssbatage)))
}

#' Captool simulations
#'
#' @importFrom magrittr %>%
#'
#' @param fit maturity object
#' @param p parameter vector of length 3, p1, p2 and p3. Defaults to values used in 2011.
#' @param nsim number of simulations
#' @param useEst If TRUE, the parameters from fit is used. If FALSE the specified p-vector is used.
#' @param cv numeric, coefficient of variation.
#'
#' @return list containing simulations and quantiles (50%, 75%, 95%, 25% and 5%).
#' @export
#'
#' @examples \dontrun{
#' captoolSim(mFit, nsim = 15000)
#' }
#'
captoolSim <- function(fit, p = c(3.5, 13.87, 0.1),
                       nsim = 15000,
                       cv=0.2,
                       useEst = FALSE){
  idx <- (fit$data$end_year-fit$data$start_year)*fit$data$length_l+
    1:fit$data$length_l
  NumLength <- fit$NumbersAtLength[idx, ]
  ml <- fit$data$meanlength
  mw <- fit$data$meanweight
  mat <- matrix(0, ncol = nsim, nrow = 4)
  if(useEst) p <- fit$sumsdrep[5:7, 1]
   M2 <- (mw*maturing(ml,p[1],p[2]))%*%NumLength
  for(i in 1:nsim){
    p2 <- stats::rnorm(1, p[1], .025*p[1])
    p3 <- stats::rnorm(1,p[3], sd = .2*p[3])
    for(t in 1:4){
      mat[t,i] <- sum( ((mw*maturing(ml,p[1],p2))%*%NumLength )*exp(-(t-1)*(p3)) +
                         stats::rnorm(length(M2), mean = 0, sd = ((mw*maturing(ml,p[1],p2))%*%NumLength )*cv))
      #mat[t,i] <- sum(stats::rnorm(length(M2), mean = M2*exp(-(t-1)*(p3)), sd = M2*.2))
    }
  }
  return <- list()
  return$fit <- fit
  df <- as.data.frame(mat)
  names(df)<- paste0("sim", 1:nsim)
  df$date <- as.Date(paste(fit$data$end_year+c(0,0,0,1), c(10:12, "01"),
                            "01", sep = "-"))
  df <- tidyr::pivot_longer(df, cols = 1:nsim,
                            names_to = "sim", values_to = "value")
  return$simulations <- df

  df2<-df %>% dplyr::group_by(date) %>% dplyr::summarize(
    "q50" = stats::median(value),
    "q75" = stats::quantile(value, .75),
    "q95" = stats::quantile(value, .95),
    "q25" = stats::quantile(value, .25),
    "q05" = stats::quantile(value, .05)) %>%
    tidyr::pivot_longer(cols = 2:6,
                        names_to = "quant", values_to = "value")
  return$quantiles <- df2
  attr(return, "class") <- c("captoolSim","list")
  return
}

#' Plot captool projection (1Oct - 1Jan)
#'
#' @param x simulation object
#' @param all logical, if TRUE all simulations are plotted with median in red, else only quantiles.
#' @param ... additional arguments
#'
#' @return ggplot object
#' @export
#'
#' @examples \dontrun{
#' plot(sim)
#' plot(sim, all = TRUE)
#' }
plot.captoolSim <- function(x, all = FALSE, ...){
  if(!all){
  quantwide <- tidyr::pivot_wider(x$quantiles,
                                  id_cols = 1,
                                  names_from = "quant",
                                  values_from="value")
  print(
    ggplot2::ggplot(quantwide,
                    ggplot2::aes_string(x = "date", y = "q50"))+
      ggplot2::geom_ribbon( ggplot2::aes_string(ymin = "q05", ymax = "q95"),
                            fill = "darkgreen")+
      ggplot2::geom_ribbon( ggplot2::aes_string(ymin = "q25", ymax = "q75"),
                            fill = "red")+
      ggplot2::geom_line(col = "yellow", lwd = 2)+
      ggplot2::scale_y_continuous(name = "SSB",
                                  limits= c(0,max(dplyr::select(quantwide, -date))),
                                  breaks = seq(0, max(dplyr::select(quantwide,-date)), 500))+
      ggplot2::scale_x_date(breaks = seq(min(quantwide$date),
                                         max(quantwide$date),
                                         by = "1 months"),
                            date_labels = "%b-%Y")
  )
  }else{
    quantwide <- tidyr::pivot_wider(x$quantiles,
                                    id_cols = 1,
                                    names_from = "quant",
                                    values_from="value")
    print(
       ggplot2::ggplot(x$simulations, ggplot2::aes_string(x = "date", y = "value"))+
         ggplot2::geom_line(lty = 2, ggplot2::aes_string(group = "sim")) +
         ggplot2::geom_line(data = dplyr::filter(x$quantiles, quant == "q50"),
                   ggplot2::aes_string(x="date", y = "value"), col = "red", lwd = 2)+
         ggplot2::scale_x_date(breaks = seq(min(quantwide$date),
                                            max(quantwide$date),
                                            by = "1 months"),
                               date_labels = "%b-%Y")
    )
  }
}


#' Run full simulation from 1.October - 1.April
#'
#' @param mFit maturity fit object
#' @param cFit consumption fit object
#' @param catches catches data
#' @param nsim Number of simulations
#'
#' @return object of type captoolSim
#' @export
#'
#' @examples
#' \dontrun{
#' data(catch)
#' catches <- colSums(catch[catch$year == 2010, c("spring01", "spring05", "spring04")])
#' fSim <- runFullSim(mFit = mFit, cFit = cFit, cathces = catches, nsim = 15000)
#' plot(fSim)}
runFullSim <- function(mFit, cFit, catches, nsim = 15000) {
  Cmax0 <- cFit$sumsdrep["Cmax",1]
  Chalf0 <- cFit$sumsdrep["Chalf",1]
  alpha <- cFit$sumsdrep["alpha",1]
  beta <- cFit$sumsdrep["beta",1]
  # Simulate from 1oct to 1jan:
  sim <- captoolSim(mFit,  useEst = FALSE, nsim = nsim)
  # Extract SSB at 1jan:
  M <- Cod <- K <- numeric(18)
  SSBmc <- matrix(0, nrow = nsim, ncol = 19)
  SSBmc[,1] <- sim$simulations[lubridate::year(sim$simulation$date) == mFit$data$end_year+1,]$value
  Zco <- (cFit$data$Fco + cFit$data$Mco)[rownames(cFit$data$Fco) == mFit$data$end_year,]
  Nco <- cFit$data$Nco[rownames(cFit$data$Nco) == mFit$data$end_year, ]
  CodSuit <- cFit$obj$report()$CodSuit
  Oco <- cFit$data$Oco[rownames(cFit$data$Nco) == mFit$data$end_year, ]
  SV <- cFit$data$SV[rownames(cFit$data$Nco) == mFit$data$end_year, ]
  Wco <- cFit$data$Wco[rownames(cFit$data$Nco) == mFit$data$end_year, ]
  catcheswithzeros <- numeric(3*6)
  catcheswithzeros[seq(3,18, by  = 6)] <- catches

  for(i in 1:nsim){
    for(t in 1:18){
      Cod[t] <- sum(Nco * CodSuit *(1 - SV)*(1-Oco)*Wco^(0.801)*exp(-t*Zco/(12*6)) )
      K[t] <- Cmax0 * Cod[t] * SSBmc[i,t] / (Chalf0 + SSBmc[i,t])
      M[t] <- -log(1-K[t]/SSBmc[i,t])
      SSBmc[i,t+1] <- SSBmc[i,t] * exp(-M[t]) - catcheswithzeros[t]
    }
  }
  SSBmc.df <- as.data.frame(t(SSBmc[,1+seq(6,18,6)]))
  names(SSBmc.df)<- paste0("sim", 1:nsim)
  SSBmc.df$date <- seq(as.Date(paste0(mFit$data$end_year+1,"-02-01")),
                 as.Date(paste0(mFit$data$end_year+1,"-04-01")),
                 by = "month")
  SSBmc.df <- tidyr::pivot_longer(SSBmc.df, cols = 1:nsim,
                            names_to = "sim", values_to = "value")

  return <- list()
  return$mFit = mFit
  return$cFit = cFit
  return$simulations <- rbind(sim$simulations, SSBmc.df)
  return$quantiles <- rbind(sim$quantiles,
                            SSBmc.df %>% dplyr::group_by(date) %>% dplyr::summarize(
    q50 = stats::median(value),
    q75 = stats::quantile(value, .75),
    q95 = stats::quantile(value, .95),
    q25 = stats::quantile(value, .25),
    q05 = stats::quantile(value, .05)) %>%
    tidyr::pivot_longer(cols = 2:6,
                        names_to = "quant", values_to = "value")
  )
  attr(return, "class") <- c("captoolSim","list")
  return(return)

}
