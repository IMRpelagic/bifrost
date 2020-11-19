#' Function for calculating maturity from parameter estimates
#'
#' @param meanlength numeric, mean length per length group
#' @param p1 numeric, p1 estimate
#' @param p2 numeric, p2 estimate
#'
#' @return maturity at length
#' @export
#' @examples maturing(6, 1, 14)
maturing = function(meanlength,p1,p2){
  r = 1.0/(1.0+exp(4*p1*(p2-meanlength)))
  return(r)
}

#' Plot maturity for all year classes
#'
#' @param result list, Result from runMaturityYearByYear
#' @param plot logical, indicating whether the figure is to be printed
#'
#' @return ggplot2 plot
#' @export
#'
#' @examples
#' #' data(cap)
#' data(catch)
#' data(maturityInitialParameters)
#' plotMaturity(runMaturityYearByYear(cap, catch=catch,
#' initPar =maturityInitialParameters, min_age = 3,
#' max_age = 4, plot = FALSE))
plotMaturity <- function(result, plot=TRUE){
  length_l <- result$estimation[[1]]$data.list$length_l
  meanlength <- result$estimation[[1]]$data.list$meanlength
  p1  <- result$results$p1
  p2  <- result$results$p2
  r = sapply(1:length(p1), FUN = function(x) maturing(meanlength,p1[x],p2[x]))
  r <- tibble::as_tibble(as.data.frame(t(r)))
  names(r) <- 1:length_l
  r$year <- unlist(lapply(result$estimation, function(x)x$year))
  r <- tidyr::pivot_longer(r, cols = -("year"), names_to = "lengthgr", values_to = "maturity")
  r$length <- meanlength[as.integer(r$lengthgr)]
  p <- ggplot2::ggplot(r, ggplot2::aes(x = length, y = maturity, group = factor(year), col =year)) +
    ggplot2::geom_line()+
    ggplot2::theme_bw() +
    ggplot2::scale_color_viridis_c()+
    ggplot2::xlab("Year") + ggplot2::ylab("Maturity") +
    ggplot2::guides(color = ggplot2::guide_colorbar(barheight = 20))
  if(plot)
    print(p)
}
