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

#' @rdname plot.maturity
#' @export
plot.maturitybyyears <- function(x, plot=TRUE,...){
  length_l <- x$estimation[[1]]$data.list$length_l
  meanlength <- x$estimation[[1]]$data.list$meanlength
  p1  <- x$results$p1
  p2  <- x$results$p2
  r = sapply(1:length(p1), FUN = function(x) maturing(meanlength,p1[x],p2[x]))
  r <- tibble::as_tibble(as.data.frame(t(r)))
  names(r) <- 1:length_l
  r$year <- unlist(lapply(x$estimation, function(x)x$year))
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
#' Plot maturity for all year classes
#'
#' @param x list, Result from runMaturityYearByYear
#' @param plot logical, indicating whether the figure is to be printed
#' @param type either "maturity" or "prediciton".
#' @param ... additional arguments
#'
#' @return ggplot2 plot
#' @export
#'
#' @examples\dontrun{
#' data(cap)
#' data(catch)
#' data(maturityInitialParameters)
#' plot(runMaturityYearByYear(cap, catch=catch,
#' initPar =maturityInitialParameters[maturityInitialParameters$agegr =="2-3",],
#'  min_age = 2, max_age = 3, plot = FALSE))
#'  }
plot.maturity <- function(x, ...){
  plot.summary.maturity(summary(x))
}
#' @rdname plot.maturity
#' @export
plot.summary.maturity <- function(x, type = "maturity",...){
  if(type == "maturity"){
  return(
    ggplot2::ggplot(data = x$maturitytable,
                    ggplot2::aes_string(x = "ml", y = "r")) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::xlab("Mean length") +
      ggplot2::ylab("Maturity rate") +
      ggplot2::theme_bw()
  )
  } else if(type == "prediction"){
    pred <- tibble::tibble(
      "date" = as.Date(paste(x$years[2]+c(0,0,1),c(11:12,"01"),"01", sep = "-")),
      "pred" = x$result.tab[5:7,1],
      "sd" = x$result.tab[5:7,2],
      "upper" = pred + 1.96 * sd,
      "lower" = pred -1.96 * sd
    )
    return(
      ggplot2::ggplot(data = pred,
                      ggplot2::aes_string(x = "date")) +
        ggplot2::geom_line(ggplot2::aes_string(y = "pred")) +
        ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "lower", ymax ="upper")) +
        ggplot2::geom_point() +
        ggplot2::xlab("Mean length") +
        ggplot2::ylab("Maturity rate") +
        ggplot2::theme_bw()
    )
  } else{
    cat("No valid type.\n")
  }

}
