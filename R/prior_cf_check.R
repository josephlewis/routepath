#' plot cost function realisations based on supplied prior parameter values and a function
#'
#' @details xxx
#'
#' @param priors \code{Matrix} prior parameter values
#'
#' @param FUN \code{function} function applied to prior parameter values. Supplied function arguments should be the same name as the prior matrix
#'
#' @param draws \code{numeric} Number of samples to draw from prior parameter values. Default is 100
#'
#' @param type \code{type} 'conductance' (default) or 'cost'. if 'cost' then 1/conductance
#'
#' @param normalise \code{logical} if TRUE (default) then cost functions scaled to between 0 and 1. if FALSE then cost functions not scaled
#'
#' @param limits \code{numeric} limits of mathematical slope values supplied to FUN. This also controls the X axis limits of the plot
#'
#' @param alpha \code{numeric} colour transparency of cost functions. Expected value from 0 to 1 (default is 1)
#'
#' @return \code{ggplot2} prior cost functions
#'
#' @author Joseph Lewis
#'
#' @export
#'
#' @examples
#'
#'priors <- cbind(a = 1, b = rnorm(n = 100, mean = 3.5, sd = 1),
#'c = rnorm(n = 100, mean = 0.05, sd = 0.2))
#'FUN <- function(x,a,b,c) {(a * exp(-b * abs(x + c))) / 3.6}
#'
#'prior_cf_check(priors = priors, FUN = FUN, draws = 10,
#'type = "conductance", normalise = TRUE)

prior_cf_check <- function(priors, FUN, draws = 100, type = "conductance", normalise = TRUE, limits = 0.9, alpha = 1) {

  math_slope <- seq(-(limits), limits, 0.01)

  if(nrow(priors) > 1) {
    priors <- apply(X = priors, MARGIN = 2, FUN = function(x) { sample(x, size = draws, replace = TRUE)})
  } else {
    draws <- 1
  }

  cf_list <- list()

  for(i in 1:draws) {

    val_list <- as.list(priors[i,])
    val_list$x <- math_slope

    cf_vals <- do.call(FUN, val_list)

    cf_list[[i]] <- data.frame(slope = math_slope, vals = cf_vals, draws = i)

  }

  cf_df <- do.call(rbind, cf_list)

  if(type == "cost") {
    cf_df$vals <- 1 / cf_df$vals
    y_lab <- "Cost"
  } else {
    y_lab <- "Conductance"
  }

  if(normalise) {
    cf_df$vals <- (cf_df$vals-min(cf_df$vals))/(max(cf_df$vals)-min(cf_df$vals))
  }

  breaks <- round(sort(c(0, seq(-(limits), limits, by = 0.2))), 1)

  plot <- ggplot2::ggplot(cf_df) +
    ggplot2::geom_vline(xintercept = 0, lty = 2, colour = "grey80") +
    ggplot2::geom_line(ggplot2::aes_string(x = "slope", y = "vals", group = "draws"), colour = "#d1e1ec", alpha = alpha) +
    ggplot2::labs(x = "Mathematical Slope", y = y_lab, title = "Cost function prior predictive check") +
    ggplot2::scale_x_continuous(limits = c(-(limits), limits), breaks = breaks) +
    ggplot2::theme_classic()

  return(plot)
}
