#' plot cost functions based on prior parameter values
#'
#' @param priors \code{Matrix} prior parameter values
#'
#' @param index \code{numeric} index of prior parameter values to be used in supplied function
#'
#' @param FUN \code{function} function applied to prior parameter values
#'
#' @param draws \code{numeric} Number of samples to draw from prior parameter values
#'
#' @param type \code{type} 'conductance' (default) or 'cost'. if 'cost' then 1/conductance
#'
#' @param normalise \code{logical} if TRUE (default) then cost functions scaled to between 0 and 1. if FALSE then cost functions not scaled
#'
#' @return \code{ggplot2} cost functions based on prior parameter values
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
#'prior_cf(priors = priors, FUN = FUN, index = 1:3, draws = 10,
#'type = "conductance", normalise = TRUE)

prior_cf <- function(priors, index, FUN, draws, type = "conductance", normalise = TRUE) {

  math_slope <- seq(-0.9, 0.9, 0.01)

  param_vals <- priors[,index, drop = FALSE]

  if(nrow(param_vals) > 1) {
    param_vals <- apply(X = param_vals, MARGIN = 2, FUN = function(x) { sample(x, size = draws, replace = TRUE)})
  } else {
    draws <- 1
  }

  cf_list <- list()

  for(i in 1:draws) {

    val_list <- as.list(param_vals[i,])
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

  ggplot2::ggplot(cf_df) +
    ggplot2::geom_vline(xintercept = 0, lty = 2, colour = "grey80") +
    ggplot2::geom_line(ggplot2::aes_string(x = "slope", y = "vals", group = "draws"), colour = "#d1e1ec") +
    ggplot2::labs(x = "Mathematical Slope", y = y_lab, title = "prior cost function") +
    ggplot2::scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.9, 0.9, length.out = 10)) +
    ggplot2::theme_classic()
}
