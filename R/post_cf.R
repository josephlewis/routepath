#' plot cost functions based on posterior parameter values
#'
#' @param routepaths \code{routepath}
#'
#' @param index \code{numeric} index of posterior parameter values to be used in supplied function
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

post_cf <- function(routepaths, index, FUN, draws, type = "conductance", normalise = TRUE){

  math_slope <- seq(-0.9, 0.9, 0.01)

  post_vals <- apply(X = routepaths$routes$p[routepaths$routes$result == "Accept",index], MARGIN = 2, FUN = function(x) { sample(x, size = draws, replace = TRUE)})

  if(inherits(post_vals, "numeric")) {
    post_vals <- matrix(post_vals, nrow = 1, ncol = length(post_vals))
  }

  cf_list <- list()

  for(i in 1:draws) {

    val_list <- as.list(post_vals[i,])
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
    ggplot2::geom_line(ggplot2::aes_string(x = "slope", y = "vals", group = "draws"), colour = "#DCBCBC") +
    ggplot2::labs(x = "Mathematical Slope", y = y_lab, title = "posterior cost function") +
    ggplot2::scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.9, 0.9, length.out = 10)) +
    ggplot2::theme_classic()

}
