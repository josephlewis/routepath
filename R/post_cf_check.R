#' plot cost functions based on posterior parameter values
#'
#' @param routepath \code{routepath}
#'
#' @param FUN \code{function} function applied to prior parameter values. Supplied function arguments should be the same name as the prior matrix
#'
#' @param draws \code{numeric} Number of samples to draw from posterior parameter values. Default is 100
#'
#' @param type \code{type} 'conductance' (default) or 'cost'. if 'cost' then 1/conductance
#'
#' @param normalise \code{logical} if TRUE (default) then cost functions scaled to between 0 and 1. if FALSE then cost functions not scaled
#'
#' @param limits \code{numeric} limits of mathematical slope values supplied to FUN. This also controls the X axis limits of the plot
#'
#' @param alpha \code{numeric} colour transparency of cost functions. Expected value from 0 to 1 (default is 1)
#'
#' @return \code{ggplot2} post cost functions
#'
#' @author Joseph Lewis
#'
#' @export

post_cf_check <- function(routepath, FUN, draws = 100, type = "conductance", normalise = TRUE, limits = 0.9, alpha = 1) {

  math_slope <- seq(-(limits), limits, 0.01)

  if(inherits(routepath$routes, "sf")) {
    routes <- sf::st_drop_geometry(routepath$routes)
  } else {
    routes <- routepath$routes
    }

  param_cols <- which(grepl(pattern = "p.", colnames(routes), fixed = TRUE))
  post_vals <- routes[routes$result == "Accept", param_cols]

  if(nrow(post_vals) == 0) {
    stop("Unable to create a posterior cost function as no simulations are accepted. Use update_tolerance() to modify tolerance required for simulation to be 'Accepted'")
  }

  if(nrow(post_vals) > 1) {
    post_vals <- apply(X = post_vals, MARGIN = 2, FUN = function(x) { sample(x, size = draws, replace = TRUE)})
  } else {
    draws <- 1
  }

  colnames(post_vals) <- base::gsub(pattern = "p.", replacement = "", x = colnames(post_vals))

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

  breaks <- round(sort(c(0, seq(-(limits), limits, by = 0.2))), 1)

  plot <- ggplot2::ggplot(cf_df) +
    ggplot2::geom_vline(xintercept = 0, lty = 2, colour = "grey80") +
    ggplot2::geom_line(ggplot2::aes_string(x = "slope", y = "vals", group = "draws"), colour = "#DCBCBC", alpha = alpha) +
    ggplot2::labs(x = "Mathematical Slope", y = y_lab, title = "Cost function posterior predictive check") +
    ggplot2::scale_x_continuous(limits = c(-(limits), limits), breaks = breaks) +
    ggplot2::theme_classic()

  return(plot)

}
