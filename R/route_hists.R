#' plot scatter plot of simulated routepath parameters
#'
#' @param routes simulated routes
#'
#' @param col_index column index of parameters to plot. if NULL then plot all parameters
#'
#' @param xlab_names xxxx
#'
#' @param ... Additional arguments. See Details
#'
#' @author Joseph Lewis
#'
#' @export

route_hists <- function(routes, col_index = NULL, xlab_names = NULL, ...) {

  routes <- routes[routes$result == "Accept",]

  if(nrow(routes[routes$result == "Accept",]) == 0) {
    stop("No accepted simulated routes. Increase threshold value of acceptance using change_tolerance()")
  }

  if(is.null(col_index)) {
    col_names <- colnames(routes)
    cols <- c(col_names[4:(which(col_names == "stats") - 1)])
  } else {
    col_names <- colnames(routes)
    cols <- col_names[col_index]
  }

  if(!is.null(xlab_names)) {
    x_labs <- xlab_names
    if (length(x_labs) == 1) {
      x_labs <- rep(x_labs, length.out = length(cols))
    }
  } else  {
    x_labs <- cols
  }

  graphics::par(mfrow = c(ceiling(length(cols)/ceiling(length(cols)/5)), ceiling(length(cols)/5)))

  for(i in 1:length(cols)) {

    param_values <- routes[[cols[i]]]
    graphics::hist(param_values, xlab = "Parameter Value", main = NULL, ...)
    graphics::title(main = x_labs[i], adj = 0)
  }

}

