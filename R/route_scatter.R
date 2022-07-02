#' plot scatter plot of simulated routepath parameters
#'
#' @param routes simulated routes
#'
#' @param col_index column index of parameters to plot. if NULL then plot all parameters
#'
#' @param xlab_names xxxx
#'
#' @param alpha xxxx
#'
#' @author Joseph Lewis
#'
#' @export

route_scatter <- function(routes, col_index = NULL, xlab_names = NULL, alpha = 1) {

  # need to add filename export functionality

  routes <- routes[routes$result == "Accept",]

  if(nrow(routes[routes$result == "Accept",]) == 0) {
    stop("No accepted simulated routes. Increase threshold value of acceptance using change_tolerance()")
  }

  col_names <- colnames(routes)
  cols <- col_names[col_index]

  if(!is.null(xlab_names)) {
    x_labs <- xlab_names
    if (length(x_labs) == 1) {
      x_labs <- rep(x_labs, length.out = length(cols))
    }
  } else  {
    x_labs <- cols
  }

  plot(routes[[cols[[1]]]], routes[[cols[[2]]]], xlab = x_labs[1], ylab = x_labs[2], pch = 16, col = grDevices::rgb(0,0,0, alpha = alpha))

}
