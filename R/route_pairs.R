#' plot pair plot of simulated routepath parameters
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

route_pairs <- function(routes, col_index = NULL, xlab_names = NULL, alpha = 1) { 
  
  routes <- routes[routes$result == "Accept",]
  
  if(nrow(routes[routes$result == "Accept",]) == 0) {
    stop("No accepted simulated routes. Increase threshold value of acceptance using change_tolerance()")
  }
  
  if(is.null(col_index)) {
    col_names <- colnames(routes)
    cols <- c(col_names[4:(which(col_names == "stats") - 1)])
  } else {
    if(length(col_index) == 1) { 
      stop("only one column in the argument col_index. route_pairs requires at least two columns")}
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
  
  if(inherits(routes, "sf")) { 
    routes <- sf::st_drop_geometry(routes)
  }
  
  panel.hist <- function(x, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="white")
  }
  
  pairs(routes[,cols], pch = 16, col = rgb(0,0,0, alpha = alpha), diag.panel=panel.hist)
  
}