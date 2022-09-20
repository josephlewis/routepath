#' binds multiple routepath objects into one
#'
#' @param x \code{list} of routepath objects
#'
#' @return \code{routepath}
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'

bind_routepaths <- function(x) {

  if(!all(unlist(lapply(x, FUN = function(x) { inherits(x, "routepath")})))) {
    stop("Invalid x argument. Expecting list containing only routepath objects")
  }

  routepaths <- x[[1]]
  routepaths$routes <- do.call(rbind, lapply(x, FUN = function(x) { x$routes}))

  class(routepaths) <- "routepath"

  return(routepaths)

}
