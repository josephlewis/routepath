#' Extract Start and End Points from known_route
#'
#' This function extracts the start and end points from the supplied known_route.
#'
#' @param known_route sp Spatialknown_route
#'
#' @return sp points of Start and End of supplied known_route
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import sp
#' @import methods

extract_end_points <- function(known_route) {

  route_points <- as(known_route, "SpatialPoints")

  end_points <- route_points[c(1,length(route_points)),]

  return(end_points)
}
