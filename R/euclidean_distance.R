#' Calculate euclidean distance from simulated route path to known route
#'
#' @param routes a SpatialLines object of the simulated route
#'
#' @param known_route a SpatialLines object of the known route to be used when comparing against the simulated route paths
#'
#' @return Euclidean distance from simulated route path to known route
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import rgeos
#' @import sp
#' @import methods

euclidean_distance <- function(routes, known_route) {

  line_pts <- as(routes, "SpatialPoints")

  distance <- base::max(base::suppressWarnings(rgeos::gDistance(spgeom1 = line_pts, spgeom2 = known_route, byid = TRUE)))

  return(distance)

}
