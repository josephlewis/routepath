#' Calculate Hausdorff distance between simulated route path and known route
#'
#' @param routes a SpatialLines object of the simulated route
#'
#' @param known_route a SpatialLines object of the known route to be used when comparing against the simulated route paths
#'
#' @return Hausdorff distance between simulated route path and known route
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import rgeos
#' @import sp
#' @import methods
#'
hausdorff_distance <- function(routes, known_route) {

  routes_sf <- sf::st_as_sf(routes)
  known_route <- sf::st_as_sf(known_route)

  sf::st_crs(routes_sf) <- sf::st_crs(known_route)

  distance <- as.numeric(sf::st_distance(routes_sf, known_route, which = "Hausdorff"))

  return(distance)

}
