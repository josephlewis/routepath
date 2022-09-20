#' Calculate euclidean distance from simulated route path to known route
#'
#' @param route a sf object of the simulated route
#'
#' @param known_route a sf object of the known route to be used when comparing against the simulated route paths
#'
#' @return Euclidean distance from simulated route path to known route
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'

euclidean_distance <- function(route, known_route) {

  route_pts <- sf::st_cast(route, "POINT", warn = FALSE)

  distance <- max(sf::st_distance(x = route_pts, y = known_route))
  distance <- as.numeric(distance)

  return(distance)

}

#' Calculate path deviation index from simulated route path to known route
#'
#' @param route a SpatialLines object of the simulated route
#'
#' @param known_route a SpatialLines object of the known route to be used when comparing against the simulated route paths
#'
#' @return Normalised Path Deviation Index of the simulated route path from the known route
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'

pdi_distance <- function(route, known_route) {

  pdi <- leastcostpath::PDI_validation(lcp = route, comparison = known_route)
  distance <- pdi$normalised_pdi

  return(distance)

}

#' Calculate Frechet distance between simulated route path and known route
#'
#' @param route a SpatialLines object of the simulated route
#'
#' @param known_route a SpatialLines object of the known route to be used when comparing against the simulated route paths
#'
#' @return Frechet distance between simulated route path and known route
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'

frechet_distance <- function(route, known_route) {

  distance <- sf::st_distance(x = route, y = known_route, "Frechet")
  distance <- as.numeric(distance)

  return(distance)

}

#' Calculate Hausdorff distance between simulated route path and known route
#'
#' @param route a SpatialLines object of the simulated route
#'
#' @param known_route a SpatialLines object of the known route to be used when comparing against the simulated route paths
#'
#' @return Hausdorff distance between simulated route path and known route
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'

hausdorff_distance <- function(route, known_route) {

  distance <- sf::st_distance(x = route, y = known_route, "Hausdorff")
  distance <- as.numeric(distance)

  return(distance)

}
