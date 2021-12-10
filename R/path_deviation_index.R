#' Calculate path deviation index from simulated route path to known route
#'
#' @param routes a SpatialLines object of the simulated route
#'
#' @param known_route a SpatialLines object of the known route to be used when comparing against the simulated route paths
#'
#' @return Normalised Path Deviation Index of the simulated route path from the known route
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import rgeos
#' @import sp
#' @import methods
#'
path_deviation_index <- function(routes, known_route) {

  pdi <- leastcostpath::PDI_validation(lcp = routes, comparison = known_route)
  distance <- pdi$normalised_PDI

  return(distance)

}
