#' Calculates route path from start and end locations of lines using user-supplied cost surface
#'
#' calculates the route path from the start and end locations of lines using a user-supplied cost surface
#'
#' @param x \code{conductanceMatrix}
#'
#' @param locations \code{sf points} origin and destination points
#'
#' @return \code{sf line} least-cost path from origin to destination
#'
#' @keywords internal
#'
#' @author Joseph Lewis

calculate_routepath <- function(x, locations) {

  route <- leastcostpath::create_lcp(x = x, origin = locations[1,], destination = locations[2,], cost_distance = FALSE)

  return(route)

}
