#' Calculates routepath from origin and destination locations of sf lines using user-supplied conductance surface
#'
#' calculates the routepath from the origin and destination locations of lines using a user-supplied conductance surface
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
