#' Calculates route path from start and end locations of lines using user-supplied cost surface
#'
#' This function calculates the route path from the start and end locations of lines using a user-supplied cost surface
#'
#' @param cost_surface Cost Surface denoting ease of traversing landscape
#'
#' @param locations Start and End SpatialPoints
#'
#' @return SpatialLines of route paths from start to end locations
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import leastcostpath
#' @import sp
#' @import methods

calculate_routepaths <- function(cost_surface, locations) {

    routepaths <- leastcostpath::create_lcp(cost_surface = cost_surface, origin = locations[1,], destination = locations[2,], directional = TRUE, cost_distance = FALSE)

    raster::crs(routepaths) <- raster::crs(cost_surface)

    return(routepaths)

}
