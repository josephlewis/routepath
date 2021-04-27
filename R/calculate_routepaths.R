#' Calculates route path from start and end locations of lines using user-supplied cost surface
#'
#' This function calculates the route path from the start and end locations of lines using a user-supplied cost surface
#'
#' @param cost_surface TransitionMatrix
#'
#' @param locations Start and End SpatialPoints returned from extract_end_points()
#'
#' @return SpatialLines of route paths from start to end locations
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import foreach
#' @import leastcostpath

calculate_routepaths <- function(cost_surface, locations) {

    loc_matrix <- base::matrix(c(from = base::seq(from = 1, to = base::length(locations), by = 2), to = base::seq(from = 2, to = base::length(locations), by = 2)), ncol = 2)

    routepaths <- leastcostpath::create_lcp_network(cost_surface = cost_surface, locations = locations, nb_matrix = loc_matrix)

    proj4string(routepaths) <- proj4string(locations)

    return(routepaths)

}
