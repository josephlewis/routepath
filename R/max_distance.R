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

max_distance <- function(routepaths, lines, matrix){

  distances <- foreach(index=iterators::iter(matrix, by='row'), .combine = "c") %do% {

    max_dist <- max(rgeos::gDistance(spgeom1 = as(routepaths[index[1], ], "SpatialPoints"), spgeom2 = lines[index[2], ], byid = TRUE))

  }

  return(distances)

}
