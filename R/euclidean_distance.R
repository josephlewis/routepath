#' Calculates route path from start and end locations of known_routes using user-supplied cost surface
#'
#' This function calculates the route path from the start and end locations of known_routes using a user-supplied cost surface
#'
#' @param cost_surface TransitionMatrix
#'
#' @param locations Start and End SpatialPoints returned from extract_end_points()
#'
#' @return Spatialknown_routes of route paths from start to end locations
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import foreach
#' @import leastcostpath

euclidean_distance <- function(routes, known_routes) {

    distances <- foreach(row_no = 1:nrow(known_routes), .combine = "c") %do% {

      line_pts <- as(routes[row_no, ], "SpatialPoints")

      max_dist <- max(base::suppressWarnings(rgeos::gDistance(spgeom1 = line_pts, spgeom2 = known_routes[row_no, ], byid = TRUE)))

    }

    return(distances)

}
