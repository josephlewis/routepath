#' Extract Start and End Points from known_routes
#'
#' This function extracts the start and end points from the supplied known_routes.
#'
#' @param known_routes sp Spatialknown_routes
#'
#' @return sp points of Start and End of supplied known_routes
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import foreach
#' @import sp
#' @import methods

extract_end_points <- function(known_routes) {

    end_points <- foreach::foreach(line_no = 1:length(known_routes), .combine = rbind) %do% {
        sp::SpatialPoints(sp::coordinates(methods::as(known_routes[line_no, ], "SpatialPoints"))[c(1, nrow(sp::coordinates(methods::as(known_routes[line_no, ],
            "SpatialPoints")))), ])

    }

    return(end_points)
}
