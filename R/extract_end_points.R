#' Extract Start and End SpatialPoints from SpatialLines
#'
#' This function extracts the start and end points from the supplied lines.
#'
#' @param lines SpatialLines
#'
#' @return SpatialPoints of Start and End of supplied lines
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import foreach
#' @import sp
#' @import methods

extract_end_points <- function(lines) {

    end_points <- foreach::foreach(line_no = 1:length(lines), .combine = rbind) %do% {
        sp::SpatialPoints(sp::coordinates(methods::as(lines[line_no, ], "SpatialPoints"))[c(1, nrow(sp::coordinates(methods::as(lines[line_no, ], "SpatialPoints")))), ])
    }

    return(end_points)
}
