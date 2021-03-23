#' Extract Start and End SpatialPoints from SpatialLines
#'
#' This function extracts the start and end points from the supplied lines.
#'
#' @details xxx
#'
#' @param line SpatialLines
#'
#' @return SpatialPoints of Start and End of supplied lines
#'
#' @author Joseph Lewis
#'
#' @import foreach
#' @import sp
#' @import methods
#'
#' @export
#'

extract_end_points <- function(line) {

  utils::globalVariables("i")

  end_points <- foreach::foreach(i = 1:length(line), .combine = rbind) %do%
    {
      sp::SpatialPoints(sp::coordinates(methods::as(line[i,], "SpatialPoints"))[c(1,nrow(sp::coordinates(methods::as(line[i,], "SpatialPoints")))),])
    }

  return(end_points)
}
