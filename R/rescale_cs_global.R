#' Rescales cost surface based on global range of transition values
#'
#' @param cost_surface cost surface
#'
#' @param constrains Logical vector of adjacent cells not traversable
#'
#' @param neighbours xxx
#'
#' @param fun An R function to apply to the standardised transition values
#'
#' @return cost suface standardised to a maximum value of 1 and minimum value relative to 0
#'
#' @author Joseph Lewis
#'
#' @export

rescale_cs_global <- function(cost_surface, constrains = NULL, neighbours, fun = NULL) {

  cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)

  rast_cs <- suppressWarnings(raster::raster(cost_surface))

  cs_adj <- raster::adjacent(rast_cs, cells = 1:raster::ncell(rast_cs),
                             pairs = TRUE, directions = neighbours)

  if(inherits(constrains, "logical"))  {
    cost_surface[cs_adj][constrains] <- 0
  }

  cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)

  max_val <- base::max(cost_surface@transitionMatrix@x)

  cost_surface@transitionMatrix@x <- (((c(0, cost_surface@transitionMatrix@x) - 0) / (max_val - 0)))[-1]

  if(inherits(fun, "function"))  {
    cost_surface@transitionMatrix@x <- fun(cost_surface@transitionMatrix@x)
  }

  return(cost_surface)
}
