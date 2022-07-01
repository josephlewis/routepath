#' Global Weighted Linear Combination
#'
#' @param cost_surface cost surface
#'
#' @param constrains Boolean vector of adjacent cells not traversable
#'
#' @param fun An R function to apply to the standardised transition values
#'
#' @param global_weight Importance of factor
#'
#' @param neighbours xxx
#'
#' @return Global Weighted Linear Combination
#'
#' @author Joseph Lewis
#'
#' @export

global_wlc <- function(cost_surface, constrains = NULL, global_weight, neighbours, fun = NULL) {

  # remove explicit zeroes from the sparse matrix. Ensures scaling is not impacted by zero values
  cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)

  # rescale to between 0 and 1
  cost_surface <- rescale_cs_global(cost_surface = cost_surface, constrains = constrains, neighbours = neighbours, fun = fun)

  # multiply by user-supplied criterion global weight
  cost_surface <- cost_surface * global_weight

  return(cost_surface)

}
