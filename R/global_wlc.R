#' Global Weighted Linear Combination
#'
#' @param cost_surface cost surface
#'
#' @param p risk
#'
#' @param constrains Boolean vector of adjacent cells not traversable
#'
#' @param global_weight Importance of factor
#'
#' @return Global Weighted Linear Combination
#'
#' @author Joseph Lewis
#'
#' @export

global_wlc <- function(cost_surface, p = 1, constrains = NULL, global_weight) {

  if (p < 1) {stop("p must be equal or greater than 1")}

  if (global_weight > 0 & global_weight <= 1) {stop("global weight must be greater than 0 and less than or equal to 1")}

  # remove explicit zeroes from the sparse matrix. Ensures scaling is not impacted by zero values
  cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)

  # rescale to between 0 and 1
  cost_surface <- rescale_cs_global(cost_surface = cost_surface, p = p, constrains = constrains)
  # multiply by user-supplied criterion global weight
  cost_surface <- cost_surface * global_weight

  cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)

  return(cost_surface)

}
