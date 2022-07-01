#' Local Weighted Linear Combination
#'
#' @param cost_surface cost surface
#'
#' @param constrains Boolean vector of adjacent cells not traversable
#'
#' @param neighbours xxx
#'
#' @param fun xxx
#'
#' @param global_weight Importance of factor
#'
#' @return Local Weighted Linear Combination
#'
#' @author Joseph Lewis
#'
#' @export

local_wlc <- function(cost_surface, constrains = NULL, global_weight, neighbours, fun = NULL) {

  # remove explicit zeroes from the sparse matrix. Ensures scaling is not impacted by zero values
  cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)

  local_cs <- rescale_cs_local(cost_surface = cost_surface, constrains = constrains, neighbours = neighbours, fun = fun)
  local_weights <- local_weights_cs(cost_surface = cost_surface, constrains = constrains, neighbours = neighbours, global_weight = global_weight)

  cost_surface <- local_cs * local_weights

  return(cost_surface)

}
