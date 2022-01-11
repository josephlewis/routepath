#' Local Weighted Linear Combination
#'
#' @param cost_surface cost surface
#'
#' @param p risk
#'
#' @param constrains Boolean vector of adjacent cells not traversable
#'
#' @param win Window
#'
#' @param global_weight Importance of factor
#'
#' @return Local Weighted Linear Combination
#'
#' @author Joseph Lewis
#'
#' @import Matrix
#'
#' @export
local_wlc <- function(cost_surface, p = 1, constrains = NULL, win, global_weight) {

  local_cs <- rescale_cs_local(cost_surface = cost_surface, p = p, constrains = constrains, win = win)
  local_weights <- local_weights_cs(cost_surface = cost_surface, p = p, constrains = constrains, win = win, global_weight = global_weight)

  cost_surface <- local_cs * local_weights

  cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)

  return(cost_surface)

}
