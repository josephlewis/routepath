#' Local Weight
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
#' @return cost suface standardised to a maximum value of 1 and minimum value relative to 0 incorporating local range
#'
#' @author Joseph Lewis
#'
#' @import gdistance
#' @import raster
#'
#' @export

local_weights_cs <- function(cost_surface, p = 1, constrains = NULL, win, global_weight) {

  cs_adj <- gdistance::adjacencyFromTransition(cost_surface)
  gw <- global_weight

  if (!is.null(constrains)) {
    gr <- max(cost_surface[cs_adj][!constrains]) - min(cost_surface[cs_adj][!constrains])
  } else {
    gr <- max(cost_surface[cs_adj]) - min(cost_surface[cs_adj])
  }

  rast <- raster(cost_surface)
  adj_rast <- raster::adjacent(rast, cells=1:ncell(rast), pairs=TRUE, directions=win, include = TRUE, sorted = TRUE)

  if (!is.null(constrains)) {
    adj_rast2 <- rbind(adj_rast, cs_adj[constrains,])
    adj_rast3 <- adj_rast2[!(duplicated(adj_rast2) | duplicated(adj_rast2, fromLast = TRUE)), ]
  }

  rast_vals <- rast[adj_rast[,2]]
  rast_vals_mat <-  cbind(adj_rast, rast_vals)

  local_weights <- stats::aggregate(rast_vals ~ from, data = rast_vals_mat, FUN = function(x) { ((gw*(max(x) - min(x))) / (gr))})
  local_weights <- unlist(local_weights$rast_vals)

  cost_surface@transitionMatrix@x <- rep(local_weights, times = diff(cost_surface@transitionMatrix@p))

  if (!is.null(constrains)) {
    cost_surface[cs_adj][constrains] <- 0
    cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)
  }

  return(cost_surface)

}
