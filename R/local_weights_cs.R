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
#' @export

local_weights_cs <- function(cost_surface, p = 1, constrains = NULL, win, global_weight) {

  if (p <= 1) {stop("p must be equal or greater than 1")}

  # if(!inherits(constrains, "logical")) {stop("constraints must be a logical vector")}

  if(!inherits(win, "matrix")) {stop("win must be a matrix. See raster::adjacent for details on neighourbood matrix")}

  if(global_weight > 1 | global_weight <= 0) {stop("global weight must be greater than 0 and less than or equal to 1")}

  cs_adj <- gdistance::adjacencyFromTransition(cost_surface)
  gw <- global_weight

  if (!is.null(constrains)) {
    gr <- base::max(cost_surface[cs_adj][!constrains]) - base::min(cost_surface[cs_adj][!constrains])
  } else {
    gr <- base::max(cost_surface[cs_adj]) - base::min(cost_surface[cs_adj])
  }

  rast <- raster::raster(cost_surface)
  adj_rast <- raster::adjacent(rast, cells=1:raster::ncell(rast), pairs=TRUE, directions=win, include = TRUE, sorted = TRUE)

  if (!is.null(constrains)) {
    adj_rast2 <- base::rbind(adj_rast, cs_adj[constrains,])
    adj_rast <- adj_rast2[!(base::duplicated(adj_rast2) | base::duplicated(adj_rast2, fromLast = TRUE)), ]
  }

  rast_vals <- rast[adj_rast[,2]]
  rast_vals_mat <-  base::cbind(adj_rast, rast_vals)

  local_weights <- as.numeric(tapply(rast_vals_mat[,3],rast_vals_mat[,1], function(x) { ((gw*(max(x) - min(x))) / (gr))}))

  #local_weights <- stats::aggregate(rast_vals ~ from, data = rast_vals_mat, FUN = function(x) { ((gw*(max(x) - min(x))) / (gr))})
  #local_weights <- base::unlist(local_weights)

  cost_surface@transitionMatrix@x <- base::rep(local_weights, times = diff(cost_surface@transitionMatrix@p))

  if (!is.null(constrains)) {
    cost_surface[cs_adj][constrains] <- 0
    cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)
  }

  return(cost_surface)

}
