#' Local Weight
#'
#' @param cost_surface cost surface
#'
#' @param constrains Boolean vector of adjacent cells not traversable
#'
#' @param neighbours xxxxx
#'
#' @param global_weight Importance of factor
#'
#' @return cost suface standardised to a maximum value of 1 and minimum value relative to 0 incorporating local range
#'
#' @author Joseph Lewis
#'
#' @export

local_weights_cs <- function(cost_surface, constrains = NULL, global_weight, neighbours) {

  cs_rast <- suppressWarnings(raster::raster(cost_surface))

  # calculate adjacent with centre cell included (this is done so that the local value can be assigned later on to the centre cell)
  rast_adj <- raster::adjacent(cs_rast, cells=1:raster::ncell(cs_rast), pairs=TRUE, directions=neighbours, include = TRUE, sorted = TRUE)
  cs_adj <- raster::adjacent(cs_rast, cells=1:raster::ncell(cs_rast), pairs=TRUE, directions=neighbours, include = FALSE, sorted = TRUE)

  gw <- global_weight

  if (!is.null(constrains)) {
    gr <- base::max(cost_surface[cs_adj][!constrains]) - base::min(cost_surface[cs_adj][!constrains])
  } else {
    gr <- base::max(cost_surface[cs_adj]) - base::min(cost_surface[cs_adj])
  }

  if(inherits(constrains, "logical"))  {
    rast_adj <- base::rbind(rast_adj, cs_adj[constrains,])
    rast_adj <- rast_adj[!(base::duplicated(rast_adj) | base::duplicated(rast_adj, fromLast = TRUE)), ]
  }

  rast_vals <- cs_rast[rast_adj[,2]]
  rast_vals_mat <-  base::cbind(rast_adj, rast_vals)

  local_weights <- as.numeric(tapply(rast_vals_mat[,3],rast_vals_mat[,1], function(x) { ((gw*(max(x) - min(x))) / (gr))}))

  #local_weights <- stats::aggregate(rast_vals ~ from, data = rast_vals_mat, FUN = function(x) { ((gw*(max(x) - min(x))) / (gr))})
  #local_weights <- base::unlist(local_weights)

  cost_surface@transitionMatrix@x <- base::rep(local_weights, times = diff(cost_surface@transitionMatrix@p))

  if(inherits(constrains, "logical"))  {
    cost_surface[cs_adj][constrains] <- 0
    cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)
  }

  return(cost_surface)

}

