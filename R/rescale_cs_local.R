#' Rescales cost surface based on local range of transition values
#'
#' @param cost_surface cost surface
#'
#' @param constrains Boolean vector of adjacent cells not traversable
#'
#' @param neighbours xxxxx
#'
#' @param fun xxxx
#'
#' @return cost suface standardised to a maximum value of 1 and minimum value relative to 0 incorporating local range
#'
#' @author Joseph Lewis
#'
#' @export

rescale_cs_local <- function(cost_surface, constrains = NULL, neighbours, fun = NULL) {

  cs_rast <- suppressWarnings(raster::raster(cost_surface))

  # calculate adjacent with centre cell included (this is done so that the local value can be assigned later on to the centre cell)
  rast_adj <- raster::adjacent(cs_rast, cells=1:raster::ncell(cs_rast), pairs=TRUE, directions=neighbours, include = TRUE, sorted = TRUE)

  cs_adj <- raster::adjacent(cs_rast, cells=1:raster::ncell(cs_rast), pairs=TRUE, directions=neighbours, include = FALSE, sorted = TRUE)

  if(inherits(constrains, "logical"))  {
    rast_adj <- base::rbind(rast_adj, cs_adj[constrains,])
    rast_adj <- rast_adj[!(duplicated(rast_adj) | duplicated(rast_adj, fromLast = TRUE)),]
  }

  rast_vals <- cs_rast[rast_adj[,2]]
  rast_vals_mat <-  base::cbind(rast_adj, rast_vals)
  local_values <- tapply(rast_vals_mat[,3],rast_vals_mat[,1], function(x) { (((c(0, x) - 0) / (max(x) - 0)))[-1]})
  local_values <- base::unlist(local_values)

  if(inherits(fun, "function"))  {
    local_values <- fun(local_values)
  }

  local_values <- local_values[which(rast_vals_mat[,1] == rast_vals_mat[,2])]

  cost_surface@transitionMatrix@x <- base::rep(local_values, times = diff(cost_surface@transitionMatrix@p))

  if(inherits(constrains, "logical"))  {
    cost_surface[cs_adj][constrains] <- 0
    cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)
  }

  return(cost_surface)

}


