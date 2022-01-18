#' Rescales cost surface based on local range of transition values
#'
#' @param cost_surface cost surface
#'
#' @param p risk
#'
#' @param constrains Boolean vector of adjacent cells not traversable
#'
#' @param win Window
#'
#' @return cost suface standardised to a maximum value of 1 and minimum value relative to 0 incorporating local range
#'
#' @author Joseph Lewis
#'
#' @export

rescale_cs_local <- function(cost_surface, p = 1, constrains = NULL, win) {

  if (p <= 1) {stop("p must be equal or greater than 1")}

  # if(!inherits(constrains, "logical")) {stop("constraints must be a logical vector")}

  if(!inherits(win, "matrix")) {stop("win must be a matrix. See raster::adjacent for details on neighourbood matrix")}

  rast <- raster::raster(cost_surface)
  adj_rast <- raster::adjacent(rast, cells=1:raster::ncell(rast), pairs=TRUE, directions=win, include = TRUE, sorted = TRUE)
  cs_adj <- gdistance::adjacencyFromTransition(cost_surface)

  if (!is.null(constrains)) {
    adj_rast2 <- base::rbind(adj_rast, cs_adj[constrains,])
    adj_rast3 <- adj_rast2[!(duplicated(adj_rast2) | duplicated(adj_rast2, fromLast = TRUE)), ]
  }

  rast_vals <- rast[adj_rast[,2]]
  rast_vals_mat <-  base::cbind(adj_rast, rast_vals)

  local_values <- tapply(rast_vals_mat[,3],rast_vals_mat[,1], function(x) { (((c(0, x) - 0) / (max(x) - 0)) ^ 1)[-1]})
  local_values <- base::unlist(local_values)
  local_values <- local_values[which(rast_vals_mat[,1] == rast_vals_mat[,2])]

  cost_surface@transitionMatrix@x <- base::rep(local_values, times = diff(cost_surface@transitionMatrix@p))

  if (!is.null(constrains)) {
    cost_surface[cs_adj][constrains] <- 0
    cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)
  }

  return(cost_surface)

}
