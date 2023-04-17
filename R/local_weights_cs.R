#' rescale conductanceMatrix based on global range of values
#'
#' @param x \code{conductanceMatrix}
#'
#' @param global_weight \code{numeric} importance of factor. Supplied value must be between 0 and 1
#'
#' @return \code{conductanceMatrix}
#'
#' @author Joseph Lewis
#'
#' @export

local_weights_cs <- function(x, global_weight) {

  cs_adj <- Matrix::summary(x$conductanceMatrix)
  cs_adj <- cs_adj[order(cs_adj$j),]
  gw <- global_weight
  gr <- base::max(cs_adj$x) - 0

  local_weights <- stats::aggregate(x ~ j, data = cs_adj, FUN = function(x) { ((gw*(max(x) - 0)) / (gr))})
  local_weights$x2 <- local_weights$x / sum(local_weights$x)

  rep_times <-tabulate(cs_adj$j)
  rep_times <- rep_times[rep_times != 0]

  cs_adj$loc_weights <- base::rep(local_weights$x2, times = rep_times)
  cs_adj <- cs_adj[order(cs_adj$i),]

  x$conductanceMatrix@x <- base::rep(local_weights$x, times = rep_times)

  return(x)

}
