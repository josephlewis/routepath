#' rescale conductanceMatrix based on global range of values
#'
#' @param x \code{conductanceMatrix}
#'
#' @param global_weight \code{numeric} importance of factor. Supplied value must be between 0 and 1

local_weights_cs <- function(x, global_weight) {

  cs_adj <- Matrix::summary(x$conductanceMatrix)
  gw <- global_weight
  gr <- base::max(cs_adj$x) - base::min(cs_adj$x)

  local_weights <- stats::aggregate(x ~ j, data = cs_adj, FUN = function(x) { ((gw*(max(x) - min(x))) / (gr))})

  rep_times <-tabulate(cs_adj$j)
  rep_times <- rep_times[rep_times != 0]

  x$conductanceMatrix@x <- base::rep(local_weights$x, times = rep_times)

  return(x)

}
