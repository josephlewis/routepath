#' rescale conductanceMatrix based on global range of values
#'
#' @param x \code{conductanceMatrix}
#'
#' @param FUN \code{function} function applied to standardised conductanceMatrix values
#'
#' @return \code{conductanceMatrix}
#'
#' @author Joseph Lewis
#'
#' @export

rescale_cs_local <- function(x, FUN = NULL) {

  cs_adj <- Matrix::summary(x$conductanceMatrix)
  cs_adj <- cs_adj[order(cs_adj$j),]

  local_values <- stats::aggregate(x ~ j, data = cs_adj, FUN = function(x) { ((x - 0) / (max(x) - 0))})

  cs_adj$loc_vals <- unlist(local_values$x)
  cs_adj <- cs_adj[order(cs_adj$i),]

  if(inherits(FUN, "function"))  {
    cs_adj$loc_vals <- FUN(cs_adj$loc_vals)
  }

  x$conductanceMatrix@x <- cs_adj$loc_vals

  return(x)

}
