#' rescale conductanceMatrix based on global range of values
#'
#' @param x \code{conductanceMatrix}
#'
#' @param max_value \code{numeric} maximum value used when scaling the conductanceMatrix between 0 and 1. The specified max_value should be the maximum value within the conductanceMatrix if each modelled route is to be viewed independently, or the maximum value across all conductanceMatrix, i.e. the whole spatRaster, if multiple modelled routes to be compared and viewed concurrently. If max_value is NULL (default) max_value is the maximum value within the supplied conductanceMatrix
#'
#' @param FUN \code{function} function applied to standardised conductanceMatrix values

rescale_cs_local <- function(x, max_value = NULL, FUN = NULL) {

  cs_adj <- Matrix::summary(x$conductanceMatrix)

  if(is.null(max_value)) {
    max_value <- base::max(cs_adj$x)
  }
  else {
    max_value <- max_value
  }

  local_values <- stats::aggregate(x ~ j, data = cs_adj, FUN = function(x) { (((c(0, x) - 0) / (max_value - 0)))[-1]})

  if(inherits(FUN, "function"))  {
    local_values <- FUN(local_values)
  }

  x$conductanceMatrix@x <- unlist(local_values$x)

  return(x)

}
