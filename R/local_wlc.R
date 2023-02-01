#' rescale conductanceMatrix based on global range of values
#'
#' @param x \code{conductanceMatrix}
#'
#' @param global_weight \code{numeric} importance of factor. Supplied value must be between 0 and 1
#'
#' @param max_value \code{numeric} maximum value used when scaling the conductanceMatrix between 0 and 1. The specified max_value should be the maximum value within the conductanceMatrix if each modelled route is to be viewed independently, or the maximum value across all conductanceMatrix, i.e. the whole spatRaster, if multiple modelled routes to be compared and viewed concurrently. If max_value is NULL (default) max_value is the maximum value within the supplied conductanceMatrix
#'
#' @param FUN \code{function} function applied to standardised conductanceMatrix values


local_wlc <- function(x, global_weight, max_value = NULL, FUN = NULL) {

  local_weights <- local_weights_cs(x = x, global_weight = global_weight)
  local_cs <- rescale_cs_local(x = x, FUN = FUN, max_value = max_value)

  x$conductanceMatrix <- local_cs$conductanceMatrix * local_weights$conductanceMatrix

  return(x)

}
