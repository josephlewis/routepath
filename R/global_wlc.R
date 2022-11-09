#' Global Weighted Linear Combination
#'
#' @param x \code{conductanceMatrix}
#'
#' @param global_weight \code{numeric} importance of factor. Value must be between 0 and 1
#'
#' @param max_value \code{numeric} value used when scaling conductanceMatrix to between 0 and 1. The specified max_value should be the maximum value possible within the supplied conductanceMatrix or the SpatRaster that the conductanceMatrix is croppped from
#'
#' @param FUN \code{function} if function supplied (default NULL) then this function is applied to the standardised conductanceMatrix values
#'
#' @return \code{conductanceMatrix}
#'
#' @author Joseph Lewis
#'
#' @export

global_wlc <- function(x, global_weight, max_value, FUN = NULL) {

  # rescale to between 0 and 1
  x <- rescale_cs_global(x = x, FUN = FUN, max_value = max_value)

  # multiply by user-supplied criterion global weight
  x$conductanceMatrix <- x$conductanceMatrix * global_weight

  return(x)

}
