#' Global Weighted Linear Combination
#'
#' @param x \code{conductanceMatrix}
#'
#' @param global_weight \code{numeric} importance of factor. Supplied value must be between 0 and 1
#'
#' @param max_value \code{numeric} maximum value used when scaling the conductanceMatrix between 0 and 1. The specified max_value should be the maximum value within the conductanceMatrix if each modelled route is to be viewed independently, or the maximum value across all conductanceMatrix, i.e. the whole spatRaster, if to be compared and viewed concurrently
#'
#' @param FUN \code{function} if function supplied (default NULL) then this function is applied to the standardised conductanceMatrix values
#'
#' @return \code{conductanceMatrix}
#'
#' @author Joseph Lewis
#'
#' @export

global_wlc <- function(x, global_weight, max_value = NULL, FUN = NULL) {

  # rescale to between 0 and 1
  x <- rescale_cs_global(x = x, FUN = FUN, max_value)

  # multiply by user-supplied criterion global weight
  x$conductanceMatrix@x <- x$conductanceMatrix@x * global_weight

  return(x)

}
