#' Global Weighted Linear Combination
#'
#' @param x \code{conductanceMatrix}
#'
#' @param global_weight \code{numeric} importance of factor. Value must be between 0 and 1
#'
#' @param FUN \code{function} function applied to standardised conductanceMatrix values
#'
#' @return \code{conductanceMatrix}
#'
#' @author Joseph Lewis
#'
#' @export

global_wlc <- function(x, global_weight, FUN = NULL) {

  # rescale to between 0 and 1
  x <- rescale_cs_global(x = x, FUN = FUN)

  # multiply by user-supplied criterion global weight
  x$conductanceMatrix <- x$conductanceMatrix * global_weight

  return(x)

}
