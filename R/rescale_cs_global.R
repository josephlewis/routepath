#' rescale conductanceMatrix based on global range of values
#'
#' @param x \code{conductanceMatrix}
#'
#' @param FUN \code{function} function applied to standardised conductanceMatrix values
#'
#' @param max_value \code{numeric} value used when scaling conductanceMatrix to between 0 and 1. The specified max_value should be the maximum value possible within the supplied conductanceMatrix or the SpatRaster that the conductanceMatrix is croppped from
#'
#' @return \code{conductanceMatrix} standardised conductanceMatrix with a maximum value of 1 and minimum value relative to 0
#'
#' @author Joseph Lewis
#'
#' @export

rescale_cs_global <- function(x, FUN = NULL, max_value) {

  # explicitly remove zeroes from the sparse matrix. This is to ensure that scaling is not impacted by zero values
  x$conductanceMatrix <- Matrix::drop0(x$conductanceMatrix)

  x$conductanceMatrix@x <- (((c(0, x$conductanceMatrix@x) - 0) / (max_value - 0)))[-1]

  if(inherits(FUN, "function"))  {
    x$conductanceMatrix@x <- FUN(x$conductanceMatrix@x)
  }

  return(x)
}
