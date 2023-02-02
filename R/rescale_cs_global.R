#' rescale conductanceMatrix based on global range of values
#'
#' @param x \code{conductanceMatrix}
#'
#' @param FUN \code{function} function applied to standardised conductanceMatrix values
#'
#' @param max_value \code{numeric} maximum value used when scaling the conductanceMatrix between 0 and 1. The specified max_value should be the maximum value within the conductanceMatrix if each modelled route is to be viewed independently, or the maximum value across all conductanceMatrix, i.e. the whole spatRaster, if multiple modelled routes to be compared and viewed concurrently. If max_value is NULL (default) max_value is the maximum value within the supplied conductanceMatrix
#'
#' @return \code{conductanceMatrix} standardised conductanceMatrix with a maximum value of 1 and minimum value relative to 0
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @export

rescale_cs_global <- function(x, max_value = NULL, FUN = NULL) {

  # explicitly remove zeroes from the conductanceMatrix. This is to ensure that scaling is not impacted by zero values
  x$conductanceMatrix <- Matrix::drop0(x$conductanceMatrix)

  if(is.null(max_value)) {
    max_value <- base::max(x$conductanceMatrix@x)
  }
  else {
    max_value <- max_value
    }

  # rescales conductanceMatrix to between 0 and 1. Maximum value in conductanceMatrix is given a value of 1 whereas minimum value in conductanceMatrix is given a value relative to when the minimum value is 0
  # This is done to ensure that the minimum value in the conductanceMatrix is not given a value of 0 as this would mean that the minimum value become non-traversable. The minimum value relative to 0 also ensures that all rescaled conductanceMatrix are based on a comparable range
  x$conductanceMatrix@x <- (((c(0, x$conductanceMatrix@x) - 0) / (max_value - 0)))[-1]

  # The above assumes a linear relationship when rescaling the conductanceMatrix. This linear relationship can be modified via a supplied function, e.g. raising to a power
  if(inherits(FUN, "function"))  {
    x$conductanceMatrix@x <- FUN(x$conductanceMatrix@x)
  }

  return(x)
}
