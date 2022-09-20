#' rescale conductanceMatrix based on global range of values
#'
#' @param x \code{conductanceMatrix}
#'
#' @param FUN \code{function} function applied to standardised conductanceMatrix values
#'
#' @return \code{conductanceMatrix} standardised conductanceMatrix with a maximum value of 1 and minimum value relative to 0
#'
#' @author Joseph Lewis
#'
#' @export

rescale_cs_global <- function(x, FUN = NULL) {

  # explicitly remove zeroes from the sparse matrix. This is to ensure that scaling is not impacted by zero values
  x$conductanceMatrix <- Matrix::drop0(x$conductanceMatrix)
  max_val <- base::max(x$conductanceMatrix@x)

  x$conductanceMatrix@x <- (((c(0, x$conductanceMatrix@x) - 0) / (max_val - 0)))[-1]

  if(inherits(FUN, "function"))  {
    x$conductanceMatrix@x <- FUN(x$conductanceMatrix@x)
  }

  return(x)
}
