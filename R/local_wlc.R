#' rescale conductanceMatrix based on global range of values
#'
#' @param x \code{conductanceMatrix}
#'
#' @param global_weight \code{numeric} Importance of factor. Supplied value must be between 0 and 1
#'
#' @param FUN \code{function} function applied to standardised conductanceMatrix values
#'
#' @return \code{conductanceMatrix}
#'
#' @author Joseph Lewis
#'
#' @export

local_wlc <- function(x, global_weight, FUN = NULL) {

  local_weights <- local_weights_cs(x = x, global_weight = global_weight)
  local_cs <- rescale_cs_local(x = x, FUN = FUN)

  x$conductanceMatrix@x <- local_cs$conductanceMatrix@x * local_weights$conductanceMatrix@x

  return(x)

}
