#' Rescales cost surface based on global range of transition values
#'
#' @param cost_surface cost surface
#'
#' @param p risk
#'
#' @param constrains Boolean vector of adjacent cells not traversable
#'
#' @return cost suface standardised to a maximum value of 1 and minimum value relative to 0
#'
#' @author Joseph Lewis
#'
#' @export

rescale_cs_global <- function(cost_surface, p = 1, constrains = NULL) {

  if (p < 1) {stop("p must be equal or greater than 1")}

  cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)

  cs_adj <- gdistance::adjacencyFromTransition(cost_surface)

  if (!is.null(constrains)) {
    cost_surface[cs_adj][constrains] <- 0
  }

  cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)

  max_val <- base::max(cost_surface@transitionMatrix@x)

  cost_surface@transitionMatrix@x <- (((c(0, cost_surface@transitionMatrix@x) - 0) / (max_val - 0)) ^ p)[-1]

  return(cost_surface)
}
