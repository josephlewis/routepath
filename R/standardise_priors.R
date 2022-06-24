#' Standardise priors
#'
#' Priors in selected columns standardised to sum to one
#'
#' @param priors matrix of prior parameter values
#'
#' @param col_index Column index of prior parameter values to standardise
#'
#' @return priors matrix
#'
#' @author Joseph Lewis

standardise_priors <- function(priors, col_index) {

  if(!inherits(priors, "matrix")) {
    stop("Expecting prior to be a matrix object")
  }

  if(any(col_index > ncol(priors))) {
    stop("Invalid column index")
  }

  for(i in col_index) {
    standardised_vals <- matrix(priors[,i] / (rowSums(priors[,col_index])))
    colnames(standardised_vals) <- paste0(colnames(priors)[i], "_s")
    priors <- cbind(priors, standardised_vals)
  }

  return(priors)

}
