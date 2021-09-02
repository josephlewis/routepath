#' Extract parameter values from routepaths
#'
#' @param parameters Extract parameter values from routepaths
#'
#' @param no_samples routepaths
#'
#' @return parameter values
#'
#' @author Joseph Lewis
#'
#' @export

sample_parameters <- function(parameters, no_samples) {

  parameters <- apply(X = parameters, MARGIN = 2, FUN = function(x) { sample(x = x, size = no_samples, replace = TRUE)})

  return(parameters)

}
