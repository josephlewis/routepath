#' Identify accepted simulations
#'
#' This function returns an index of the identified accepted simulations
#'
#' @param parameters accepted parameter values
#'
#' @return vector index of accepted simulations
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import foreach
#' @import sp
#' @import methods

abc_index <- function(parameters = param_reject) {

  parameters_index <- lapply(X = parameters, FUN = function(x) { x$region})

  parameters_index <- unlist(parameters_index)

  return(parameters_index)

}


