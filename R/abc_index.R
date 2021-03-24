#' @keywords internal

abc_index <- function(parameters = param_reject) {

  parameters_index <- lapply(X = parameters, FUN = function(x) { x$region})

  parameters_index <- unlist(parameters_index)

  return(parameters_index)

}


