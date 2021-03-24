#' Process parameter values from Rejection sampling scheme for ABC
#'
#' @param parameters accepted parameter values
#'
#' @param lines SpatialLines
#'
#' @return Matrix of accepted prior parameter values
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import foreach
#' @import sp
#' @import methods

process_abc <- function(parameters = param_reject, lines = lines) {

  parameters_abc <- cbind(line_id = rep(1:length(lines), each = sum(parameters[[1]]$region)),
                          do.call(rbind, lapply(X = parameters, FUN = function(x) { x$unadj.values})))

  return(parameters_abc)

}


