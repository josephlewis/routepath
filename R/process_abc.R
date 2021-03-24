#' @keywords internal

process_abc <- function(parameters = param_reject, lines = lines) {

  parameters_abc <- cbind(line_id = rep(1:length(lines), each = sum(parameters[[1]]$region)),
                          do.call(rbind, lapply(X = parameters, FUN = function(x) { x$unadj.values})))

  return(parameters_abc)

}


