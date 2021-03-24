#' @keywords internal

abc_reject <- function(parameters = processed_params, lines = lines, summary_stat_target = summary_stat_target , tol = tol) {

  parameters_reject <- lapply(X = parameters, FUN = function(x) { abc::abc(target = summary_stat_target, param = x[,2:ncol(x)], sumstat = x[,"stats"], tol = tol, method = "rejection")})

  return(parameters_reject)

}


