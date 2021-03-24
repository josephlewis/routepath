#' Rejection sampling scheme for ABC
#'
#' This function applies the rejection sampling scheme based on the efficacy of the summary statistic
#'
#' @param parameters prior parameter values
#'
#' @param lines SpatialLines
#'
#' @param summary_stat_target a vector of the simulated summary statistic
#'
#' @param tol tolerance, the required proportion of points accepted nearest the target values
#'
#' @return list of accepted parameter values
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import foreach
#' @import sp
#' @import methods

abc_reject <- function(parameters = processed_params, lines = lines, summary_stat_target = summary_stat_target , tol = tol) {

  parameters_reject <- lapply(X = parameters, FUN = function(x) { abc::abc(target = summary_stat_target, param = x[,2:ncol(x)], sumstat = x[,"stats"], tol = tol, method = "rejection")})

  return(parameters_reject)

}
