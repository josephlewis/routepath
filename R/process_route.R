#' Process route
#'
#' @param route \code{sf line} least-cost path
#'
#' @param priors \code{Matrix} prior parameter values to be included within routepath object
#'
#' @param line_id \code{numeric} value to be included within routepath object
#'
#' @param row_no \code{numeric} value to be included within the routepath object. This value corresponds to the row of the prior parameter Matrix. Default is a value of 1
#'
#' @param summary_stat \code{numeric} distance betwen simulated route and known route to be included within routepath object
#'
#' @param spatial \code{logical} if TRUE then sf object returned. if FALSE (default) then data.frame object returned
#'
#' @return \code{sf object} or \code{data.frame}
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'

process_route <- function(route, priors, line_id, row_no = row_no, summary_stat, spatial = spatial) {

  route$line_id <- line_id[1]
  route$param_row <- row_no
  route <- cbind(route, p = priors[row_no,, drop = FALSE])
  route$stat <- summary_stat
  route$result = "Accept"

  # if route cannot be calculate from origin to destination then stats value will be -Inf. Replace this value with NA and then change result to 'Reject'
  route$stat[is.infinite(route$stat)] <- NA
  route$result[is.na(route$stat)] <- "Reject"

  if (!spatial) {
    route <- sf::st_drop_geometry(route)
  }

  return(route)
}
