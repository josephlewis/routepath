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

process_route <- function(route, priors, route_no = route_no, row_no = row_no, distance, spatial = spatial) {

  route$line_id <- route_no
  route$param_row <- row_no
  route <- cbind(route, p = priors[row_no,, drop = FALSE])
  route$distance <- distance
  route$result = "Accept"

  # if route cannot be calculate from origin to destination then stats value will be -Inf. Replace this value with NA and then change result to 'Reject'
  route$distance[is.infinite(route$distance)] <- NA
  route$result[is.na(route$distance)] <- "Reject"

  if (!spatial) {
    route <- sf::st_drop_geometry(route)
  }

  return(route)
}
