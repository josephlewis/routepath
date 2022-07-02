#' Attach parameters dataframe to sf
#'
#' @param known_routes xxxx
#'
#' @param parameters xxx
#'
#' @param ... see Details
#'
#' @return xxx
#'
#' @author Joseph Lewis
#'
#' @export

attach_parameters <- function(known_routes, parameters, ...) {

  known_routes <- base::merge(known_routes, parameters, ...)

  return(known_routes)

}
