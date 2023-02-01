#' Extracts origin and destination locations from known_route
#'
#' Extracts the origin and destination point from the supplied known route.
#'
#' @param known_route \code{sf line} known route
#'
#' @return sf POINT origin and destination of supplied known_route
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'

extract_end_points <- function(known_route) {

  route_pts <- sf::st_cast(known_route, "POINT", warn = FALSE)
  end_pts <- route_pts[c(1,nrow(route_pts)),]

  return(end_pts)
}

