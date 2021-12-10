#' Process prior parameter values
#'
#' @param routes simulated route paths
#'
#' @param known_route a SpatialLines object of the known route to be used when comparing against the simulated route paths
#'
#' @param validation Method to compare simulated route paths against known_routes
#'
#' @return Summary Statistic of distance from known route to simulated route
#'
#' @keywords internal
#'
#' @author Joseph Lewis

calculate_distance <- function(routes, known_route, validation) {

    if (validation == "euclidean") {

        distances <- euclidean_distance(routes, known_route)

    }

  if (validation == "pdi") {

    distances <- path_deviation_index(routes, known_route)

  }

  if (validation == "frechet") {

    distances <- frechet_distance(routes, known_route)

  }

    return(distances)

}
