#' Process prior parameter values
#'
#' @param routepaths simulated route paths
#'
#' @param known_routes Spatialknown_routes
#'
#' @param matrix matrix
#'
#' @param validation Method to compare simulated route paths against known_routes
#'
#' @return summary statistic
#'
#' @keywords internal
#'
#' @author Joseph Lewis

calculate_distance <- function(routes, known_routes, validation) {

    if (validation == "euclidean") {

        distances <- euclidean_distance(routes, known_routes)

    }

    return(distances)

}
