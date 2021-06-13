#' Process prior parameter values
#'
#' @param routepaths simulated route paths
#'
#' @param known_routes Spatialknown_routes
#'
#' @param priors prior parameter values
#'
#' @param validation Method to compare simulated route paths against known_routes
#'
#' @param output Method to compare simulated route paths against known_routes
#'
#' @return list of prior parameter values
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import foreach
#' @import sp
#' @import methods

process_parameters <- function(routepaths, known_routes, priors, summary_stat, output = output, row_no = row_no) {

    parameters <- data.frame(line_id = 1:nrow(known_routes),
                             param_row = row_no,
                             priors[rep(seq_len(nrow(priors)), each = nrow(known_routes)),, drop = FALSE],
                        stats = summary_stat)

    if (output == "spatial") {
        routepaths <- sf::st_as_sf(routepaths)
        parameters <- cbind(routepaths, parameters)
    }

    return(parameters)

}
