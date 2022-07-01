#' Process prior parameter values
#'
#' @param routepaths simulated route paths
#'
#' @param priors a matrix or dataframe of priors
#'
#' @param line_id ID attached to output. Default value is 1. Useful if iteratively modelling routes and want to incrementally assign line IDs.
#'
#' @param row_no Row number
#'
#' @param summary_stat Summary Statistic of distance from known route to simulated route
#'
#' @param spatial if TRUE then sf Lines returned. If FALSE then dataframe returned
#'
#' @return routepaths. If Spatial TRUE then sf. If Spatial FALSE then dataframe
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'
#' @import sf

process_parameters <- function(routepaths, priors, line_id, row_no = row_no, summary_stat, spatial = spatial) {

    parameters <- data.frame(line_id = line_id,
                             param_row = row_no,
                             p = priors[row_no,, drop = FALSE],
                             stats = summary_stat,
                             result = "Accept")

    if (spatial) {
        routepaths <- sf::st_as_sf(routepaths)
        parameters <- cbind(routepaths, parameters)
    }

    return(parameters)

}
