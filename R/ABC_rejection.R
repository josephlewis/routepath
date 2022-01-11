#' Route Paths Rejection sampling scheme for Approximate Bayesian Computation
#'
#' This function launches a series of model simulations with model parameters drawn from the user-supplied prior values.
#'
#' @details xxx
#'
#' @param input_data a list of input data to be used in the route path modelling process
#'
#' @param priors a matrix or dataframe of priors
#'
#' @param model An R function implementing the route model to be simulated.
#'
#' @param known_route a SpatialLines object of the known route to be used when comparing against the simulated route paths
#'
#' @param validation Method used to validate simulated routes against supplied line. Current implementations are: 'euclidean', 'pdi' and 'frechet'
#'
#' @param tol tolerance. Maximum deviation from known route for the simulation to be accepted. If NULL all simulated route paths are returned.
#'
#' @param cores Number of cores
#'
#' @param spatial if TRUE then sf Lines returned. If FALSE then dataframe returned
#'
#' @param drop_rows if TRUE (default) then rejected simulations are dropped. If FALSE then all simulations returned
#'
#' @param line_id ID attached to output. Default value is 1. Useful if iteratively modelling routes and want to incrementally assign line IDs
#'
#' @return Dataframe or sf Lines
#'
#' @author Joseph Lewis
#'
#' @import snow
#' @import doSNOW
#' @import foreach
#' @import utils
#' @import sf
#'
#' @export

ABC_rejection <- function(input_data, model, priors, known_route, validation = "euclidean", tol = NULL, cores = 1, spatial = FALSE, drop_rows = FALSE, line_id = 1) {

    cl <- snow::makeCluster(cores, type = "SOCK")
    doSNOW::registerDoSNOW(cl)
    pb <- utils::txtProgressBar(max = nrow(priors), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    routepaths <- foreach::foreach(row_no = 1:nrow(priors), .combine = "rbind", .packages = "routepath", .options.snow = opts) %dopar% {

        cost_surface <- model(input_data, priors[row_no,])
        points <- extract_end_points(known_route = known_route)
        routes <- calculate_routepaths(cost_surface = cost_surface, locations = points)
        summary_stat <- calculate_distance(routes = routes, known_route = known_route, validation = validation)

        processed_params <- process_parameters(routepaths = routes, priors = priors, line_id = line_id, row_no = row_no, summary_stat = summary_stat, spatial = spatial)

    }

    close(pb)
    snow::stopCluster(cl)

    routepaths$result <- "Accept"

    if (!is.null(tol)) {
        routepaths$result[routepaths$stats >= tol] <- "Reject"
    }

    if (drop_rows) {
        routepaths <- routepaths[!routepaths$result == "Reject", ]
    }

    return(routepaths)

}
