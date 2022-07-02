#' Routepath Rejection sampling scheme using Approximate Bayesian Computation
#'
#' This function launches a series of model simulations using user-supplied prior parameter values
#'
#' @details xxx
#'
#' @param input_data a list of input data to be used in the routepath modelling process
#'
#' @param priors a matrix of priors
#'
#' @param model An R function implementing the route model to be simulated. This function is expected to return a TransitionLayer object
#'
#' @param known_route a SpatialLines object of the known route to compare against the simulated routepaths
#'
#' @param validation Distance method used to validate simulated routes against the supplied known route. Current implementations are: 'euclidean', 'pdi', 'frechet' and 'hausdorff'
#'
#' @param tol Tolerance threshold. Maximum deviation from known route for the simulated routepaths to be accepted. If NULL all simulated routepaths are returned.
#'
#' @param cores Number of cores
#'
#' @param spatial if TRUE then sf Lines returned. If FALSE then dataframe returned
#'
#' @param drop_rows if FALSE (default) all simulated routepaths returned. If TRUE rejected simulations are dropped
#'
#' @param line_id ID attached to routepaths output. Default value is 1. Useful if iteratively modelling known routes and want to incrementally assign line IDs
#'
#' @return Dataframe or sf Lines
#'
#' @author Joseph Lewis
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

    if (!is.null(tol)) {
        routepaths$result[routepaths$stats >= tol] <- "Reject"
    }

    if (drop_rows) {
        routepaths <- routepaths[!routepaths$result == "Reject", ]
    }

    return(routepaths)

}
