#' Route Paths Rejection sampling scheme for ABC
#'
#' This function launches a series of model simulations with model parameters drawn from the user-supplied prior values.
#'
#' @details xxx
#'
#' @param input_data a list of input data to be used in the route modelling process
#'
#' @param priors a matrix of priors.
#'
#' @param model an R function implementing the route model to be simulated.
#'
#' @param known_routes a Spatialknown_routes to be used when comparing against the simulated route paths
#'
#' @param validation Method used to validate simulated routes against supplied line. Current implementations are: 'euclidean'
#'
#' @param tol tolerance. If NULL returns all simulations
#'
#' @param cores xx
#'
#' @param output if 'matrix' (default) then a matrix of parameter values of the accepted simulations is returned. If 'routes' then all accepted simulated routes are returned with the parameter values attached as dataframe.
#'
#' @param drop_rows if TRUE (default) then rejected simulations are dropped. If FALSE then all simulations retained.
#'
#' @return Matrix or Spatialknown_routesDataFrame dependent on output argument
#'
#' @author Joseph Lewis
#'
#' @import snow
#' @import doSNOW
#' @import foreach
#' @import abc
#' @import utils
#' @import sf
#'
#' @export

ABC_rejection <- function(input_data, model, priors, known_routes, validation = "euclidean", tol = NULL, cores = 1, output = "dataframe", drop_rows = FALSE) {

    cl <- snow::makeCluster(cores, type = "SOCK")
    doSNOW::registerDoSNOW(cl)
    pb <- utils::txtProgressBar(max = nrow(priors), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    routepaths <- foreach::foreach(row_no = 1:nrow(priors), .combine = "rbind", .packages = "routepath", .options.snow = opts) %dopar% {

        cost_surface <- model(input_data, priors[row_no,])
        points <- extract_end_points(known_routes = known_routes)
        routes <- calculate_routepaths(cost_surface = cost_surface, locations = points)
        summary_stat <- calculate_distance(routes = routes, known_routes = known_routes, validation = validation)
        processed_params <- process_parameters(routepaths = routes, known_routes = known_routes, priors = priors[row_no,, drop = FALSE], summary_stat = summary_stat, output = output, row_no = row_no)

    }

    close(pb)
    snow::stopCluster(cl)

    # routepaths$row_no <- rep(1:nrow(priors), each = nrow(known_routes))
    routepaths$result <- "Accept"

    if (!is.null(tol)) {
        routepaths$result[routepaths$stats >= tol] <- "Reject"
    }

    if (drop_rows) {
        routepaths <- routepaths[routepaths$result == "Accept", ]
    }

    return(routepaths)

}
