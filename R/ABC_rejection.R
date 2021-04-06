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
#' @param lines a SpatialLines to be used when comparing against the simulated route paths
#'
#' @param validation Method used to validate simulated routes against supplied line. Current implementations are: 'max_distance'
#'
#' @param summary_stat_target a vector containing the targeted (observed) summary statistics.
#'
#' @param tol tolerance, a strictly positive number (between 0 and 1) indicating the proportion of simulations retained nearest the targeted summary statistics.
#'
#' @param cores xx
#'
#' @param output if "matrix" (default) then a matrix of parameter values of the accepted simulations is returned. If "routes" then all accepted simulated routes are returned with the parameter values attached as dataframe.
#'
#' @return Matrix or SpatialLinesDataFrame dependent on output argument
#'
#' @author Joseph Lewis
#'
#' @import snow
#' @import doSNOW
#' @import foreach
#' @import abc
#' @import utils
#'
#' @export

ABC_rejection <- function(input_data, model, priors, lines, validation = "max_distance", summary_stat_target = 0, tol = 1, cores = 1, output = "matrix") {

    cl <- snow::makeCluster(cores, type = "SOCK")
    doSNOW::registerDoSNOW(cl)
    pb <- utils::txtProgressBar(max = nrow(priors), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    routepaths <- foreach::foreach(row_no = 1:nrow(priors), .combine = "rbind", .packages = 'routepath', .options.snow = opts) %dopar% {

      cost_surface <- model(input_data, priors[row_no,])
      points <- extract_end_points(lines = lines)
      routes <- calculate_routepaths(cost_surface = cost_surface, locations = points)

    }

    close(pb)
    snow::stopCluster(cl)

    processed_params <- process_parameters(routepaths = routepaths, lines = lines, priors = priors, validation = validation)
    param_reject <- abc_reject(parameters = processed_params, lines = lines, summary_stat_target = summary_stat_target , tol = tol)
    processed_abc <- process_abc(parameters = param_reject, lines = lines)

    if (output == "matrix") {
      processed_abc <- processed_abc

    } else if (output == "routes") {
      index <- abc_index(parameters = param_reject)
      routepaths <- routepaths[index,]
      routepaths@data <- data.frame(processed_abc)
      colnames(routepaths@data) <- colnames(processed_abc)

      processed_abc <- routepaths
    }

    return(processed_abc)

}
