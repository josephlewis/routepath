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
#' @param lines a SpatialLines to be used when comparing against the simulated routes
#'
#' @param validation Method used to validate simulated routes against supplied line. Current implementations are: 'max_distance'
#'
#' @param summary_stat_target a vector containing the targeted (observed) summary statistics.
#'
#' @param tol tolerance, a strictly positive number (between 0 and 1) indicating the proportion of simulations retained nearest the targeted summary statistics.
#'
#' @return xx
#'
#' @author Joseph Lewis
#'
#' @import parallel
#' @import doParallel
#' @import foreach
#' @import abc
#'
#' @export

ABC_rejection <- function(input_data, model, priors, lines, validation = "max_distance", summary_stat_target = 0, tol = 1) {

    # cl <- parallel::makeCluster(cores)
    # doParallel::registerDoParallel(cl)

    routepaths <- foreach::foreach(row_no = 1:nrow(priors), .combine = "rbind") %do% {

      print(paste0("Iteration: ", row_no))

      cost_surface <- model(input_data, priors[row_no,])
      points <- extract_end_points(lines = lines)
      routes <- calculate_routepaths(cost_surface = cost_surface, locations = points)

    }

    # parallel::stopCluster(cl)

    posterior <- process_parameters(routepaths = routepaths, lines = lines, priors = priors, validation = validation, summary_stat_target = summary_stat_target, tol = tol)

    return(posterior)

}



