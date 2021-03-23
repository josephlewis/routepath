#' Route Paths Rejection sampling scheme for ABC
#'
#' This function launches a series of model simulations with model parameters drawn from prior_matrix
#'
#' @details
#'
#' @param input_data a list of input data to be used in the route modelling process
#'
#' @param prior a matrix of priors.
#'
#' @param model an R function implementing the route model to be simulated.
#'
#' @param line a SpatialLines to be used when comparing against the simulated routes
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
#' @export
#'
#' @examples

ABC_rejection <- function(input_data, model, prior, lines, validation = "max_distance", summary_stat_target = 0, tol = 1, cores = 1) {

  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)

  routepaths <- foreach(prior = iterators::iter(prior, by = 'row'), .combine=rbind) %dopar%
    {
      cost_surface <- model(input_data = input_data, prior = prior)
      points <- extract_end_points(lines = lines)
      routes <- calculate_routepaths(cost_surface = cost_surface, locations = points)

    }

  parallel::stopCluster(cl)

  # lcps <- pbapply::pbapply(X = prior, MARGIN = 1, FUN = function(x) {
  #
  #   cs <- model(input_data = input_data, prior = x)
  #
  #   pts <- extract_end_points(lines = lines)
  #
  #   lcps <- calculate_routepaths(cost_surface = cs, locations = pts)
  #
  # })
  #
  # lcps <- do.call(rbind, lcps)
}



