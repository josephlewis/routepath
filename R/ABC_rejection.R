#' routepath rejection sampling scheme using Approximate Bayesian Computation
#'
#' Launches a series of model simulations using user-supplied prior parameter values. See details for more information
#'
#' @details xxx
#'
#' @param input_data \code{list} Data used in the routepath ABC process
#'
#' @param priors \code{Matrix} prior parameter values
#'
#' @param model \code{function} route model to be used when simulating routes. This function requires that a \code{conductanceMatrix} is returned
#'
#' @param known_route \code{sf line} known route to compare against the simulated routepaths
#'
#' @param validation \code{character} Validation method used to assess fit of least-cost path against known route. Implemented methods include: 'euclidean' (Default), 'pdi', 'frechet', 'hausdorff'
#'
#' @param tol \code{numeric} maximum distance between simulated routes and known route to be deemed equal. All simulated routes above this value are rejected
#'
#' @param ncores \code{numeric} number of cores used during the routepath ABC process. Default number of cores is 1
#'
#' @param spatial \code{logical} if TRUE then sf object returned. if FALSE (default) then data.frame object returned
#'
#' @param line_id \code{numeric} value to be included within routepath object. Default value is 1. This can be useful if iteratively modelling known routes and incrementally assigning line ids is required
#'
#' @return \code{routepath}
#'
#' @author Joseph Lewis
#'
#' @importFrom foreach %dopar%
#'
#' @export
#'
#' @examples
#'
#'library(leastcostpath)
#'
#'r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#'slope_cs <- create_slope_cs(x = r, cost_function = "tobler", neighbours = 4)
#'locs <- sf::st_sf(geometry = sf::st_sfc(
#'sf::st_point(c(839769, 4199443)),
#'sf::st_point(c(1038608, 4100024)),
#'crs = terra::crs(r)))
#'
#'known_route <- leastcostpath::create_lcp(x = slope_cs, origin = locs[1,],
#'destination = locs[2,], cost_distance = FALSE)
#'
#'input_data <- list(r)
#'
#'nsims <- 10
#'priors <- cbind(a = 1, b = rnorm(n = nsims, mean = 3.5, sd = 1),
#'c = rnorm(n = nsims, mean = 0.05, sd = 0.1))
#'
#'model <- function(x, y) {
#'slope_cs <- leastcostpath::create_slope_cs(x = x[[1]],
#'cost_function = function(x) {(y[1] * exp(-y[2] * abs(x + y[3]))) / 3.6},
#'neighbours = 4)
#'
#'return(slope_cs)
#'}
#'
#'routepath <- ABC_rejection(input_data = input_data, model = model,
#'priors = priors, known_route = known_route,
#'line_id = 1, validation = "euclidean", tol = NULL)

ABC_rejection <- function(input_data, model, priors, known_route, line_id = 1, validation = "euclidean", tol = NULL, ncores = 1, spatial = FALSE) {

  input_data <- lapply(input_data, FUN = function(x) { terra::wrap(x)})

  myCluster <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(myCluster)

  routes <- foreach::foreach(row_no = 1:nrow(priors), .combine = "rbind") %dopar% {

    input_data2 <- lapply(input_data, FUN = function(x) { terra::rast(x)})

    conductanceMatrix <- model(x = input_data2, y = priors[row_no,])
    points <- extract_end_points(known_route = known_route)
    route <- calculate_routepath(x = conductanceMatrix, locations = points)
    summary_stat <- calculate_distance(route = route, known_route = known_route, validation = validation)
    processed_route <- process_route(route = route, priors = priors, line_id = line_id, row_no = row_no, summary_stat = summary_stat, spatial = spatial)

  }

  parallel::stopCluster(myCluster)

  if (!is.null(tol)) {
    routes$result[routes$stats >= tol] <- "Reject"
  }

  routepaths <- process_routepaths(routes = routes, model = model, validation = validation, tol = tol)

  return(routepaths)

}

#' @export

print.routepath <- function(x, verbose = FALSE) {
  cat("Class:", class(x))
  if(verbose) {
    cat("\nmodel:\n\n", deparse(body(x$model)))
  }
  cat("\n")
  cat("\nvalidation method:", x$validation)
  cat("\ntolerance:", x$tolerance)
  cat("\nno. of routes:", length(unique(x$routes$line_id)))
  cat("\ntotal no. of simulations:", nrow(x$routes))
  if(verbose) {
    cat("\ntotal no. of simulations per route:", stats::aggregate(x$routes$param_row, list(x$routes$line_id), max)[,2])
  }
  cat("\ntotal no. of parameters:", sum(grepl(pattern = "p.", x = colnames(x$routes), fixed = TRUE)))
  cat("\npercentage of simulations accepted:", sum(x$routes$result == "Accept") / length(x$routes$result) * 100, "%")
  if(verbose) {
  cat("\npercentage of simulations accepted per route:", paste0((stats::aggregate(x$routes$result == "Accept", list(x$routes$line_id), sum)[,2]) / stats::aggregate(x$routes$param_row, list(x$routes$line_id), max)[,2] *100, "%"))
  }
}
