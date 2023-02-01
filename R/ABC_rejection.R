#' routepath rejection sampling scheme using Approximate Bayesian Computation
#'
#' Launches a series of model simulations using user-supplied prior parameter values. See details for more information
#'
#' @details xxx
#'
#' @param input_data \code{list} Data used in the routepath ABC modelling procedure
#'
#' @param model \code{function} Model to be used when simulating routes. This function requires that a \code{conductanceMatrix} is returned
#'
#' @param priors \code{Matrix} Prior parameter values
#'
#' @param known_routes \code{sf line} Known routes to compare against the simulated routes
#'
#' @param validation \code{character} Validation method used to assess fit of least-cost path against known route. Implemented methods include: 'euclidean' (Default), 'pdi', 'frechet', 'hausdorff'
#'
#' @param summary_function \code{function} If function supplied then this function is used to summarise the distance measurements for all modelled roads to a single value. Default is NULL
#'
#' @param tol \code{numeric} Maximum distance between simulated routes and the known route for the two lines to still be deemed equal. All simulated routes and their parameters that result in a simulated route with a maximum distance from the known route above this value are rejected
#'
#' @param ncores \code{numeric} Number of cores used during the routepath ABC procedure. Default value is 1
#'
#' @param spatial \code{logical} if TRUE (default) then sf object returned. if FALSE then sf geometry is dropped and a data.frame is returned
#'
#' @return \code{routepath}
#'
#' @author Joseph Lewis
#'
#' @importFrom foreach %dopar%
#'
#' @export
#'

ABC_rejection <- function(input_data, model, priors, known_routes, validation = "euclidean", summary_function = NULL, tol = NULL, ncores = 1, spatial = TRUE) {

  res <- max(terra::res(input_data[[1]]))

  input_data <- lapply(input_data, FUN = function(x) { if (inherits(x, "SpatRaster")) {terra::wrap(x)} else {x}})

  myCluster <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(myCluster)

  routes <- foreach::foreach(row_no = 1:nrow(priors), .combine = "rbind") %dopar% {

    processed_routes <- list()

    input_data2 <- lapply(input_data, FUN = function(x) { if (inherits(x, "PackedSpatRaster")) {terra::rast(x)} else {x}})

    for(j in 1:nrow(known_routes)) {

      road_ext <- sf::st_as_sf(sf::st_buffer(x = sf::st_as_sfc(sf::st_bbox(known_routes[j,])), dist = res * 10))

      input_data3 <- lapply(input_data2, FUN = function(x) { if (inherits(x, "SpatRaster")) {terra::crop(x, road_ext)} else {x}})

      conductanceMatrix <- model(x = input_data3, y = priors[row_no,, drop = FALSE])

      points <- extract_end_points(known_route = known_routes[j,])
      route <- calculate_routepath(x = conductanceMatrix, locations = points)
      distance <- calculate_distance(route = route, known_route = known_routes[j,], validation = validation)

      processed_routes[[j]] <- process_route(route = route, priors = priors, line_id = j, row_no = row_no, distance = distance, spatial = spatial)

    }

    processed_routes <- do.call(rbind, processed_routes)

    if(is.function(summary_function)) {
      processed_routes$stat <- summary_function(processed_routes$distance[!is.na(processed_routes$distance)])
    } else {
      processed_routes$stat <- NA
    }

    return(processed_routes)
  }

  parallel::stopCluster(myCluster)

  if (!is.null(tol)) {
    routes$result[routes$stat >= tol] <- "Reject"
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
