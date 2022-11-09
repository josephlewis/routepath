#' routepath rejection sampling scheme using Approximate Bayesian Computation
#'
#' Launches a series of model simulations using user-supplied prior parameter values. See details for more information
#'
#' @details xxx
#'
#' @param input_data \code{list} Data used in the routepath ABC modelling procedure
#'
#' @param priors \code{Matrix} Prior parameter values
#'
#' @param model \code{function} Model to be used when simulating routes. This function requires that a \code{conductanceMatrix} is returned
#'
#' @param known_routes \code{sf line} Known routes to compare against the simulated routes
#'
#' @param validation \code{character} Validation method used to assess fit of least-cost path against known route. Implemented methods include: 'euclidean' (Default), 'pdi', 'frechet', 'hausdorff'
#'
#' @param summary_function \code{function} If function supplied (default NULL) then this function is used to summarise the distance measurements for all roads to a single value. if NULL then maximum distance is used (maximum deviation from known route)
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
#' @examples
#'
#' library(leastcostpath)
#'
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' slope_cs <- create_slope_cs(x = r, cost_function = "tobler", neighbours = 4)
#' locs <- sf::st_sf(geometry = sf::st_sfc(
#' sf::st_point(c(839769, 4199443)),
#' sf::st_point(c(1038608, 4100024)),
#' crs = terra::crs(r)))
#'
#' known_route <- leastcostpath::create_lcp(x = slope_cs, origin = locs[1,],
#'                                          destination = locs[2,], cost_distance = FALSE)
#'
#' input_data <- list(r)
#'
#' nsims <- 10
#' priors <- cbind(a = 1, b = rnorm(n = nsims, mean = 3.5, sd = 1),
#' c = rnorm(n = nsims, mean = 0.05, sd = 0.1))
#'
#' model <- function(x, y) {
#' slope_cs <- leastcostpath::create_slope_cs(x = x[[1]],
#' cost_function = function(x) {(y[1] * exp(-y[2] * abs(x + y[3]))) / 3.6},
#' neighbours = 4)
#'
#' return(slope_cs)
#' }
#'
#' routepath <- ABC_rejection(input_data = input_data, model = model,
#' priors = priors, known_routes = known_route, validation = "euclidean", tol = NULL)

ABC_rejection <- function(input_data, model, priors, known_routes, validation = "euclidean", summary_function = NULL, tol = NULL, ncores = 1, spatial = TRUE) {

  res <- max(terra::res(input_data[[1]]))

  input_data <- lapply(input_data, FUN = function(x) { if (inherits(x, "SpatRaster")) {terra::wrap(x)} else {x}})

  myCluster <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(myCluster)

  routes <- foreach::foreach(row_no = 1:nrow(priors), .combine = "rbind") %dopar% {

    processed_routes <- list()

    for(route_no in 1:nrow(known_routes)) {

      road_ext <- sf::st_as_sf(sf::st_buffer(x = sf::st_as_sfc(sf::st_bbox(known_routes[route_no,])), dist = res * 10))

      input_data2 <- lapply(input_data, FUN = function(x) { if (inherits(x, "PackedSpatRaster")) {terra::rast(x)} else {x}})
      input_data3 <- lapply(input_data2, FUN = function(x) { if (inherits(x, "SpatRaster")) {terra::crop(x, road_ext)} else {x}})

      conductanceMatrix <- model(x = input_data3, y = priors[row_no,])

      points <- extract_end_points(known_route = known_routes[route_no,])
      route <- calculate_routepath(x = conductanceMatrix, locations = points)
      distance <- calculate_distance(route = route, known_route = known_routes[route_no,], validation = validation)

      processed_routes[[route_no]] <- process_route(route = route, priors = priors, route_no = route_no, row_no = row_no, distance = distance, spatial = spatial)

    }

    processed_routes <- do.call(rbind, processed_routes)

    if(is.function(summary_function)) {
      processed_routes$stat <- summary_function(processed_routes$distance[!is.na(processed_routes$distance)])
    } else {
      processed_routes$stat <- max(x = processed_routes$distance, na.rm = TRUE)
      processed_routes$stat[is.infinite(processed_routes$stat)] <- NA
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
