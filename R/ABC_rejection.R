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
#' @param ncores \code{numeric} Number of cores used during the routepath ABC procedure. Default value is 1
#'
#' @param spatial \code{logical} if TRUE (default) then sf object returned. if FALSE then sf geometry is dropped and a data.frame is returned
#'
#' @param expand \code{numeric} Maximum resolution multiplier used to expand the window around the known route when cropping SpatRaster objects in input_data. Default is a value of 10. For example, if the maximum resolution of the SpatRaster object is 100m and the expand value is 10 the window will be buffered by 1,000m (100m*10)
#'
#' @param verbose \code{logical} if TRUE message returned detailing the current known route number (and percentage) being simulated. Default value is FALSE
#'
#' @return \code{routepath}
#'
#' @author Joseph Lewis
#'
#' @importFrom foreach %dopar%
#'
#' @export
#'
#'@examples
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
#' euclidean_distance <- function(route, known_route) {
#' route_pts <- sf::st_cast(route, "POINT", warn = FALSE)
#' distance <- max(sf::st_distance(x = route_pts, y = known_route))
#' distance <- as.numeric(distance)
#' return(distance)
#' }
#'
#' routepath <- ABC_rejection(input_data = input_data, model = model,
#' priors = priors, known_routes = known_route, validation = "euclidean")
#'
#' routepath2 <- ABC_rejection(input_data = input_data, model = model,
#' priors = priors, known_routes = known_route,
#' validation = list(euclidean_distance, euclidean_distance))

ABC_rejection <- function(input_data, model, priors, known_routes, validation = "euclidean", ncores = 1, spatial = TRUE, expand = 10, verbose = FALSE) {

  res <- max(terra::res(input_data[[which(sapply(input_data, class) == "SpatRaster")[1]]]))

  nrows <- nrow(known_routes) * nrow(priors)
  NAs <- rep(NA, nrows)

  processed_routes <- suppressWarnings(sf::st_sf(costFunction = NAs,
                                                 fromCell = NAs,
                                                 toCell = NAs,
                                                 line_id = NAs,
                                                 param_row = NAs,
                                                 matrix(NA, nrow = length(NAs), ncol = ncol(priors), byrow = TRUE, dimnames = list(NULL, paste0("p.", colnames(priors)))),
                                                 matrix(NA, nrow = length(NAs), ncol = length(validation), byrow = TRUE, dimnames = list(NULL, paste0("s.", 1:length(validation)))),
                                                 geometry = sf::st_sfc(lapply(1, function(x) sf::st_linestring())),
                                                 crs = sf::st_crs(known_routes)))

  for(j in 1:nrow(known_routes)) {

    if(verbose) {
      message("Simulating Route No: ", j, "/", nrow(known_routes), " (", round(j/nrow(known_routes), 3)*100, "%)")
    }

    myCluster <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(myCluster)

    road_ext <- sf::st_as_sf(sf::st_buffer(x = sf::st_as_sfc(sf::st_bbox(known_routes[j,])), dist = res * expand))

    input_data1 <- lapply(input_data, FUN = function(x) { if (inherits(x, "SpatRaster")) {terra::crop(x, road_ext)} else {x}})
    input_data2 <- lapply(input_data1, FUN = function(x) { if (inherits(x, "SpatRaster")) {terra::wrap(x)} else {x}})

    routes <- foreach::foreach(row_no = 1:nrow(priors), .combine = "rbind") %dopar% {

      input_data3 <- lapply(input_data2, FUN = function(x) { if (inherits(x, "PackedSpatRaster")) {terra::rast(x)} else {x}})

      conductanceMatrix <- model(x = input_data3, y = priors[row_no,, drop = FALSE])

      points <- extract_end_points(known_route = known_routes[j,])
      route <- leastcostpath::create_lcp(x = conductanceMatrix, origin = points[1,], destination = points[2,], cost_distance = FALSE)
      distance <- calculate_distance(route = route, known_route = known_routes[j,], validation = validation)

      route2 <- process_route(route = route, priors = priors, line_id = j, row_no = row_no, distance = distance, spatial = spatial)

      return(route2)

    }

    parallel::stopCluster(myCluster)

    index_start <- seq(1, nrow(known_routes) * nrow(priors), by = nrow(known_routes) * nrow(priors) / nrow(known_routes))
    index_end <- seq(nrow(priors), nrow(known_routes) * nrow(priors), by = nrow(priors))

    processed_routes[index_start[j]:index_end[j],] <- routes

  }

  processed_routes2 <- process_routepaths(routes = processed_routes, model = model, validation = validation)
  return(processed_routes2)

}


#' @export

print.routepath <- function(x) {
  cat("class:", class(x))
  cat("\ntotal no. of summary stats:", sum(grepl(pattern = "s.", x = colnames(x$routes), fixed = TRUE)))
  cat("\nno. of routes:", length(unique(x$routes$line_id)))
  cat("\ntotal no. of simulations:", nrow(x$routes))
  cat("\ntotal no. of parameters:", sum(grepl(pattern = "p.", x = colnames(x$routes), fixed = TRUE)))
}
