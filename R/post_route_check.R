#' simulate posterior route realisations based on a supplied routepath
#'
#' @details xxx
#'
#' @param input_data \code{list} Data used in the routepath ABC modelling procedure
#'
#' @param routepath \code{routepath} routepath object containing the accepted parameter values
#'
#' @param model \code{function} Model to be used when simulating routes. This function requires that a \code{conductanceMatrix} is returned
#'
#' @param known_routes \code{sf line} Known routes to compare against the simulated routes
#'
#' @param validation \code{character} Validation method used to assess fit of least-cost path against known route. Implemented methods include: 'euclidean' (Default), 'pdi', 'frechet', 'hausdorff'
#'
#' @param summary_function \code{function} If function supplied (default NULL) then this function is used to summarise the distance measurements for all roads to a single value. if NULL then maximum distance is used (maximum deviation from known route)
#'
#' @param draws \code{numeric} Number of samples to draw from posterior parameter values. Default is 100
#'
#' @param ncores \code{numeric} Number of cores used during the routepath ABC procedure. Default value is 1
#'
#' @param spatial \code{logical} if TRUE (default) then sf object returned. if FALSE then sf geometry is dropped and a data.frame is returned
#'
#' @return \code{routepath}
#'
#' @author Joseph Lewis
#'
#' @export
#'

post_route_check <- function(input_data, model, routepath, known_routes, validation = "euclidean", summary_function = NULL, draws = 100, ncores = 1, spatial = TRUE) {

  res <- max(terra::res(input_data[[1]]))

  input_data <- lapply(input_data, FUN = function(x) { if (inherits(x, "SpatRaster")) {terra::wrap(x)} else {x}})

  myCluster <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(myCluster)

  if(inherits(routepath$routes, "sf")) {
    routes_df <- sf::st_drop_geometry(routepath$routes)
  } else {
    routes_df <- routepath$routes
  }

  param_cols <- which(grepl(pattern = "p.", colnames(routes_df), fixed = TRUE))
  post_vals <- routes_df[routes_df$result == "Accept", param_cols]

  if(nrow(post_vals) == 0) {
    stop("Unable to create a route using posterior values as no simulations are accepted. Use update_tolerance() to modify tolerance required for simulation to be 'Accepted'")
  }

  if(nrow(post_vals) > 1) {
    post_vals <- apply(X = post_vals, MARGIN = 2, FUN = function(x) { sample(x, size = draws, replace = TRUE)})
  } else {
    draws <- 1
  }

  routes <- foreach::foreach(row_no = 1:nrow(post_vals), .combine = "rbind") %dopar% {

    processed_routes <- list()

    for(route_no in 1:nrow(known_routes)) {

      road_ext <- sf::st_as_sf(sf::st_buffer(x = sf::st_as_sfc(sf::st_bbox(known_routes[route_no,])), dist = res * 10))

      input_data2 <- lapply(input_data, FUN = function(x) { if (inherits(x, "PackedSpatRaster")) {terra::rast(x)} else {x}})
      input_data3 <- lapply(input_data2, FUN = function(x) { if (inherits(x, "SpatRaster")) {terra::crop(x, road_ext)} else {x}})

      conductanceMatrix <- model(x = input_data3, y = post_vals[row_no,])

      points <- extract_end_points(known_route = known_routes[route_no,])
      route <- calculate_routepath(x = conductanceMatrix, locations = points)
      distance <- calculate_distance(route = route, known_route = known_routes[route_no,], validation = validation)

      processed_routes[[route_no]] <- process_route(route = route, priors = post_vals, route_no = route_no, row_no = row_no, distance = distance, spatial = spatial)

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

  tol <- routepath$tolerance

  if (!is.null(tol)) {
    routes$result[routes$stat >= tol] <- "Reject"
  }

  routepaths <- process_routepaths(routes = routes, model = model, validation = validation, tol = tol)

  return(routepaths)

}
