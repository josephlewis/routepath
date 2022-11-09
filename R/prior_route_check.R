#' simulate prior route realisations based on supplied prior parameter values and a model
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
#' @param ncores \code{numeric} Number of cores used during the routepath ABC procedure. Default value is 1
#'
#' @return \code{sf line} simulated routes
#'
#' @author Joseph Lewis
#'
#' @export
#'

prior_route_check <- function(input_data, model, priors, known_routes, ncores = 1) {

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

      route$line_id <- route_no
      route$param_row <- row_no
      route <- cbind(route, p = priors[row_no,, drop = FALSE])

      processed_routes[[route_no]] <- route

    }

    processed_routes <- do.call(rbind, processed_routes)

  }

  parallel::stopCluster(myCluster)

  return(routes)

}
