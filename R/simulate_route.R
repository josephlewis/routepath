#' simulate route realisations based on supplied parameter values and a model
#'
#' @details xxx
#'
#' @param input_data \code{list} Data used in the routepath ABC modelling procedure
#'
#' @param values \code{Matrix} Parameter values to be used in the model. Only the first row of the Matrix is used
#'
#' @param model \code{function} Model to be used when simulating routes. This function requires that a \code{conductanceMatrix} is returned
#'
#' @param known_routes \code{sf line} Known routes used for origin and destinations of simulated routes#
#'
#' @param ncores \code{numeric} Number of cores used during the routepath ABC procedure. Default value is 1
#'
#' @return \code{sf line} simulated routes
#'
#' @author Joseph Lewis
#'
#' @export
#'

simulate_routes <- function(input_data, model, values, known_routes, ncores = 1) {

  res <- max(terra::res(input_data[[1]]))

  input_data <- lapply(input_data, FUN = function(x) { if (inherits(x, "SpatRaster")) {terra::wrap(x)} else {x}})

  myCluster <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(myCluster)

  routes <- foreach::foreach(route_no = 1:nrow(known_routes), .combine = "rbind") %dopar% {

    road_ext <- sf::st_as_sf(sf::st_buffer(x = sf::st_as_sfc(sf::st_bbox(known_routes[route_no,])), dist = res * 10))

    input_data2 <- lapply(input_data, FUN = function(x) { if (inherits(x, "PackedSpatRaster")) {terra::rast(x)} else {x}})
    input_data3 <- lapply(input_data2, FUN = function(x) { if (inherits(x, "SpatRaster")) {terra::crop(x, road_ext)} else {x}})

    conductanceMatrix <- model(x = input_data3, y = values[1,])

    points <- extract_end_points(known_route = known_routes[route_no,])
    route <- calculate_routepath(x = conductanceMatrix, locations = points)

    route$line_id <- route_no
    route <- cbind(route, p = values[1,, drop = FALSE])

    return(route)

  }

  return(routes)

}
