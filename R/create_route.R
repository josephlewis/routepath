#' Create a route using predefined parameter values
#'
#' This function creates a route using predefined parameter values.
#'
#' @details xxx
#'
#' @param input_data a list of input data to be used in the route modelling process
#'
#' @param values a matrix of defined parameter values to be used in model function
#'
#' @param model An R function implementing the route model to be used when creating a route
#'
#' @param origin xx
#'
#' @param destination xx
#'
#' @return sf line
#'
#' @author Joseph Lewis
#'
#' @import sf
#'
#' @export

create_route <- function(input_data, model, values, origin, destination) {
  
  ## check if nrow of values not one
  ## check if values is a matrix
  
  cost_surface <- model(input_data, values)
  
  points <- rbind(origin, destination)
  
  route <- calculate_routepaths(cost_surface = cost_surface, locations = points)
  
  route <- sf::st_as_sf(route)
  route <- cbind(route, values)
  
  return(route)
  
}