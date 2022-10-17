#' calculates distance between simulated route and known route
#'
#' Calculates distance between simulated route and known route using the validation method chosen
#'
#' @param route \code{sf line} least-cost path
#'
#' @param known_route \code{sf line} known route
#'
#' @param validation \code{character} validation method used to assess fit of least-cost path against known route. Implemented methods include: 'euclidean' (Default), 'pdi', 'frechet', 'hausdorff'
#'
#' @return distance betwen simulated route and known route
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'

calculate_distance <- function(route, known_route, validation) {

  sf::st_crs(route) <- sf::st_crs(known_route)

  if (validation == "euclidean") {
    distance <- euclidean_distance(route, known_route)
  } else if (validation == "pdi") {
    distance <- pdi_distance(route, known_route)
  } else if (validation == "frechet") {
    distance <- frechet_distance(route, known_route)
  } else if (validation == "hausdorff") {
    distance <- hausdorff_distance(route, known_route)
  } else {
    stop("Validation method not implemented. Implemented methods include: 'euclidean', 'pdi', 'frechet', 'hausdorff'")
  }

  return(distance)
}
