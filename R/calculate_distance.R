#' calculates distance between routepath and known route
#'
#' Calculates distance between routepath and known route using a number of validation methods.
#'
#' Implemented methods include: 'euclidean' (Default), 'pdi', 'frechet', 'hausdorff'
#'
#' @param route \code{sf line} routepath
#'
#' @param known_route \code{sf line} known route
#'
#' @param validation \code{character} validation method used to assess fit of least-cost path against known route. Implemented methods include: 'euclidean' (Default), 'pdi', 'frechet', 'hausdorff'
#'
#' @return \code{numeric} distance between routepath and known route
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'

calculate_distance <- function(route, known_route, validation) {

  # if(is.list(validation)) {
  #
  #   if(!all(sapply(validation, is.function))) {
  #     stop("At least one element in the validation list is not a function")
  #   }
  #
  #   distance <- list()
  #
  #   for(v in 1:length(validation)) {
  #     FUN <- validation[[v]]
  #     distance[[v]] <- FUN(route, known_route)
  #   }
  #
  #   distance <- do.call(cbind, distance)

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
