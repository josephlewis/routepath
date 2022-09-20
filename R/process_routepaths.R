#' Process routepaths
#'
#' @param routes \code{sf line} least-cost paths
#'
#' @param model \code{function} model function used to create \code{conductanceMatrix} used to calculate least-cost paths
#'
#' @param validation \code{character} Validation method used to assess fit of least-cost path against known route. Implemented methods include: 'euclidean' (Default), 'pdi', 'frechet', 'hausdorff'
#'
#' @param tol \code{numeric} maximum distance between simulated routes and known route to be deemed equal. All simulated routes above the maximum value are rejected
#'
#' @return \code{routepath}
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'

process_routepaths <- function(routes, model, validation, tol) {

  routepaths <- list("routes" = routes,
                     "model" = model,
                     "validation" = validation,
                     "tolerance" = ifelse(test = inherits(tol, "numeric"), yes = as.character(tol), no = NA))

  class(routepaths) <- "routepath"

  return(routepaths)

}
