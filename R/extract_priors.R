#' Extract parameter values from routepaths
#'
#' @param routepaths Extract parameter values from routepaths
#'
#' @param lines routepaths
#'
#' @return parameter values
#'
#' @author Joseph Lewis
#'
#' @export

extract_priors <- function(routepaths) {

  if (!inherits(routepaths, c("sf", "data.frame"))) {
    stop("routepaths argument is invalid. Expecting a sf or data.frame object")
  }

  if(inherits(routepaths, "sf")) {
    routepaths <- sf::st_drop_geometry(routepaths)
  }

  start <- which(names(routepaths) == "row_no") + 1
  end <- which(names(routepaths) == "stats") - 1

  priors <- routepaths[,start:end]

  return(priors)

}
