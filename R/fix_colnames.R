#' Fixes column names
#'
#' @param routepaths simulated routepaths
#'
#' @return routepaths
#'
#' @author Joseph Lewis

fix_colnames <- function(routepaths) {

  colnames(routepaths)[c(1:3)] <- c("direction", "line_id", "param_row")

  # if sf class then need to account for geometry column
  if(inherits(routepaths, "sf")) {
    colnames(routepaths)[length(colnames(routepaths))-1] <- c("result")
    colnames(routepaths)[length(colnames(routepaths))-2] <- c("stats")
  } else {
    colnames(routepaths)[length(colnames(routepaths))] <- c("result")
    colnames(routepaths)[length(colnames(routepaths))-1] <- c("stats")
  }
  return(routepaths)
}
