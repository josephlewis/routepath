#' Change tolerance of accepted simulated routepaths
#'
#' @param routepaths simulated routepaths
#'
#' @param tolerance Maximum tolerance value for routepaths to be accepted
#'
#' @return routepaths
#'
#' @author Joseph Lewis

change_tolerance <- function(routepaths, tolerance) {

  routepaths$result <- "Accept"
  routepaths[routepaths$stats > tolerance ] <- "Reject"

  return(routepaths)

}
