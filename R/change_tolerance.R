#' Change tolerance of accepted simulated routes
#'
#' @param routes simulated routes
#'
#' @param tolerance Maximum tolerance value for routes to be accepted
#'
#' @param type absolute or percentage
#'
#' @return routes
#'
#' @author Joseph Lewis

change_tolerance <- function(routes, tolerance, type = "absolute") {

  if(tolerance < 0) {
    stop("tolerance must be above zero")
  }

  if(!type %in% c("absolute", "percentage")) {
    stop("invalid type argument. Expecting 'absolute' or 'percentage")
  }

  routes$result[!is.na(routes$stats)] <- "Accept"
  routes$result[is.na(routes$stats)] <- "Reject"

  if(type == "absolute") {
    routes$result[routes$stats > tolerance & !is.na(routes$stats)] <- "Reject"
  }

  if(type == "percentage") {
    if(tolerance >= 100) {
      stop("tolerance must be less than 100 percent when type is 'percentage'")
    }

  routes_not_na <- routes[!is.na(routes$stats),]
  routes_ordered <- routes_not_na[order(routes_not_na$line_id, routes_not_na$stats, decreasing = FALSE),]
  routes_rejected <- routes_ordered[stats::ave(routes_ordered$stats, routes_ordered$line_id, FUN = seq_along) > sum(routes$line_id == 1) * (tolerance/100),]

  routes[as.numeric(rownames(routes_rejected)),]$result <- "Reject"

  }

  return(routes)
}
