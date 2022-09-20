#' update tolerance
#'
#' Updates the maximum distance between simulated routes and known route to be deemed equal. All simulated routes above this value are rejected
#'
#' @param routepath \code{routepath}
#'
#' @param tol \code{numeric} maximum distance between simulated routes and known route to be deemed equal
#'
#' @param type \code{character} 'abolute' or 'percentage'. if 'absolute' (default) then all simulated routes above this value are rejected. if 'percentage' then all simulated routes with a stat above the supplied percentage are rejected
#'
#' @return \code{routepath}
#'
#' @keywords internal
#'
#' @author Joseph Lewis
#'

update_tolerance <- function(routepath, tol, type = "absolute") {

  if(tol < 0) {
    stop("tolerance must be above zero")
  }

  if(!type %in% c("absolute", "percentage")) {
    stop("Invalid type argument. Expects either 'absolute' or 'percentage'")
  }

  routepath$routes$result[!is.na(routepath$routes$stat)] <- "Accept"
  routepath$routes$result[is.na(routepath$routes$stat)] <- "Reject"

  if(type == "absolute") {
    routepath$routes$result[routepath$routes$stat > tol & !is.na(routepath$routes$stat)] <- "Reject"
  }

  if(type == "percentage") {
    if(tol >= 100) {
      stop("tolerance must be less than 100 percent when type is 'percentage'")
    }

    routepath_not_na <- routepath$routes[!is.na(routepath$routes$stat),]
    routepath_ordered <- routepath_not_na[order(routepath_not_na$line_id, routepath_not_na$stat, decreasing = FALSE),]
    routepath_accepted <- routepath_ordered[!stats::ave(routepath_ordered$stat, routepath_ordered$line_id, FUN = seq_along) > sum(routepath$routes$line_id == 1) * (tol/100),]
    routepath_max_stat <- max(routepath_accepted$stat)

    routepath$routes$result[routepath$routes$stat > routepath_max_stat & !is.na(routepath$routes$stat)] <- "Reject"

  }

  routepath$tolerance <- ifelse(test = type == "absolute", yes = as.character(tol), no = paste0(as.character(tol), "%"))

  return(routepath)

}
