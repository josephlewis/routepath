#' plot summary of simulated routepath stats
#'
#' @param routes simulated routes
#'
#' @param filename xxxxx
#'
#' @param ... Additional arguments. See Details
#'
#' @author Joseph Lewis
#'
#' @export

route_stats <- function(routes, filename = NULL, ...) {

  routes$result <- factor(routes$result, levels = c("Reject", "Accept"))

  routes_accept <- routes[routes$result == "Accept",]
  routes_reject <- routes[routes$line_id %in% which(table(routes$line_id, routes$result)[,2] == 0) & !is.na(routes$stats),]

  if(nrow(routes_accept) != 0 & nrow(routes_reject) != 0) {
    graphics::layout(matrix(c(1,1,2,2,3,3), ncol = 2, byrow = TRUE))
    # bottom, left, top and right margins
    graphics::par(mai = c(0.8, 0.8, 0.3, 0.1))

    a_letter_plot <- "B"
    r_letter_plot <- "C"

  } else {
    graphics::layout(matrix(c(1,1,2,2), ncol = 2, byrow = TRUE))
    graphics::par(mai = c(0.8, 0.8, 0.3, 0.1))

    a_letter_plot <- "B"
    r_letter_plot <- "B"
  }

  plot(x = routes$line_id, y = routes$stats, ylim = rev(c(0, max(routes$stats, na.rm = TRUE))),
       xlab = "All simulated routes", ylab = "Distance from known route", pch = 16, col = routes$result, xaxt="n", c(graphics::abline(v = seq(1, max(routes$line_id), by = 1), col="grey")))
  graphics::title("A", adj = 0)
  graphics::axis(1, at = 1:max(routes$line_id), las=2)

  if(nrow(routes_accept) > 0) {

    routes_accept$line_id <- factor(routes_accept$line_id, levels = sort(unique(routes_accept$line_id)))

    plot(x = routes_accept$line_id, y = routes_accept$stats, ylim = rev(c(0, ceiling(max(routes_accept$stats, na.rm = TRUE)))), xlab = "All accepted routes", ylab = "Distance from known route", col = routes_accept$result)
    graphics::title(a_letter_plot, adj = 0)

  }

  if(nrow(routes_reject) > 0) {

    routes_reject$line_id <- factor(routes_reject$line_id, levels = sort(unique(routes_reject$line_id)))

    plot(x = routes_reject$line_id, y = routes_reject$stats, ylim = rev(c(floor(min(routes_reject$stats, na.rm = TRUE)), ceiling(max(routes_reject$stats, na.rm = TRUE)))), xlab = "All rejected routes", ylab = "Distance from known route", col = routes_reject$result)
    graphics::title(r_letter_plot, adj = 0)

  }

  if(is.character(filename))  {
    invisible(utils::capture.output(suppressWarnings(grDevices::dev.print(grDevices::png, paste0(filename, "_Route_Stats", ".png"), units = "in", res = 300, ...))))
    invisible(utils::capture.output(grDevices::dev.off()))
  }
}
