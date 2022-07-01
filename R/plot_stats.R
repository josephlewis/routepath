plot_stats <- function(routes, filename = NULL, ...) {

  routes$result <- factor(routes$result, levels = c("Reject", "Accept"))

  routes_df_accept <- routes[routes$result == "Accept",]

  routes_df_accept$line_id <- factor(routes_df_accept$line_id, levels = sort(unique(routes_df_accept$line_id)))

  if(nrow(routes_df_accept) == 0) {
    layout(matrix(c(1,1), ncol = 1, byrow = TRUE))
    # bottom, left, top and right margins
    par(mai = c(0.8, 0.8, 0.3, 0.1))
  } else {
    layout(matrix(c(1,1,2,2), ncol = 2, byrow = TRUE))
    par(mai = c(0.8, 0.8, 0.3, 0.1))
  }

  plot(x = routes$line_id, y = routes$stats, ylim = rev(c(0, max(routes$stats))),
       xlab = "All simulated routes", ylab = "Distance from known route", pch = 16, col = routes$result, xaxt="n", c(abline(v = seq(1, max(routes$line_id), by = 1), col="grey")))
  title("A", adj = 0)
  axis(1, at = 1:max(routes$line_id), las=2)

  if(nrow(routes_df_accept) > 0) {

  plot.default(x = routes_df_accept$line_id, y = routes_df_accept$stats, ylim = rev(c(0, ceiling(max(routes_df_accept$stats)))), xlab = "All Accepted Routes", ylab = "Distance from known route", pch = 16, col = routes_df_accept$result, xaxt="n",panel.first = c(abline(v = seq(1, max(as.numeric(routes_df_accept$line_id)), by = 1), col="grey")))
  title("B", adj = 0)
  axis(1, at = 1:length(unique(routes_df_accept$line_id)), labels = sort(unique(routes_df_accept$line_id)), las = 2)

  }

  if(is.character(filename))  {
    invisible(capture.output(suppressWarnings(dev.print(png, paste0(filename, "_Route_Stats", ".png"), units = "in", res = 300, ...))))
    invisible(capture.output(dev.off()))
  }
}




