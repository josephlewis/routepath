plot_parameters_summary <- function(routes, col_index = NULL, filename = NULL, xlab_names = NULL, ...) {

  routes <- routes[routes$result == "Accept",]

  if(nrow(routes[routes$result == "Accept",]) == 0) {
      stop("No accepted simulated routes. Increase threshold value of acceptance using change_tolerance()")
    }

  if(is.null(col_index)) {
    col_names <- colnames(routes)
    cols <- c(col_names[4:(which(col_names == "stats") - 1)])
  } else {
    col_names <- colnames(routes)
    cols <- col_names[col_index]
  }

  if(!is.null(xlab_names)) {
    x_labs <- xlab_names
    if (length(x_labs) == 1) {
      x_labs <- rep(x_labs, length.out = length(cols))
    }
  } else  {
    x_labs <- cols
  }

  # bottom, left, top and right margins
  par(mfrow = c(length(cols), 1), mai = c(0.6, 0.7, 0.3, 0.1))

  for(i in 1:length(cols)) {

    param_values <- routes[[cols[i]]]

    param_mean <- aggregate(param_values, list(routes$line_id),  mean)
    param_mean <- cbind(param_mean, cols[i])
    colnames(param_mean) <- c("line_id", "mean", "param")

    param_low <- aggregate(param_values, list(routes$line_id), min)[,2]
    param_high <- aggregate(param_values, list(routes$line_id), max)[,2]

    hdi_param_low <- aggregate(param_values, list(routes$line_id),  HDInterval::hdi)[,2][,1]
    hdi_param_high <- aggregate(param_values, list(routes$line_id),  HDInterval::hdi)[,2][,2]

    plot(param_mean$line_id, param_mean$mean, xlab = "Route", ylab = x_labs[i], pch = 16, xaxt="n", panel.first= c(abline(v = seq(1, max(as.numeric(routes$line_id)), by = 1), col="cornsilk2", lty = 2), grid(NA, NULL, col="cornsilk2", lty = 2)), ylim = c(min(param_values), max(param_values)), xlim = c(1, max(routes$line_id)))
    title(LETTERS[i], adj = 0)

    for (j in 1:length(unique(routes$line_id))) {
      lines(cbind(sort(unique(routes$line_id))[j], sort(unique(routes$line_id))[j]), cbind(param_low[j], param_high[j]))
      lines(cbind(sort(unique(routes$line_id))[j], sort(unique(routes$line_id))[j]), cbind(hdi_param_low[j], hdi_param_high[j]), lwd = 2)
    }

    points(param_mean$line_id, param_low)
    points(param_mean$line_id, param_high)
    abline(h = mean(param_values), lty = 2)

    axis(1, at = 1:max(routes$line_id), las=2)

  }

  if(is.character(filename))  {
    invisible(capture.output(suppressWarnings(dev.print(png, paste0(filename, "_Route_Parameters", ".png"), units = "in", res = 300, ...))))
    invisible(capture.output(dev.off()))
  }
}
