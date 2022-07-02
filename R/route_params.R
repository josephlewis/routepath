#' plot summary of simulated routepath parameters
#'
#' @param routes simulated routes
#'
#' @param col_index column index of parameters to plot. if NULL then plot all parameters
#'
#' @param filename xxx
#'
#' @param xlab_names xxxx
#'
#' @param ... Additional arguments. See Details
#'
#' @author Joseph Lewis
#'
#' @export

route_params <- function(routes, col_index = NULL, filename = NULL, xlab_names = NULL, ...) {

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

  graphics::par(mfrow = c(ceiling(length(cols)/ceiling(length(cols)/5)), ceiling(length(cols)/5)))

  for(i in 1:length(cols)) {

    param_values <- routes[[cols[i]]]

    param_mean <- stats::aggregate(param_values, list(routes$line_id),  mean)
    param_mean <- cbind(param_mean, cols[i])
    colnames(param_mean) <- c("line_id", "mean", "param")

    param_low <- stats::aggregate(param_values, list(routes$line_id), min)[,2]
    param_high <- stats::aggregate(param_values, list(routes$line_id), max)[,2]

    hdi_param_low <- stats::aggregate(param_values, list(routes$line_id),  HDInterval::hdi)[,2][,1]
    hdi_param_high <- stats::aggregate(param_values, list(routes$line_id),  HDInterval::hdi)[,2][,2]

    plot(param_mean$line_id, param_mean$mean, xlab = "Route", ylab = x_labs[i], pch = 16, xaxt="n", panel.first= c(graphics::abline(v = seq(1, max(as.numeric(routes$line_id)), by = 1), col="cornsilk2", lty = 2), graphics::grid(NA, NULL, col="cornsilk2", lty = 2)), ylim = c(min(param_values), max(param_values)), xlim = c(1, max(routes$line_id)))
    graphics::title(LETTERS[i], adj = 0)

    for (j in 1:length(unique(routes$line_id))) {
      graphics::lines(cbind(sort(unique(routes$line_id))[j], sort(unique(routes$line_id))[j]), cbind(param_low[j], param_high[j]))
      graphics::lines(cbind(sort(unique(routes$line_id))[j], sort(unique(routes$line_id))[j]), cbind(hdi_param_low[j], hdi_param_high[j]), lwd = 2)
    }

    graphics::points(param_mean$line_id, param_low)
    graphics::points(param_mean$line_id, param_high)
    graphics::abline(h = mean(param_values), lty = 2)

    graphics::axis(1, at = 1:max(routes$line_id), las=2)

  }

  if(is.character(filename))  {
    invisible(utils::capture.output(suppressWarnings(grDevices::dev.print(grDevices::png, paste0(filename, "_Route_Parameters", ".png"), units = "in", res = 300, ...))))
    invisible(utils::capture.output(grDevices::dev.off()))
  }
}
