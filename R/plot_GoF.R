#' Calculates route path from start and end locations of lines using user-supplied cost surface
#'
#' This function calculates the route path from the start and end locations of lines using a user-supplied cost surface
#'
#' @param routepaths routepaths of outputs from ABC rejection
#'
#' @param threshold numeric values of distances between known route and simulated routes
#'
#' @param normalise if TRUE (default) then percentage of routepaths within distance normalised to 1
#'
#' @return plot
#'
#' @author Joseph Lewis
#'
#' @import foreach
#' @import dplyr
#' @import ggplot2
#' @import sf
#'
#' @export

plot_GoF <- function(routepaths, thresholds, normalise = TRUE) {

    if (!inherits(routepaths, c("sf", "data.frame"))) {
        stop("routepaths argument is invalid. Expecting a sf or data.frame object")
    }

    if (!inherits(thresholds, c("numeric"))) {
        stop("thresholds argument is invalid. Expecting a numeric vector object")
    }

    if (inherits(routepaths, "sf")) {
        routepaths <- sf::st_drop_geometry(routepaths)
    }

    GoF <- foreach::foreach(thres = thresholds, .combine = "rbind", .packages = "dplyr") %do% {

        routepath_count <- routepaths %>% dplyr::filter(stats <= thres) %>% dplyr::group_by(line_id) %>% dplyr::count(line_id) %>% dplyr::mutate(threshold = thres)

    }

    routepath_df_raw <- data.frame(line_id = rep(1:length(unique(routepaths$line_id)), length(thresholds)), n = 0, threshold = rep(thresholds, each = length(unique(routepaths$line_id))))

    routepath_df_updated <- routepath_df_raw %>% dplyr::rows_update(GoF, by = c("line_id", "threshold"))

    if (normalise) {
        routepath_df_updated$n <- routepath_df_updated$n/max(table(routepaths$line_id)) * 100

        fill_lab <- "Percentage of Routepaths"

    } else {

        fill_lab <- "Number of Routepaths"

    }

    GoF_plot <- ggplot2::ggplot(data = routepath_df_updated, aes(x = line_id, y = threshold, fill = n)) + ggplot2::geom_tile() + ggplot2::scale_fill_gradientn(limits = c(0,
        max(routepath_df_updated$n)), colours = c("darkorange1", "darkmagenta", "navyblue")) + ggplot2::scale_x_continuous(breaks = unique(routepath_df_updated$line_id)) +
        ggplot2::scale_y_continuous(breaks = unique(routepath_df_updated$threshold)) + ggplot2::labs(x = "Road Number", y = "Threshold (m)", fill = fill_lab) +
        ggplot2::theme_classic() + ggplot2::theme(legend.position = "bottom") + ggplot2::guides(fill = guide_colourbar(barwidth = 30, label.position = "bottom",
        title.position = "top"))

    return(GoF_plot)

}
