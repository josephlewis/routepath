plot_residuals <- function(routepaths) {

  if (!inherits(routepaths, c("sf", "data.frame"))) {
    stop("routepaths argument is invalid. Expecting a sf or data.frame object")
  }

  if (inherits(routepaths, "sf")) {
    routepaths <- sf::st_drop_geometry(routepaths)
  }

  minimum_total_GoF <- routepaths %>%
    group_by(line_id) %>%
    slice(which.min(stats))

  minimum_group_GoF <- routepaths %>%
    group_by(param_row) %>%
    dplyr::summarize(Mean = mean(stats, na.rm=TRUE)) %>%
    slice(which.min(Mean))

  minimum_total_GoF$line_id <- as.factor(minimum_total_GoF$line_id)

  ggplot() +
    geom_point(data = minimum_total_GoF, mapping = aes(x = line_id, y = stats), size = 4) +
    geom_hline(yintercept= minimum_group_GoF$Mean, linetype="dashed", color = "red") +
    theme_classic()

}
