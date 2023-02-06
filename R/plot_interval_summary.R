plot_interval_summary <- function(summary, known_values = NULL) {

  cred_mass <- stats::na.omit(unique(as.numeric(gsub("[^0-9.-]", "", colnames(summary)))))

  summary$line_id <- factor(summary$line_id)

  if(!is.null(known_values)) {

    if(!length(known_values) == unique(summary$parameter)) {
      stop("supplied known values has to be the same length as the number of parameters. If a known value for a parameter is unknown then assign NA")
      }

    line_df <- data.frame(parameter = unique(summary$parameter), hline = known_values)
  }

  lower_cols <- colnames(summary)[grep(paste0(cred_mass[1], "$"), colnames(summary))]
  upper_cols <- colnames(summary)[grep(paste0(cred_mass[2], "$"), colnames(summary))]

  summary_plot <- ggplot2::ggplot(summary) +
    ggplot2::geom_linerange(ggplot2::aes_string(x = "line_id", ymin = upper_cols[1], ymax = upper_cols[2]), colour = "#A96769", size = 1) +
    ggplot2::geom_linerange(ggplot2::aes_string(x = "line_id", ymin = lower_cols[1], ymax = lower_cols[2]), colour = "#7B191E", size = 2) +
    ggplot2::geom_point(ggplot2::aes(x = line_id, y = mean), size = 3, fill = "#D3ADAE", pch = 21, colour = "#7B191E") +
    ggplot2::facet_wrap(~parameter, scales = "free_y", ncol = 1)

  if(!is.null(known_values)) {
    summary_plot <- summary_plot +
      ggplot2::geom_hline(data = line_df, ggplot2::aes(yintercept = hline), lty = 2, col = "grey60")
  }

  summary_plot <- summary_plot +
    ggplot2::labs(x = "Route no.", y = "", title = paste0("Posterior means\n", "with ", cred_mass[1]*100, "% and ", cred_mass[2]*100, "% intervals, ", length(unique(summary$line_id)), " Route(s)")) +
    ggplot2::theme_classic()

  return(summary_plot)

}
