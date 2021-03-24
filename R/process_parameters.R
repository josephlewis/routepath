#' @keywords internal

process_parameters <- function(routepaths, lines, priors, validation) {

  points_matrix <- matrix(c(1:length(routepaths), rep(1:length(lines), times = nrow(priors))), ncol = 2)

  if(validation == "max_distance") {

    summarystats <- max_distance(routepaths, lines, points_matrix)

  }

  parameters <- priors[rep(seq_len(nrow(priors)), rep(length(lines), nrow(priors))), 1:ncol(priors)]
  parameters <- cbind(line_id = rep(1:length(lines), times = nrow(priors)), param = parameters, stats = summarystats)

  parameters <- split.data.frame(parameters, parameters[,"line_id"])

  return(parameters)

}
