#' @keywords internal

process_parameters <- function(routepaths, lines, priors, validation, summary_stat_target, tol) {

  points_matrix <- matrix(c(1:length(routepaths), rep(1:length(lines), times = nrow(priors))), ncol = 2)

  if(validation == "max_distance") {

    summarystats <- max_distance(routepaths, lines, points_matrix)

  }

  parameters <- priors[rep(seq_len(nrow(priors)), rep(length(lines), nrow(priors))), 1:ncol(priors)]
  parameters <- cbind(line_id = rep(1:length(lines), times = nrow(priors)), param = parameters, stats = summarystats)

  parameters <- split.data.frame(parameters, parameters[,"line_id"])

  parameters_abc <- lapply(X = parameters, FUN = function(x) { rej <- abc::abc(target = summary_stat_target, param = x[,2:ncol(x)], sumstat = x[,"stats"], tol = tol, method = "rejection")
  })

  parameters_abc <- cbind(line_id = rep(1:length(lines), each = sum(parameters_abc[[1]]$region)),
                          do.call(rbind, lapply(X = parameters_abc, FUN = function(x) { x$unadj.values})))

  return(parameters_abc)

}
