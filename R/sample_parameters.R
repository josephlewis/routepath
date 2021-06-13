sample_parameters <- function(parameters, no_samples) {

  parameters <- apply(X = parameters, MARGIN = 2, sample, no_samples, replace = TRUE)

  return(parameters)

}
