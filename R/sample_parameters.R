sample_parameters <- function(parameters, no_samples) {

  param <- apply(X = parameters, MARGIN = 2, sample, no_samples, replace = TRUE)

  return(param)

}

