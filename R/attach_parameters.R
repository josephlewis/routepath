attach_parameters <- function(known_routes, parameters, ...) {

  known_routes <- base::merge(known_routes, parameters, ...)

  return(known_routes)

}
