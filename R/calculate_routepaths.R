calculate_routepaths <- function(cost_surface, locations) {

  loc_matrix <- matrix(c(from = seq(from = 1, to = length(locations), by = 2), to = seq(from = 2, to = length(locations), by = 2)), ncol = 2)

  routepaths <- create_lcp_network(cost_surface = cost_surface, locations = locations, nb_matrix = loc_matrix)

  return(routepaths)

}
