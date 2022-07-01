create_wlc_surface <- function(input_data, model, values) {

  cost_surface <- model(input_data, values)

  return(cost_surface)

}
