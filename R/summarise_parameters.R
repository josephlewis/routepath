summarise_parameters <- function(routepaths, col_index, fun) {

  if(any(col_index > ncol(routepaths))){
    stop("col_index outside of possible columns")
  }

  accepted_routepaths <- routepaths[routepaths$result == "Accept",]
  func_vector <- rep(fun, length.out = length(col_index))

  params <- list()

  for(i in 1:length(col_index)) {
    summarised_param <- aggregate(accepted_routepaths[[col_index[i]]], list(accepted_routepaths$line_id), func_vector[i])
    summarised_param <- summarised_param[,2, drop = FALSE]
    colnames(summarised_param) <- colnames(accepted_routepaths)[col_index[i]]
    params[[i]] <- summarised_param
  }

  params <- do.call(cbind, params)

  params_summary <- cbind(line_id = unique(accepted_routepaths$line_id), params)
  params_summary <- params_summary[order(params_summary$line_id),]

  return(params_summary)

}

