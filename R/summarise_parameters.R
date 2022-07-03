#' Summarise parameters
#'
#' @param routes simulated routes
#'
#' @param col_index ...
#'
#' @param fun ....
#'
#' @return df
#'
#' @author Joseph Lewis
#'
#' @export

summarise_parameters <- function(routes, col_index, fun) {

  # do if col_index NULL then do all. check other functions for how I've done this

  if(any(col_index > ncol(routes))){
    stop("col_index outside of possible columns")
  }

  accepted_routepaths <- routes[routes$result == "Accept",]
  func_vector <- rep(fun, length.out = length(col_index))

  params <- list()

  for(i in 1:length(col_index)) {
    summarised_param <- stats::aggregate(accepted_routepaths[[col_index[i]]], list(accepted_routepaths$line_id), func_vector[i])
    summarised_param <- summarised_param[,2, drop = FALSE]
    colnames(summarised_param) <- paste0(colnames(accepted_routepaths)[col_index[i]], "_", func_vector[i])
    params[[i]] <- summarised_param
  }

  params <- do.call(cbind, params)

  params_summary <- cbind(line_id = unique(accepted_routepaths$line_id), params)
  params_summary <- params_summary[order(params_summary$line_id),]

  return(params_summary)

}

