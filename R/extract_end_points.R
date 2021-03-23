extract_end_points <- function(lines) {

  end_points <- foreach::foreach(i = 1:length(lines), .combine = rbind) %do%
    {
      sp::SpatialPoints(sp::coordinates(as(lines[i,], "SpatialPoints"))[c(1,nrow(sp::coordinates(as(lines[i,], "SpatialPoints")))),])
    }

  return(end_points)
}
