% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distances.R
\name{hausdorff_distance}
\alias{hausdorff_distance}
\title{Calculate Hausdorff distance between simulated route path and known route}
\usage{
hausdorff_distance(route, known_route)
}
\arguments{
\item{route}{a SpatialLines object of the simulated route}

\item{known_route}{a SpatialLines object of the known route to be used when comparing against the simulated route paths}
}
\value{
Hausdorff distance between simulated route path and known route
}
\description{
Calculate Hausdorff distance between simulated route path and known route
}
\author{
Joseph Lewis
}
\keyword{internal}
