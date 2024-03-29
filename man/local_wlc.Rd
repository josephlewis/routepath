% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/local_wlc.R
\name{local_wlc}
\alias{local_wlc}
\title{rescale conductanceMatrix based on global range of values}
\usage{
local_wlc(x, global_weight, FUN = NULL)
}
\arguments{
\item{x}{\code{conductanceMatrix}}

\item{global_weight}{\code{numeric} Importance of factor. Supplied value must be between 0 and 1}

\item{FUN}{\code{function} function applied to standardised conductanceMatrix values}
}
\value{
\code{conductanceMatrix}
}
\description{
rescale conductanceMatrix based on global range of values
}
\author{
Joseph Lewis
}
