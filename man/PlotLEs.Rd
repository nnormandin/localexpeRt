% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotLEs.R
\name{PlotLEs}
\alias{PlotLEs}
\title{Plot Local Expert summary function}
\usage{
PlotLEs(model.info)
}
\arguments{
\item{model.info}{A model information object created using the
\code{\link{ExtractModelInfo}} function}
}
\description{
This function allows a user to quickly visualize the performance of a tuning /
method configuration for a local expert list by plotting the average
accuracy and kappa values from the resamples of local experts, in addition
to the standard deviation of these metrics.
}

