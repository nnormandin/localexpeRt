% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FitMatrix.R
\name{FitMatrix}
\alias{FitMatrix}
\title{Fit a curve to a matrix of local expert predictions}
\usage{
FitMatrix(preds.matrix, y.values, sample.points = FALSE, ...)
}
\arguments{
\item{preds.matrix}{Matrix of LE predictions from
\code{\link{ExtractModelInfo}} function}

\item{y.values}{Y-values from \code{\link{BinCols}} function}

\item{sample.points}{Whether or not to include sample points in output;
defaults to false}

\item{...}{Additional parameters to be passed to fitting function}
}
\value{
Data frame containing sample points and moments for each instance
}
\description{
Fits a curve and calculates moments for all rows in a matrix of LE predictions.
}

