#' Fit a CDF to a row of local expert predictions
#'
#' Fits a curve to LE predictions, outputs fitted curve point estimates and distribution moments
#' @param row Vector of LE predictions
#' @param y.vals Y-values from BinCols function
#' @param granularity Number of interpolation points in fitting; defaults to 100
#' @param df Degrees of freedom in smoothing spline; defaults to 10
#' @param mode Choose whether probability output is in CDF or PDF format
#' @param plot Plot output; defaults to FALSE
#' @keywords smooth
#' @export
#' @examples
#'


FitInstance <- function(row, y.values, granularity = 100, df = 10, mode = 'CDF', plot = FALSE) {

  # first fit a spline to the predictions to make a smoothed ECDF
  smooth <- smooth.spline(y = row, x = y.values, df = df)

  # sample at points from the smallest to largest y value, n equal to granularity
  sample.points <- seq(from = min(y.values), to = max(y.values), length.out = granularity)

  # function that returns an estimated y-value at each sample point x-value
  FitPredict <- function(x){
    return(predict(smooth, x)$y)}

  # ecdf is a vector of smoothed sample points in original predictions ECDF
  ecdf <- sapply(sample.points, FitPredict)

  # epdf is the diff of the ecdf
  epdf <- diff(ecdf)

  # compute average using epdf
  avg <- (sum(epdf*sample.points[1:length(sample.points)-1])
          + sum(epdf*sample.points[2:length(sample.points)]))/2

  # compute variance using epdf
  ## TODO: var function gives negative values somehow- need fix
  variance <- (sum((((sample.points[1:length(sample.points)-1] - avg)^2) * epdf)) +
                 sum((((sample.points[2:length(sample.points)] - avg)^2) * epdf))) / 2

  # compute skew and kurtosis of epdf
  skew <- skewness(epdf)
  kurt <- kurtosis(epdf)


  if (mode == 'CDF') {
    out <- list(ecdf, sample.points, avg, variance, skew, kurt)
    names(out) <- c("CDF", "SamplePoints", "mean", "var", "skew", "kurtosis")
    if (plot == TRUE) {
      plot(x = out$SamplePoints, y = out$CDF, type = 'l', xlab = 'target variable',
           ylab = 'probability')
    }
    return(out)
  } else {
    out <- list(epdf, sample.points, avg, variance, skew, kurt)
    names(out) <- c("PDF", "SamplePoints", "mean", "var", "skew", "kurtosis")
    if (plot == TRUE) {
      plot(x = out$SamplePoints[2:length(out$SamplePoints)], y = out$PDF, type = 'l',
           xlab = 'target variable', ylab = 'probability')
    }
    return(out)
  }
}
