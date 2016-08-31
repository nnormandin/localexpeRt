#' Fit a curve to a row of local expert predictions
#'
#' Fits a smoothing spline to local expert predictions, creating a CDF-like
#' distribution. The CDF is differentiated, and moments of the PDF are calculated.
#'
#' @param LE.preds Vector of LE predictions
#' @param y.values Y-values from BinCols function
#' @param granularity Number of interpolation points in fitting; defaults to 100
#' @param df Degrees of freedom in smoothing spline; defaults to 10
#' @param plot Plot output; defaults to FALSE
#' @return Returns a list containing the empirical PDF, empirical CDF, the sample
#' points associated with the distribution shapes, and the moments of the
#' smoothed distribution.
#' @export
#' @examples
#'


FitInstance <- function(LE.preds, y.values, granularity = 1000, df = 10, plot = FALSE) {

  # first fit a spline to the predictions to make a smoothed ECDF
  smooth <- smooth.spline(y = LE.preds, x = y.values, df = df)

  # sample at points from the smallest to largest y value, n equal to granularity
  sample.points <- seq(from = min(y.values), to = max(y.values), length.out = granularity)

  # function that returns an estimated y-value at each sample point x-value
  FitPredict <- function(x){
    return(predict(smooth, x)$y)}

  # ecdf is a vector of smoothed sample points in original predictions ECDF
  ecdf <- sapply(sample.points, FitPredict)
  ecdf[ecdf > 1] <- 1

  # epdf is the diff of the ecdf
  epdf <- diff(ecdf)

  # no negative values
  epdf[epdf<0] <- 0

  # compute mid points in sample.points
  sample.interp <- (sample.points[1:length(sample.points)-1]
                    + sample.points[2:length(sample.points)])/2

  # compute average using epdf and interpolated points
  avg <- sum(sample.interp * epdf)

  # compute variance using epdf and interpolated points
  variance <- sum(((sample.interp - avg)^2) * epdf)

  # compute skew and kurtosis of epdf
  skew <- PerformanceAnalytics::skewness(epdf)
  kurt <- PerformanceAnalytics::kurtosis(epdf)


  out <- list(epdf = epdf, ecdf = ecdf, sample.points = sample.points,
              mean = avg, var = variance, skew = skew, kurtosis = kurt)

  if (plot == TRUE) {

    # save graphical parameters
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))

    plot(x = sample.interp, y = out$epdf, type = 'l',
           xlab = 'target variable', ylab = 'probability')
  }

  return(out)

}
