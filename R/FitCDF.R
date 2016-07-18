#' Fit a CDF to a row of local expert predictions
#'
#' Fits a curve to LE predictions, outputs fitted curve point estimates and distribution moments
#' @param row
#' @param y.vals
#' @param granularity
#' @param df
#' @param
#' @keywords plot
#' @export
#' @examples
#'


FitCDF <- function(row, y.values, granularity = 100, df = 10, mode = 'CDF') {

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
  variance <- (sum((((sample.points[1:length(sample.points)-1] - avg)^2) * epdf)) +
                 sum((((sample.points[2:length(sample.points)] - avg)^2) * epdf))) / 2

  # compute skew and kurtosis of epdf
  skew <- skewness(epdf)
  kurt <- kurtosis(epdf)

  ## TODO: fix this with a proper ELIF statement
  # included epdf in output as a test
  if (mode == 'CDF') {
    out <- list(ecdf, sample.points, avg, variance, skew, kurt)
    names(out) <- c("CDF", "SamplePoints", "mean", "var", "skew", "kurtosis")
    return(out)
  } else {
    out <- list(epdf, sample.points, avg, variance, skew, kurt)
    names(out) <- c("PDF", "SamplePoints", "mean", "var", "skew", "kurtosis")
    return(out)
  }
}
