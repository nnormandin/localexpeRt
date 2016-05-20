#' Binary columns function
#'
#' This function transforms a continuous target vector into a series of binary target vectors
#' @param x The column to be binarized
#' @param increment The increment for Equally Probable intervals, defaults to 0.01
#' @keywords binary
#' @export
#' @examples
#' BinCols(trainingY, increment = 0.02)
#'


BinCols <- function(x, increment = 0.01){
  prob.values <- seq(from = increment, to = 1-increment, by = increment)
  y.values <- quantile(x, probs = prob.values)
  binary.cols <- as.data.frame(lapply(y.values, function(n) ifelse(x >= n, "UP", "DOWN")))
  output <- list(prob.values, y.values, binary.cols)
  names(output) <- c("prob.values", "y.values", "columns")
  return(output)}


# test test test
