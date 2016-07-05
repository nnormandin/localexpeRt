#' Binary columns function
#'
#' This function transforms a continuous target vector into a series of binary target vectors
#' @param x The column to be binarized
#' @param n The number of columns in the resulting binary matrix
#' @param mode Defaults to 'EW' intervals, but 'EP' is also available
#' @keywords binary
#' @export
#' @examples
#' y <- runif(1000, 0, 200)
#' BinCols(y, n = 50, mode = 'EP')
#'

BinCols <- function(x, n = 30, mode = 'EW'){

  if(mode == 'EP'){
    increment = (1/ (n +1))
    prob.values <- seq(from = increment, to = 1-increment, by = increment)
    y.values <- quantile(x, probs = prob.values)
    binary.cols <- as.data.frame(lapply(y.values, function(i) ifelse(x >= i, "UP", "DOWN")))
    output <- list(prob.values, y.values, binary.cols)
    names(output) <- c("prob.values", "y.values", "columns")}

  if(mode == 'EW'){
    y.values <- seq(from = quantile(x, probs = 0.02),
                    to = quantile(x, probs = 0.98), length.out = n)
    binary.cols <- as.data.frame(lapply(y.values, function(i) ifelse(x >= i, "UP", "DOWN")))
    output <- list(y.values, binary.cols)
    names(output) <- c("y.values", "columns")}

  return(output)}
