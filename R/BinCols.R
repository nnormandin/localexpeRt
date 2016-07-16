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


BinCols <- function(x, n = 30, mode = 'EW', EW.buffer = 0.02){

  # binarize columns with equal quantiles
  if(mode == 'EP'){

    # set quantile values from 0 to 1; save quantiles and corresponding y-value
    increment = (1/ (n +1))
    prob.values <- seq(from = increment, to = 1-increment, by = increment)
    y.values <- quantile(x, probs = prob.values)

    # apply indicator function, generating binary column for each quantile
    binary.cols <- as.data.frame(lapply(y.values, function(i) ifelse(x >= i, "UP", "DOWN")))

    # output quantiles, y-values, and list of columns
    output <- list(prob.values, y.values, binary.cols)
    names(output) <- c("q.vals", "y.vals", "cols")}

  # binarize columns with equal distance in response variable
  if(mode == 'EW'){

    # save
    y.values <- seq(from = 0),
                    to = 1, length.out = n + 2)
    y.values <- y.values[2:length(y.values - 1)]
    binary.cols <- as.data.frame(lapply(y.values, function(i) ifelse(x >= i, "UP", "DOWN")))
    output <- list(y.values, binary.cols)
    names(output) <- c("y.values", "columns")}

  return(output)}
