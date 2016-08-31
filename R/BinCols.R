#' Binary columns function
#'
#' This function takes a continuous target vector (ie. the `y` vector in regression)
#' and generates a matrix of binary target vectors.
#'
#' The options for \code{mode} are for equally probable bin sizes ('EP') and
#' equal width bins ('EW') in terms of the values of the target variable. The
#' parameter \code{n} corresponds to the number of models that will be in the final
#' local expert ensemble.
#'
#' @param y The column to be binarized
#' @param n The number of columns in the resulting binary matrix
#' @param mode Defaults to 'EW' intervals, but 'EP' is also available
#' @param EW.buffer Defaults to 0.01, only for EW testing; excludes outliers from width
#' @return A matrix of width equal to \code{n} and height equal to \code{length(y)}
#' @keywords binary
#' @export
#' @examples
#' y <- runif(1000, 0, 200)
#' binary.matrix <- BinCols(y, n = 15, mode = 'EP')
#'


BinCols <- function(y, n = 30,
                    mode = 'EW',
                    EW.buffer = 0.01){

  # binarize columns with equal quantiles
  if(mode == 'EP'){

    # set quantile values from 0 to 1; save quantiles and corresponding y-value
    increment = (1/ (n +1))
    prob.values <- seq(from = increment, to = 1-increment, by = increment)
    y.values <- quantile(y, probs = prob.values)

    # apply indicator function, generating binary column for each quantile
    binary.cols <- as.data.frame(lapply(y.values, function(i) ifelse(y >= i, "UP", "DOWN")))

    # rename columns
    bin.names <-paste("c", seq(1, n), sep = '')
    names(binary.cols) <- bin.names

    # output quantiles, y-values, and list of columns
    output <- list(prob.values, y.values, binary.cols)
    names(output) <- c("q.vals", "y.vals", "cols")}

  # binarize columns with equal distance in response variable
  if(mode == 'EW'){

    # save y.values
    a <- quantile(y, probs = EW.buffer)
    b <- quantile(y, probs = 1-EW.buffer)
    y.values <- seq(from = a,
                    to = b, length.out = n)

    # apply indicator function, generating binary column for each quantile
    binary.cols <- as.data.frame(lapply(y.values, function(i) ifelse(y >= i, "UP", "DOWN")))

    # rename columns
    bin.names <-paste("c", seq(1, n), sep = '')
    names(binary.cols) <- bin.names

    # output y-values, columns
    output <- list(y.values, binary.cols)
    names(output) <- c("y.vals", "cols")}

  return(output)}
