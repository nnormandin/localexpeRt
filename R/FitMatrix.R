#' Fit a matrix of LE predictions
#'
#' Fit a curve and calculate moments for all rows in a matrix of LE predictions
#' @param preds.matrix Matrix of LE predictions from ExtractModelInfo function
#' @param y.values Y-values from BinCols function
#' @param sample.points Whether or not to include sample points in output; defaults to false
#' @param ... Additional parameters to be passed to fitting function
#' @keywords smooth
#' @export
#' @examples
#'

FitMatrix <- function(preds.matrix, y.values, sample.points = FALSE, ...){

  # apply fitting function row wise to every instance in LE predictions
  meta.rows <- apply(preds.matrix, 1, FitInstance, y.values = y.values, ...)

  # bind the list of output
  meta.rows <- as.data.frame(do.call(rbind, meta.rows))

  # collapse the first column of lists into a data table and rename
  dist.rows <- do.call(rbind, meta.rows[,1])
  cnames <- paste("c", seq(1, ncol(dist.rows)), sep = '')

  # combine distribution estimate with moments
  out <- as.data.frame(cbind(dist.rows, meta.rows$mean, meta.rows$var,
                             meta.rows$skew, meta.rows$kurtosis))
  out <- as.data.frame(do.call(cbind, lapply(out, as.numeric)))


  # rename output
  names(out) <- c(cnames, "mean", "var", "skewness", "kurtosis")

  if(sample.points == FALSE){
    return(out)
  }
  else
  {
    samp.rows <- do.call(rbind, meta.rows[,2])
    names(samp.rows) <- paste("s", seq(1, ncol(samp.rows)), sep = '')
    out <- list(samp.rows, out)
    names(out) <- c("sample.rows", "output")
    return(out)
  }
}
