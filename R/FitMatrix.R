#' Fit a matrix of LE predictions
#'
#' Fit a curve and calculate moments for all rows in a matrix of LE predictions
#' @param preds.matrix Matrix of LE predictions from ExtractModelInfo function
#' @param y.vals Y-values from BinCols function
#' @param sample.points Whether or not to include sample points in output; defaults to false
#' @param ... Additional parameters to be passed to fitting function
#' @keywords smooth
#' @export
#' @examples
#'

FitMatrix <- function(preds.matrix, y.values, sample.points = FALSE, ...){
  meta.rows <- apply(preds.matrix, 1, FitInstance, y.values = y.values)
  meta.rows <- as.data.frame(do.call(rbind, meta.rows))
  dist.rows <- do.call(rbind, meta.rows[,1])
  out <- as.data.frame(c(dist.rows, meta.rows[,1:length(meta.rows)]))

  samp.rows <- do.call(rbind, meta.rows[,2])
  return(meta.rows)
}
