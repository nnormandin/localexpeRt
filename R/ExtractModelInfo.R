#' Extract Model Information
#'
#' This function trains a list of local expert (level 0) models on a binary response matrix.
#' @param x The feature matrix
#' @param bincols Binary target matrix created with 'BinCols()' function
#' @param trControl Optional argument to specify a train control object- defaults to cross validated
#' @param n.repeats Number of repeats if default train control object is used- defaults to 10
#' @param method Type of learning algorithm used for induction- defaults to linear model
#' @param ... Additional parameters to pass to model training function
#' @keywords train
#' @export
#' @examples
#'

ExtractModelInfo <- function(model.list){

  # take raw performance data and make it a data frame
  performance <- lapply(model.list, ExtractPerformance)
  accuracy <- sapply(performance, ExtractAccuracy)
  accuracySD <- sapply(performance, ExtractAccuracySD)
  kappa <- sapply(performance, ExtractKappa)
  kappaSD <- sapply(performance, ExtractKappaSD)
  performance <- data.frame(accuracy, accuracySD, kappa, kappaSD)

  # take prediction data and refine to matrix of DOWN probabilities
  best.preds <- lapply(model.list, ExtractBestPreds)
  preds.matrix <- sapply(best.preds, ExtractDownProbs)

  ## TODO: make var.imp usable
  #var.imp <- lapply(model.list, ExtractVarImp)

  ## TODO: append the numeric "y" column to predictions, sort predictions by rowIndex
  out <- list("preds.matrix" = preds.matrix, "performance" = performance)
  return(out)
}
