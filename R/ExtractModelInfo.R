#' Extract Model Information
#'
#' This function extracts the performance information and best predictions from a model list
#' @param model.list List of models trained using the TrainLEs function
#' @keywords model, predictions, train
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
