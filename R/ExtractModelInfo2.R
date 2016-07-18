#' Extract Model Information2
#'
#' This function extracts the performance information and best predictions from a model list
#' @param model.list List of models trained using the TrainLEs function
#' @keywords model, predictions, train
#' @export
#' @examples
#'

ExtractModelInfo2 <- function(model.list){

  # function to pull prediction probs,
  ExtractBestPreds <- function(model){
    best.pred.key <- data.table(model$bestTune, key = names(model$bestTune)) # save this
    all.preds <- data.table(model$pred, key = names(model$bestTune))
    best.pred <- all.preds[best.pred.key, ] # save this, and reorder?
    setorderv(best.pred, c("rowIndex", "Resample"))
    return(best.pred)}

  ExtractDownProbs <- function(model){
    down.probs <- model$DOWN
    return(down.probs)
  }

  # function to pull model performance
  ExtractPerformance <- function(model){
    best.pred.key <- data.table(model$bestTune, key = names(model$bestTune))
    results.table <- data.table(model$results, key = names(model$bestTune))
    results.table <- results.table[best.pred.key, ] # save this
    return(results.table)}

  # get all Kappas
  ExtractKappa <- function(model){
    Kappa <- model$Kappa
    return(Kappa)}

  ExtractAccuracy <- function(model){
    Accuracy <- model$Accuracy
    return(Accuracy)}

  ExtractAccuracySD <- function(model){
    AccuracySD <- model$AccuracySD
    return(AccuracySD)}

  ExtractKappaSD <- function(model){
    KappaSD <- model$KappaSD
    return(KappaSD)}

  # function to calculate variable importance
  ExtractVarImp <- function(model){
    importance <- varImp(model)$importance$Overall
    return(importance)}

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
