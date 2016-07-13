#' Extract best model predictions
#'
#' This function transforms a continuous target vector into a series of binary target vectors
#' @param model The column to be binarized
#' @param n The number of columns in the resulting binary matrix
#' @param mode Defaults to 'EW' intervals, but 'EP' is also available
#' @keywords binary
#' @export
#' @examples
#' y <- runif(1000, 0, 200)
#' BinCols(y, n = 50, mode = 'EP')
#'

# function to pull prediction probs,
ExtractBestPreds <- function(model){
  best.pred.key <- data.table(model$bestTune, key = names(model$bestTune)) # save this
  all.preds <- data.table(model$pred, key = names(model$bestTune))
  best.pred <- all.preds[best.pred.key, ] # save this, and reorder?
  setorderv(best.pred, c("rowIndex", "Resample"))
  return(best.pred)}

ExtractDownProbs <- function(model){
  down.probs <- model$DOWN
  return(down.probs)}

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
