#' Extract best model predictions
#'
#' Extracts best predictions from each resample from cv data
#' @param model The model to extract predictions from
#' @keywords model
#'

ExtractBestPreds <- function(model){
  best.pred.key <- data.table(model$bestTune, key = names(model$bestTune)) # save this
  all.preds <- data.table(model$pred, key = names(model$bestTune))
  best.pred <- all.preds[best.pred.key, ] # save this, and reorder?
  setorderv(best.pred, c("rowIndex", "Resample"))
  return(best.pred)}

#' Extract probabilities from class estimation predictions
#'
#' Extracts the probability that instances will be less than the target value
#' @param model The model to extract probabilities from
#' @keywords model
#'

ExtractDownProbs <- function(model){
  down.probs <- model$DOWN
  return(down.probs)}

#' Extract model performance
#'
#' Extracts all performance data from best model
#' @param model The model to extract performance from
#' @keywords model
#'

ExtractPerformance <- function(model){
  best.pred.key <- data.table(model$bestTune, key = names(model$bestTune))
  results.table <- data.table(model$results, key = names(model$bestTune))
  results.table <- results.table[best.pred.key, ] # save this
  return(results.table)}

#' Extract model kappa
#'
#' Extracts kappa from best model performance
#' @param model The model to extract kappa from
#' @keywords model
#'

ExtractKappa <- function(model){
  Kappa <- model$Kappa
  return(Kappa)}

#' Extract model accuracy
#'
#' Extracts accuracy from best model performance
#' @param model The model to extract accuracy from
#' @keywords model
#'

ExtractAccuracy <- function(model){
  Accuracy <- model$Accuracy
  return(Accuracy)}

#' Extract model accuracy standard deviation
#'
#' Extracts accuracy standard deviation of resamples from best model performance
#' @param model The model to extract accuracy SD from
#' @keywords model
#'

ExtractAccuracySD <- function(model){
  AccuracySD <- model$AccuracySD
  return(AccuracySD)}

#' Extract model kappa standard deviation
#'
#' Extracts kappa standard deviation of resamples from best model performance
#' @param model The model to extract kappa SD from
#' @keywords model
#'

ExtractKappaSD <- function(model){
  KappaSD <- model$KappaSD
  return(KappaSD)}



# function to calculate variable importance- TODO: validate VARIMP capability of base learner
# ExtractVarImp <- function(model){
#  importance <- varImp(model)$importance$Overall
#  return(importance)}
