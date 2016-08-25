#' Extract best model predictions
#'
#' Extracts best predictions from each resample from cv data
#' @param model The model to extract predictions from
#' @keywords model
#' @export
#'

ExtractBestPreds <- function(model){
  best.pred.key <- data.table::data.table(model$bestTune, key = names(model$bestTune)) # save this
  all.preds <- data.table::data.table(model$pred, key = names(model$bestTune))
  best.pred <- all.preds[best.pred.key, ] # save this, and reorder?
  data.table::setorderv(best.pred, c("rowIndex", "Resample"))
  return(best.pred)}

#' Extract probabilities from class estimation predictions
#'
#' Extracts the probability that instances will be less than the target value
#' @param model The model to extract probabilities from
#' @keywords model
#' @export

ExtractDownProbs <- function(model){
  down.probs <- model$DOWN
  return(down.probs)}

#' Extract model performance
#'
#' Extracts all performance data from best model
#' @param model The model to extract performance from
#' @keywords model
#' @export

ExtractPerformance <- function(model){
  best.pred.key <- data.table::data.table(model$bestTune, key = names(model$bestTune))
  results.table <- data.table::data.table(model$results, key = names(model$bestTune))
  results.table <- results.table[best.pred.key, ] # save this
  return(results.table)}

#' Extract model kappa
#'
#' Extracts kappa from best model performance
#' @param model The model to extract kappa from
#' @keywords model
#' @export

ExtractKappa <- function(model){
  Kappa <- model$Kappa
  return(Kappa)}

#' Extract model accuracy
#'
#' Extracts accuracy from best model performance
#' @param model The model to extract accuracy from
#' @keywords model
#'@export

ExtractAccuracy <- function(model){
  Accuracy <- model$Accuracy
  return(Accuracy)}

#' Extract model accuracy standard deviation
#'
#' Extracts accuracy standard deviation of resamples from best model performance
#' @param model The model to extract accuracy SD from
#' @keywords model
#' @export

ExtractAccuracySD <- function(model){
  AccuracySD <- model$AccuracySD
  return(AccuracySD)}

#' Extract model kappa standard deviation
#'
#' Extracts kappa standard deviation of resamples from best model performance
#' @param model The model to extract kappa SD from
#' @keywords model
#' @export

ExtractKappaSD <- function(model){
  KappaSD <- model$KappaSD
  return(KappaSD)}

#' Calculate HDI
#'
#' Takes vector of pdf sample points and corresponding target variable values to
#' calculate the HDI at desired confidence level
#' @param sample.points Vector of target variable values corresponding to pdf
#' @param epdf Vector of epdf sample point values
#' @param cred Confidence level
#' @keywords model
#' @export

CalcHDI <- function(sample.points, epdf, cred = 0.90){

  # check lengths
  if(length(sample.points) != length(epdf)){
    cat("Sample point and PDF vectors are of unequal lengths\n")

    # if length is off by one, do interpolation automatically
    if(length(sample.points)-1 == length(epdf)){
      cat("Calculating sample points interpolation vector...\n\n")
      sample.points <- (sample.points[1:length(sample.points)-1]
                        + sample.points[2:length(sample.points)])/2
    }
  }

  prob.mass <- epdf/sum(epdf)
  sorted.mass <- sort(prob.mass, decreasing = TRUE)
  HDI.height.id <- min(which(cumsum(sorted.mass) >= cred))
  HDI.height <- sorted.mass[HDI.height.id]
  HDI.mass <- sum(prob.mass[prob.mass >= HDI.height])
  HDI.ids <- which(prob.mass >= HDI.height)
  x0 <- sample.points[min(HDI.ids)]
  x1 <- sample.points[max(HDI.ids)]
  out <- list(interval = c(x0, x1))
  return(out)
}

#' Check model.list object
#'
#' Ensures model.list object contains all `train` objects
#' @param model.list
#' @keywords model
#' @export

CheckModelList <- function(model.list){

  if(is.list(model.list) != TRUE){
    stop('The model.list object is not a list')
  }

  n.train <- sum((sapply(model.list, function(x)
    class(x) == 'train')))
  if(n.train != length(model.list)){
    stop('The model.list object contains objects that are not models')
  }
  }

  prob.mass <- epdf/sum(epdf)
  sorted.mass <- sort(prob.mass, decreasing = TRUE)
  HDI.height.id <- min(which(cumsum(sorted.mass) >= cred))
  HDI.height <- sorted.mass[HDI.height.id]
  HDI.mass <- sum(prob.mass[prob.mass >= HDI.height])
  HDI.ids <- which(prob.mass >= HDI.height)
  x0 <- sample.points[min(HDI.ids)]
  x1 <- sample.points[max(HDI.ids)]
  out <- list(interval = c(x0, x1))
  return(out)
}
# function to calculate variable importance- TODO: validate VARIMP capability of base learner
# ExtractVarImp <- function(model){
#  importance <- varImp(model)$importance$Overall
#  return(importance)}
