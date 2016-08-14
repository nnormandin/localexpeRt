#' Predict response of new instance using local expert ensemble with stacking
#'
#' Fits a curve to LE predictions, outputs fitted curve point estimates and distribution moments
#' @param x Attribute vector of new instance to be predicted
#' @param LE.model.list Model list object from TrainLEs function
#' @param stack.model Regression model trained on LE output
#' @param y.vals Y-values from BinCols function
#' @param mode Choose whether probability output is in CDF or PDF format
#' @param plot Display plot of LE predictions and stack estimate
#' @param verbose Determines whether prediction information is printed
#' @param ... Additional arguments to pass to the FitInstance function
#' @keywords predict
#' @export
#' @examples
#'

PredictNew <- function(x, LE.model.list, stack.model, y.vals, mode = 'CDF',
                       plot = FALSE, verbose = FALSE, ...){

  # 1) pass x vector through all LEs to get prediction

  LE.preds <- PredictLEs(x, LE.model.list)

  # 2) input LE prediction vector into FitInstance function

  instance.fit <- FitInstance(row = LE.preds, y.values = y.vals, mode = mode, ...)

  # 3) attach meta features if they are detected in the original model

  meta.names <- c('mean', 'var', 'skewness', 'kurtosis')
  for(i in 1:length(meta.names)){
    if(meta.names[i] %in% names(stack.model$trainingData)){
      x1 <- c(LE.preds, instance.fit[meta.names[i]])
      print(paste('Attaching meta-feature:', meta.names[i]))
    }
  }

  ## no longer necessary:
  # if(mean == TRUE){
  #   LE.preds <- c(LE.preds, instance.fit$mean)
  # }
  #
  # if(var == TRUE){
  #   LE.preds <- c(LE.preds, instance.fit$var)
  # }
  #
  # if(skewness == TRUE){
  #   LE.preds <- c(LE.preds, instance.fit$skew)
  # }
  #
  # if(kurtosis == TRUE){
  #   LE.preds <- c(LE.preds, instance.fit$kurtosis)
  # }


  # 4) transpose and pass new x vector into L1 regression model
  x1 <- t(x1)
  stack.pred <- predict(stack.model, newdata = x1)

  if(plot == TRUE){
    plot(x = y.vals, y = LE.preds, type = 'l')
    }

  # 5) output point prediction, LE predictions, var
  return(list(stack.pred, y.vals, LE.preds))
  # 6) output plot w/ lines at distribution mean and stacked prediction


}

