#' Predict response of new instance using local expert ensemble with stacking
#'
#' Fits a curve to LE predictions, outputs fitted curve point estimates and distribution moments
#' @param x Attribute vector of new instance to be predicted
#' @param LE.model.list Model list object from TrainLEs function
#' @param stack.model Regression model trained on LE output
#' @param y.vals Y-values from BinCols function
#' @param mode Choose whether probability output is in CDF or PDF format
#' @param mean Whether to include mean in stacked model
#' @param var Whether to include variance in stacked model
#' @param skewness Whether to include skewness in stacked model
#' @param kurtosis Whether to include kurtosis in stacked model
#' @param plot Display plot of LE predictions and stack estimate
#' @param verbose Determines whether prediction information is printed
#' @param ... Additional arguments to pass to the FitInstance function
#' @keywords predict
#' @export
#' @examples
#'

PredictNew <- function(x, LE.model.list, stack.model, y.vals, mode = 'CDF', mean = FALSE,
                       var = FALSE, skewness = FALSE, kurtosis = FALSE,
                       plot = FALSE, verbose = FALSE, ...){

  # 1) pass x vector through all LEs to get prediction

  LE.preds <- PredictLEs(x, LE.model.list)

  # 2) input LE prediction vector into FitInstance function

  instance.fit <- FitInstance(row = LE.preds, y.values = y.vals, mode = mode, ...)

## TODO: test this

  meta.names <- c('mean', 'var', 'skewness', 'kurtosis')
  for(i in 1:length(meta.names)){
    if(meta.names[i] %in% names(stack.model$finalModel$model)){
      LE.preds <- c(LE.preds, instance.fit[meta.names[i]])
    }
  }


  # 3) cbind meta features depending on user parameter choices

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
  LE.preds <- t(LE.preds)
  stack.pred <- predict(stack.model, newdata = LE.preds)

  # 5) output point prediction, LE predictions, var
  return(stack.pred)
  # 6) output plot w/ lines at distribution mean and stacked prediction
}

