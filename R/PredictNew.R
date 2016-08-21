#' Predict response of new instance using local expert ensemble with stacking
#'
#' Fits a curve to LE predictions, outputs fitted curve point estimates and distribution moments
#' @param x Attribute vector of new instance to be predicted
#' @param LE.model.list Model list object from TrainLEs function
#' @param stack.model Regression model trained on LE output
#' @param y.vals Y-values from BinCols function
#' @param plot Display plot of LE predictions and stack estimate
#' @param verbose Determines whether prediction information is printed
#' @param df Degrees of freedom to pass spline if plotting
#' @param ... Additional arguments to pass to the FitInstance function
#' @keywords predict
#' @export
#' @examples
#'

PredictNew <- function(x, LE.model.list, stack.model, y.vals,
                       plot = FALSE, verbose = FALSE, df = 10, ...){

  # 1) pass x vector through all LEs to get prediction

  LE.preds <- PredictLEs(x, LE.model.list)

  # 2) input LE prediction vector into FitInstance function

  instance.fit <- FitInstance(LE.preds = LE.preds, y.values = y.vals, ...)

  # 3) attach meta features if they are detected in the original model

  meta.names <- c('mean', 'var', 'skewness', 'kurtosis')
  for(i in 1:length(meta.names)){
    if(meta.names[i] %in% names(stack.model$trainingData)){
      x1 <- c(LE.preds, instance.fit[meta.names[i]])
      print(paste('Attaching meta-feature:', meta.names[i]))
    }
  }

  # 4) transpose and pass new x vector into L1 regression model
  x1 <- t(x1)
  stack.pred <- predict(stack.model, newdata = x1)

  if(plot == TRUE){
    plot(x = y.vals, y = LE.preds)
    abline(v = stack.pred)
    smooth <- smooth.spline(y = LE.preds, x = y.vals, df = df)
    lines(smooth)
    text(y = 0.2, x = stack.pred, pos = 4,
         labels = paste0('y.hat = ', format(stack.pred, digits = 4)))
    }

  # 5) output point prediction, LE predictions, var
  return(list(y.hat = stack.pred, y.vals = y.vals, LE.preds = LE.preds))
  # 6) output plot w/ lines at distribution mean and stacked prediction


}

