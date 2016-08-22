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

    # save current graph parameters
    suppressWarnings(opar <- par())

    # change to double plot
    par(mfrow = c (2,1))

    # calculate midpoints in samples for PDF
    sample.interp <- (instance.fit$sample.points[1:length(instance.fit$sample.points)-1]
                      + instance.fit$sample.points[2:length(instance.fit$sample.points)])/2

    # plot pdf with line at y.hat and label
    plot(x = sample.interp, y = instance.fit$epdf, type = 'l')
    abline(v = stack.pred)
    text(y = max(instance.fit$epdf)/10, x = stack.pred, pos = 2,
         labels = paste0('y.hat = ', format(stack.pred, digits = 4)))

    # plot LE predictions with smooth ECDF overlay
    plot(x = instance.fit$sample.points, y = instance.fit$ecdf, type = 'l')
    lines(x = y.vals, y = LE.preds, type = 'p')

    # restore original parameters
    suppressWarnings(par(opar))
    }

  # 5) output point prediction, LE predictions, var
  return(list(y.hat = stack.pred, y.vals = y.vals, LE.preds = LE.preds))

}

