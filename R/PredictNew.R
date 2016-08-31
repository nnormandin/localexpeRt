#' Predict response of new instance using local expert ensemble with stacking
#'
#' Fits a curve to LE predictions, outputs fitted curve point estimates and distribution moments
#' @param x Attribute vector of new instance to be predicted
#' @param model.list Model list object from TrainLEs function
#' @param stack.model Regression model trained on LE output
#' @param y.vals Y-values from BinCols function
#' @param plot Display plot of LE predictions and stack estimate
#' @param verbose Determines whether prediction information is printed
#' @param df Degrees of freedom to pass spline if plotting
#' @param cred Credibility interval for HDI in plot
#' @param ... Additional arguments to pass to the FitInstance function
#' @keywords predict
#' @export
#' @examples
#'

PredictNew <- function(x, model.list, stack.model, y.vals,
                       plot = FALSE, verbose = FALSE, df = 10, cred, ...){

  # make sure the nubmer of models and the number of y.vals are correct
  CheckLengths(model.list, y.vals)

  # 1) pass x vector through all LEs to get prediction

  LE.preds <- PredictLEs(x, model.list)

  # 2) input LE prediction vector into FitInstance function

  instance.fit <- FitInstance(LE.preds = LE.preds, y.values = y.vals, ...)

  if(verbose == TRUE){
    cat(paste0('spline fit to LE predictions with ', df, ' degrees of freedom\n'))
    cat(paste0('\n...mean: ', format(instance.fit$mean, digits = 3),
               '\n...var: ', format(instance.fit$var, digits = 3),
               '\n...skewness: ', format(instance.fit$skew, digits = 3),
               '\n...kurtosis: ', format(instance.fit$kurtosis, digits = 3), "\n"))
  }

  # 3) attach meta features if they are detected in the original model

  meta.names <- c('mean', 'var', 'skewness', 'kurtosis')
  for(i in 1:length(meta.names)){
    if(meta.names[i] %in% names(stack.model$trainingData)){
      x1 <- c(LE.preds, instance.fit[meta.names[i]])
      cat(paste('\nAttaching meta-feature:', meta.names[i], "\n"))
    }
  }

  # 4) transpose and pass new x vector into L1 regression model
  x1 <- t(x1)
  stack.pred <- predict(stack.model, newdata = x1)

  if(plot == TRUE){

    # save current graph parameters
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))

    # change to double plot
    par(mfrow = c (2,1))

    # calculate midpoints in samples for PDF
    sample.interp <- (instance.fit$sample.points[1:length(instance.fit$sample.points)-1]
                      + instance.fit$sample.points[2:length(instance.fit$sample.points)])/2

    # plot pdf with line at y.hat and label
    dig <- 3
    par(mar = c(0, 2.1, 4.1, 2.1))
    plot(x = sample.interp, y = instance.fit$epdf, type = 'l', main='',
         xlab = '', yaxt = 'n',ylab = '', xaxt = 'n')
    abline(v = stack.pred, lty = 2)
    abline(v = instance.fit$mean, lty = 3)
    #text(y = max(instance.fit$epdf)/10, x = stack.pred, pos = 2, cex = 0.8,
    #     labels = paste0('y.hat = ', format(stack.pred, digits = dig)))

    # generate HDI
    prob.mass <- instance.fit$epdf/sum(instance.fit$epdf)
    sorted.mass <- sort(prob.mass, decreasing = TRUE)
    HDI.height.id <- min(which(cumsum(sorted.mass) >= cred))
    HDI.height <- sorted.mass[HDI.height.id]
    HDI.mass <- sum(prob.mass[prob.mass >= HDI.height])
    HDI.ids <- which(prob.mass >= HDI.height)
    x0 <- sample.interp[min(HDI.ids)]
    x1 <- sample.interp[max(HDI.ids)]
    cat(paste0("\n\nEstimate of y value is ", format(stack.pred, digits = dig),
                  " with a ", (cred*100), "% HDI in the interval (",
                 format(x0, digits = dig), ", ", format(x1, digits = dig), ")"  ))

    # make shaded polygon over HDI
    seg.height <- max(instance.fit$epdf)
    poly.x <- c(x0, x1, x1, x0)
    poly.y <- c(seg.height, seg.height, 0, 0)
    polygon(x = poly.x, y = poly.y,
            col = rgb(0, .2, .2, .2), border = NA)

    #segments(x0 = x0, y0 = seg.height,
    #        x1 = x1, y1 = seg.height)


    # plot LE predictions with smooth ECDF overlay
    par(mar = c(4.1, 2.1, 0, 2.1))
    plot(x = instance.fit$sample.points, y = instance.fit$ecdf, type = 'l',
         main='', xlab = 'response', ylab = '', yaxt = 'n')
    lines(x = y.vals, y = LE.preds, type = 'p')
    poly.y2 <- c(1, 1, 0, 0)
    polygon(x = poly.x, y = poly.y2,
            col = rgb(0, .2, .2, .2), border = NA)
    abline(v = stack.pred, lty = 2)
    abline(v = instance.fit$mean, lty = 3)


    }

  # 5) output point prediction, LE predictions, var
  return(list(y.hat = stack.pred, y.vals = y.vals, LE.preds = LE.preds))

}

