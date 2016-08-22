#' Plot Local Expert summary function
#'
#' This plots the average accuracy and kappa values from the resamples of local experts, in addition to their standard deviation
#' @param model.info A model information object created using the ExtractModelInfo function
#' @keywords plot
#' @export
#' @examples
#'


PlotLEs <- function(model.info){
  opar <- par()
  par(mfrow = c(2,2))
  plot(model.info$performance$accuracy, xaxt = 'n', xlab = '', ylab = 'accuracy', pch = 16)
  plot(model.info$performance$kappa, xaxt = 'n', xlab = '', ylab = 'kappa', pch = 17)
  plot(model.info$performance$accuracySD, ylab = 'accuracy stdev', xlab = 'Local Experts', pch = 16)
  plot(model.info$performance$kappaSD, ylab = 'kappa stdev', xlab = 'Local Experts', pch = 17)
  print(paste("The maximum accuracy is", format(max(model.info$performance$accuracy), digits = 3)))
  print(paste("The average accuracy is", format(mean(model.info$performance$accuracy), digits = 3)))
  print(paste("The minimum kappa is", format(min(model.info$performance$kappa), digits = 3)))
  print(paste("The average kappa is", format(mean(model.info$performance$kappa), digits = 3)))
  par(mfrow = c(1,1))
  suppressWarnings(par(opar))
}
