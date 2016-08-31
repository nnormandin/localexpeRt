#' Plot Local Expert summary function
#'
#' This function allows a user to quickly visualize the performance of a tuning /
#' method configuration for a local expert list by plotting the average
#' accuracy and kappa values from the resamples of local experts, in addition
#' to the standard deviation of these metrics.
#'
#'
#' @param model.info A model information object created using the
#' \code{\link{ExtractModelInfo}} function
#' @export
#' @examples PlotLEs(model.info.object)
#'


PlotLEs <- function(model.info){

  # save and reset default graphical parameters
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  # change plotting window to 2x2
  par(mfrow = c(2,2))

  # first plot
  par(mar = c(0, 4.1, 2.1, 1.1))
  plot(model.info$performance$accuracy, xaxt = 'n', xlab = '', ylab = 'accuracy', pch = 16)

  # second plot
  par(mar = c(0, 3.8, 2.1, 2.1))
  plot(model.info$performance$kappa, xaxt = 'n', xlab = '', ylab = 'kappa', pch = 17)

  # third plot
  par(mar = c(4, 4.1, 2.1, 1.1))
  plot(model.info$performance$accuracySD, ylab = 'accuracy stdev', xlab = 'Local Experts', pch = 16)

  # fourth plot
  par(mar = c(4, 3.8, 2.1, 2.1))
  plot(model.info$performance$kappaSD, ylab = 'kappa stdev', xlab = 'Local Experts', pch = 17)

  # summarize output
  cat(paste("The maximum accuracy is",
            format(max(model.info$performance$accuracy), digits = 3), "\n"))
  cat(paste("The average accuracy is",
            format(mean(model.info$performance$accuracy), digits = 3), "\n"))
  cat(paste("The minimum kappa is",
            format(min(model.info$performance$kappa), digits = 3), "\n"))
  cat(paste("The average kappa is",
            format(mean(model.info$performance$kappa), digits = 3), "\n\n"))
}
