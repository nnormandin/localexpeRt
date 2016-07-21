#' Predict response of new instance using local expert ensemble with stacking
#'
#' Fits a curve to LE predictions, outputs fitted curve point estimates and distribution moments
#' @param x Attribute vector of new instance to be predicted
#' @param LE.model.list Model list object from TrainLEs function
#' @param stack.model Regression model trained on LE output
#' @param mean Whether to include mean in stacked model
#' @param var Whether to include variance in stacked model
#' @param skewness Whether to include skewness in stacked model
#' @param kurtosis Whether to include kurtosis in stacked model
#' @param plot Display plot of LE predictions and stack estimate
#' @keywords
#' @export
#' @examples
#'

PredictNew <- function(x, LE.model.list, stack.model, mean = FALSE,
                       var = FALSE, skewness = FALSE, kurtosis = FALSE,
                       plot = FALSE, verbose = FALSE){

  # 1) pass x vector through all LEs to get prediction
  # 2) input LE prediction vector into FitInstance function
  # 3) cbind meta features depending on user parameter choices
  # 4) pass new x vector into L1 regression model
  # 5) output point prediction, LE predictions, var
  # 6) output plot w/ lines at distribution mean and stacked prediction
}
