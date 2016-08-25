#' Predict response of new instance using local expert ensemble with stacking
#'
#' Fits a curve to LE predictions, outputs fitted curve point estimates and distribution moments
#' @param x Attribute vector of new instance to be predicted
#' @param LE.model.list Model list object from TrainLEs function
#' @keywords predict
#' @export
#' @examples
#'


PredictLEs <- function(x, LE.model.list){

  # error if model.list is not correct
  CheckModelList(LE.model.list)

  # define function to predict probability and output DOWN prob
  PredictProb <- function(x, model){
    pred <- predict(model, newdata = x, type = 'prob')
    out <- pred$DOWN
    return(out)
  }

  # pass LE.model.list through sapply() to get all probabilities in vector format
  LE.preds <- sapply(LE.model.list, PredictProb, x = x)

  return(LE.preds)
}
