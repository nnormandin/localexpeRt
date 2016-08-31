#' Predict response of new instance using local expert ensemble with stacking
#'
#' Generates local expert predictions for a new feature vector \code{x}.
#'
#' @param x Attribute vector of new instance to be predicted
#' @param model.list Model list object from
#' \code{\link{TrainLEs}} function
#' @return Returns a vector of probabilies in the [0,1] interval of length equal
#' to the number of local experts in the model list object.
#' @export
#'


PredictLEs <- function(x, model.list){

  # error if model.list is not correct
  CheckModelList(model.list)

  # define function to predict probability and output DOWN prob
  PredictProb <- function(x, model){
    pred <- predict(model, newdata = x, type = 'prob')
    out <- pred$DOWN
    return(out)
  }

  # pass model.list through sapply() to get all probabilities in vector format
  LE.preds <- sapply(model.list, PredictProb, x = x)

  return(LE.preds)
}
