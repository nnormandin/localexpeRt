#' Train Local Experts
#'
#' This function trains a list of local expert (level 0) models on a binary response matrix.
#' @param x The feature matrix
#' @param bincols Binary target matrix created with 'BinCols()' function
#' @param trControl Optional argument to specify a train control object- defaults to cross validated
#' @param n.repeats Number of repeats if default train control object is used- defaults to 5
#' @param method Type of learning algorithm used for induction- defaults to lda
#' @param ... Additional parameters to pass to model training function
#' @keywords train
#' @export
#' @examples
#'

TrainLEs <- function(x, bincols, trControl = NULL, method = "lda", n.repeats = 5, ...){

  if(is.null(trControl)){
    trControl <- caret::trainControl(method = "cv", number = n.repeats,
                               returnData = FALSE,
                               savePredictions = TRUE,
                               classProbs = TRUE)}
  t.0 = proc.time()
  trainer <- function(y){
    ## TODO: start parallel and stop at end of each train
    set.seed(123)
    mod <- caret::train(x = x, y = y, method = method, trControl = trControl, ...)}
  models <- lapply(bincols, trainer)
  t.final <- proc.time() - t.0
  print(t.final)

  return(models)}
