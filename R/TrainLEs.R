#' Train Local Experts
#'
#' This function trains a list of local expert (level 0) models on a binary response matrix.
#' @param x The feature matrix
#' @param bincols Binary target matrix created with 'BinCols()' function
#' @param trControl Optional argument to specify a train control object- defaults to cross validated
#' @param n.folds Number of folds if default train control object is used- defaults to 5
#' @param method Type of learning algorithm used for induction- defaults to lda
#' @param metric Optimization metric; can be either "Accuracy" or "Kappa"
#' @param JIT Whether or not just-in-time compilation is enabled
#' @param ... Additional parameters to pass to model training function
#' @keywords train
#' @export
#' @examples
#'

TrainLEs <- function(x, bincols, trControl = NULL,
                     method = "lda",
                     n.folds = 5,
                     metric = "Kappa",
                     JIT = FALSE,
                     ...){

  # enable just-in-time compilation
  if(JIT == TRUE){
    compiler::enableJIT(3)
    on.exit(compiler::enableJIT(0))
  }

  # default train control function if one is not specified
  if(is.null(trControl)){
    trControl <- caret::trainControl(method = "cv", number = n.folds,
                               returnData = FALSE,
                               savePredictions = TRUE,
                               classProbs = TRUE)}

  # start timer
  t.0 = proc.time()

  # base function to train LEs
  trainer <- function(y){
    set.seed(123)
    mod <- caret::train(x = x, y = y, method = method,
                        metric = metric, trControl = trControl, ...)}

  # apply trainer across all columns
  models <- lapply(bincols, trainer)

  # stop timer, display time
  t.final <- proc.time() - t.0
  print(t.final)

  # output model list
  return(models)
  }
