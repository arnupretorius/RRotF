#' k-fold cross-validiation for Random Rotation Forests.
#'
#' Performs k-fold cross-validation using specified Random Rotation Forest as prediction model.
#' @param x Training data input matrix.
#' @param y Training data response.
#' @param k Number of folds.
#' @param seed Specified seed for reproducible folds.
#' @param verbose Boolean, if true prints progress of k-fold CV runs, if false prints nothing.
#' @param ... Additional arguments specified to \code{RRotF}.
#' @return A list containing the average CV error computed over the folds and the error per fold.
#' @author Arnu Pretorius <arnupretorius@gmail.com>
#' @examples
#' library(ElemStatLearn)
#' library(caret)
#' data("SAheart")
#' trainIndex <- createDataPartition(SAheart$chd, p=0.6, list=FALSE)
#' train <- SAheart[trainIndex,]
#' test <- SAheart[-trainIndex,]
#' Xtrain <- train[,-10]
#' ytrain <- train[,10]
#' CVerror <- kFoldRun(Xtrain, ytrain, k=5)
#' CVerror
#' @export

kFoldRun <- function(x,y,k=10, seed=1, verbose=TRUE, ...){
      # Test preparations
      library(caret)
      foldError <- NULL
      CVError <- NULL

      # create folds
      set.seed(seed)
      folds <- createFolds(y, k=k, list = FALSE)

      # perform k-fold CV
      for(i in 1:k){
            if(verbose){
                  print(paste("fold:", i))
            }
            trainFolds <- folds != i
            model <- RRotF(x=x[trainFolds,],y=y[trainFolds], ...)
            preds <- predict.RRotF(model, x[!trainFolds,])
            foldError[i] <- mean(preds != as.numeric(factor(y[!trainFolds]))-1)
      }
      CVError <- mean(foldError)
      return(list(avgCVError=CVError, perFoldError=foldError))
}
