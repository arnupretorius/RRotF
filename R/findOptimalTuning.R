#' Tune Random Rotation Forest parameters.
#'
#' Finds optimal tuning parameters using k-fold cross-validation.
#' @param x Training data input matrix.
#' @param y Training data response.
#' @param k Number of folds.
#' @param paraGrid Parameter grid to search over for optimal parameters.
#' @param verboset Boolean, if true prints progress, otherwise prints nothing.
#' @param ... Additional arguments specified to \code{RRotF}.
#' @return A list containing the optimal tuning parameters and CV errors per fold.
#' @author Arnu Pretorius <arnupretorius@gmail.com>
#' @seealso \code{\link{kFoldRun}}
#' @examples
#' library(ElemStatLearn)
#' library(caret)
#' library(grid)
#' data("SAheart")
#' trainIndex <- createDataPartition(SAheart$chd, p=0.6, list=FALSE)
#' train <- SAheart[trainIndex,]
#' test <- SAheart[-trainIndex,]
#' Xtrain <- train[,-10]
#' ytrain <- train[,10]
#' parameterGrid <- expand.grid(K=round(ncol(Xtrain)/c(2,3), 0), L=c(10,20), mtry=c(1, 3))
#' optTune <- findOptimalTuning(Xtrain, ytrain, k=5, paraGrid = parameterGrid)
#' optTune
#' @export

findOptimalTuning <- function(x, y, k=10, paraGrid, verboset=TRUE,  ...){
      CVerrorVec <- NULL
      nconfig <- nrow(paraGrid)
      for(i in 1:nconfig){
            if(verboset){
                  print(paste("para config",i, "out of", nconfig))
            }
            K <- paraGrid[i,1]
            L <- paraGrid[i,2]
            mtry <- paraGrid[i,3]
            CVerrorVec[i] <- kFoldRun(x=x, y=y, k=k, K=K, L=L, mtry=mtry, ...)[[1]]
      }
      optParaIndex <- which(CVerrorVec == min(CVerrorVec))[1]
      list(optTuneVals=paraGrid[optParaIndex,], tuneValErrors=data.frame(paraGrid,CVerrorVec))
}
