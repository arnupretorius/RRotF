#' Predict method for RRotF objects.
#'
#' Prediction of new observations using RRotF model.
#' @param object An object of class \code{RRotF}, as created by the function \code{RRotF}.
#' @param newdata A data frame with the same predictors as in the training data.
#' @param type Type of prediction: 'prob' for probabilities and 'class' for class labels.
#' @return A vector containing either predicted class probabilities or labels.
#' @author Arnu Pretorius: \email{arnupretorius@gmail.com}
#' @references Rodriguez, J.J., Kuncheva, L.I., 2006. Rotation forest: A new classifier ensemble method. IEEE Trans. Pattern Anal. Mach. Intell. 28, 1619-1630. doi:10.1109/TPAMI.2006.211
#' @seealso \code{\link{RRotF}}
#' @examples
#' library(ElemStatLearn)
#' library(caret)
#' data("SAheart")
#' trainIndex <- createDataPartition(SAheart$chd, p=0.6, list=FALSE)
#' train <- SAheart[trainIndex,]
#' test <- SAheart[-trainIndex,]
#' Xtrain <- train[,-10]
#' ytrain <- train[,10]
#' Xtest <- test[,-10]
#' ytest <- test[,10]
#' mod <- RRotF(Xtrain, ytrain, model="log")
#' preds <- predict(mod, Xtest)
#' error <- mean(preds != ytest)
#' error
#' @export
predict.RRotF <- function (object, newdata, type="class"){
      if(class(object) != "RRotF"){
            stop("Object must be of class 'RRotF'")
      }
      newdata <- data.frame(sapply(newdata, as.numeric))
      if (!identical(colnames(newdata), object$columnnames))
            stop("Variable names and/or order of variables in newdata is not identical to training set. Please check if variables are exactly the same in both sets.")
      predicted <- matrix(NA, nrow = nrow(newdata), ncol = length(object$models))
      for (i in 1:length(object$models)) {
            final <- data.frame(as.matrix(newdata) %*% as.matrix(object$loadings[[i]]))
            predicted[, i] <- predict(object$models[[i]], final,
                                      type = "prob")[, 2]
      }
      if (type=="class") {
            ifelse(rowMeans(predicted) > 0.5, 1, 0)
      }
      else if(type=="prob") {
            rowMeans(predicted)
      } else {
            stop("Argument 'type' must be either 'class' or 'prob'")
      }
}
