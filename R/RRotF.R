#' Fit a Random Rotation Forest.
#'
#' Fit a Random Rotation Forest using randomised trees with orthogonal or oblique splits as base learners.
#' @param x Training data input matrix.
#' @param y Training data response.
#' @param K The number of variable subsets. The default is the value K that results in three features per subset.
#' @param L The number of base classfiers.
#' @param mtry Number of variables randomly sampled as candidates at each split.
#' @param model Specifies the base learner model: 'rpart' for ordinary classification trees; 'rf' for
#'    randomised trees; 'ridge', 'pls', 'log', 'svm' or 'rnd' for randomised trees using oblique splits with the corresponding model.
#' @param ... Additional arguments specified to \code{randomForest}, \code{rpart} or \code{obliqueRF}.
#' @return A fitted model object of type 'RRotF', which is a list containing base learner fits and PCA loadings.
#' @author Arnu Pretorius: \email{arnupretorius@gmail.com}
#' @references Rodriguez, J.J., Kuncheva, L.I., 2006. Rotation forest: A new classifier ensemble method. IEEE Trans. Pattern Anal. Mach. Intell. 28, 1619-1630. doi:10.1109/TPAMI.2006.211
#' @seealso \code{\link{predict.RRotF}}
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

RRotF <- function (x, y, K = round(ncol(x)/3, 0), L = 10, mtry=floor(sqrt(ncol(x))), model="log", ...){

      require(randomForest)
      require(rpart)
      require(obliqueRF)
      x <- data.frame(sapply(x, as.numeric))
      y <- factor(as.numeric(y)-1)
      while (ncol(x)%%K != 0) {
            K <- K - 1
      }
      M <- round(ncol(x)/K)
      predicted <- list()
      fit <- numeric()
      Ri <- list()
      Ria <- list()
      fit <- list()
      predicted <- matrix(NA, nrow = nrow(x), ncol = L)
      subsets <- list()
      SelectedClass <- list()
      IndependentsClassSubset <- list()
      IndependentsClassSubsetBoot <- list()
      pcdata <- list()
      loadings <- list()
      for (i in 1:L) {
            Independents <- x[, sample(1:ncol(x), ncol(x))]
            n <- 0
            subsets[[i]] <- list()
            SelectedClass[[i]] <- list()
            IndependentsClassSubset[[i]] <- list()
            IndependentsClassSubsetBoot[[i]] <- list()
            pcdata[[i]] <- list()
            loadings[[i]] <- list()
            for (j in seq(1, K)) {
                  n <- n + M
                  subsets[[i]][[j]] <- data.frame(Independents[, (n -
                                                                        (M - 1)):n], y)
                  SelectedClass[[i]][[j]] <- as.integer(sample(levels(as.factor(y)),
                                                               1))
                  IndependentsClassSubset[[i]][[j]] <- subsets[[i]][[j]][subsets[[i]][[j]]$y ==
                                                                               SelectedClass[[i]][[j]], ]
                  IndependentsClassSubsetBoot[[i]][[j]] <- IndependentsClassSubset[[i]][[j]][sample(1:dim(IndependentsClassSubset[[i]][[j]])[1],
                                                                                                    round(0.75 * nrow(IndependentsClassSubset[[i]][[j]])),
                                                                                                    replace = TRUE), ]
                  pcdata[[i]][[j]] <- princomp(IndependentsClassSubsetBoot[[i]][[j]][,
                                                                                     !colnames(IndependentsClassSubsetBoot[[i]][[j]]) %in%
                                                                                           "y"])
                  loadings[[i]][[j]] <- pcdata[[i]][[j]]$loadings[,
                                                                  ]
                  colnames(loadings[[i]][[j]]) <- dimnames(loadings[[i]][[j]])[[1]]
                  loadings[[i]][[j]] <- data.frame(dimnames(loadings[[i]][[j]])[[1]],
                                                   loadings[[i]][[j]])
                  colnames(loadings[[i]][[j]])[1] <- "rowID"
            }
            Ri[[i]] <- Reduce(function(x, y) merge(x, y, by = "rowID",
                                                   all = TRUE), loadings[[i]])
            Ri[[i]][is.na(Ri[[i]])] <- 0
            Ria[[i]] <- Ri[[i]][order(match(Ri[[i]]$rowID, colnames(x))),
                                order(match(colnames(Ri[[i]]), colnames(x)))]
            rownames(Ria[[i]]) <- Ria[[i]]$rowID
            Ria[[i]]$rowID <- NULL
            finalx <- data.frame(as.matrix(x) %*% as.matrix(Ria[[i]]))
            final <- data.frame(finalx, y)
            if(model=="rf"){
                  fit[[i]] <- randomForest(y ~ ., data = final, mtry=mtry, ntree=1,
                                           ...)
            } else if(model %in% c("ridge", "pls", "log", "svm", "rnd")){
                  capture.output(fit[[i]] <- obliqueRF(x = as.matrix(finalx), y=as.numeric(y), mtry=mtry, ntree=1,
                                                       training_method=model, verbose = FALSE, ...))
            } else {
                  stop("Argument 'model' not a valid model type.")
            }
      }
      res <- list(models = fit, loadings = Ria, columnnames = colnames(x), mod=model)
      class(res) <- "RRotF"
      res
}
