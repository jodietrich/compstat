# computational statistics series 8 problem 2
require(ISLR)
require(leaps)

# functions
shuffle.data <- function(dataset){
  row.indices <- 1:nrow(dataset)
  shuffled.indices <- sample(row.indices)
  dataset = dataset[shuffled.indices,]
  return(dataset)
}

create.cv.folds <- function(dataset, num.folds){
  indices <- seq(1,nrow(dataset))
  cv.folds <- cut(indices, breaks = num.folds, labels = FALSE)
  return(cv.folds)
}

outer.measures.cross.validation <- function(data, folds, max.p, n.inner.folds){
  test.scores <- matrix(nrow = 4, ncol = k.folds)
  for (j in 1:k.folds) {
    test.indices <- which(folds==j, arr.ind = T)
    training.data <- data[-test.indices,]
    test.data <- data[test.indices,]
    which.matrix <- multiple.selection.which(training.data, n.inner.folds, max.p)
    test.scores[,j] <- train.test.models(which.matrix, training.data, test.data)
  }
  avg.scores <- apply(test.scores,1,mean)
  names(avg.scores) <- c("bic","cp","adjr2","cv5")
  return(avg.scores)
}

train.test.models <- function(which.matrix, training.data, test.data){
  lm.models <- fit.models.from.predictor.selection(which.matrix, training.data)
  predictions <- sapply(lm.models, predict, newdata = test.data)
  test.scores <- apply(predictions,2,compute.mse,ground.truth = test.data[["Salary"]])
  return(test.scores)
}

cv.model.selection.procedure <- function(data, k.folds, max.p){
  data <- shuffle.data(data)
  folds <- create.cv.folds(data, k.folds)
  test.scores <- matrix(nrow = max.p, ncol = k.folds)
  for (j in 1:k.folds) {
    test.indices <- which(folds==j, arr.ind = T)
    training.data <- data[-test.indices,]
    test.data <- data[test.indices,]
    best.subset <- regsubsets(Salary ~ .,data = training.data,nbest = 1,nvmax = max.p,method = "exhaustive")
    best.subset.summary <- summary(best.subset)
    current.scores <- train.test.models(best.subset.summary$which, training.data, test.data)
    test.scores[,j] <- current.scores
  }
  avg.scores <- apply(test.scores,1,mean)
  best.npredictors <- which.min(avg.scores)
  best.subset.all <- regsubsets(Salary ~ .,data = data,nbest = 1,nvmax=best.npredictors,method = "exhaustive")
  best.subset.summary.all <- summary(best.subset.all)
  best.predictors <- best.subset.summary.all$which[best.npredictors,]
  return(best.predictors)
}

compute.mse <- function(predictions, ground.truth){
  stopifnot(length(ground.truth)==length(predictions))
  mse <- mean((predictions-ground.truth)^2)
  return(mse)
}

model.selection.procedure <- function(data, max.p){
  # for statistics that can be computed from the fit directly
  # need different method for CV model selection
  best.subset <- regsubsets(Salary ~ .,data = data,nbest = 1,nvmax=max.p,method = "exhaustive")
  best.subset.summary <- summary(best.subset)
  best.subset.indices <- c(which.max(best.subset.summary$bic),
                            which.max(best.subset.summary$cp),
                            which.max(best.subset.summary$adjr2))
  which.matrix <- best.subset.summary$which[best.subset.indices,]
  rownames(which.matrix) = c("bic","cp","adjr2")
  return(which.matrix)
}

fit.models.from.predictor.selection <- function(which.matrix, data){
  fitted.models <- apply(which.matrix, 1, fit.subset.model, data=data)
  return(fitted.models)
}

fit.subset.model <- function(predictors.mask, data){
  dependent.mask <- names(data)=="Salary"
  sub.mask <- predictors.mask | dependent.mask
  fit <- lm(Salary ~ ., data = data[,sub.mask])
  return(fit)
}

multiple.selection.which <- function(data, n.inner.folds, max.p){
  which.matrix <- matrix(nrow = 4, ncol = ncol(data))
  which.matrix[1:3,] <- model.selection.procedure(data, max.p)
  which.matrix[4,] <- cv.model.selection.procedure(data, n.inner.folds, max.p)
  return(which.matrix)
}

  
# 2a load data
data = Hitters
data = na.omit(data)

# 2b create cross validation folds
k.folds <- 10
k.inner.folds <- 5
max.p <- 8
data <- shuffle.data(data)
folds <- create.cv.folds(data, k.folds)

#2c model selection and cross validation
sel.comp.mse <- outer.measures.cross.validation(data, folds, max.p, k.inner.folds)
# fit all selection on the whole data
which.all.data <- multiple.selection.which(data, k.inner.folds, max.p)
lm.models.all.data <- fit.models.from.predictor.selection(which.all.data, data)
# the results, which is the best vary
