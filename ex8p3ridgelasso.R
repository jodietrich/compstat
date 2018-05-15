# computational statistics series 8 problem 3

require(hdi)
require(glmnet)

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

find.best.lambda.cv <- function(data, glm.alpha, n.inner.folds){
  inner.cv.res <- cv.glmnet(data$x,data$y,family="gaussian",lambda=grid,nfolds = n.inner.folds,
                            alpha=glm.alpha)
  best.lambda <- inner.cv.res$lambda.min
  best.fit <- inner.cv.res$glmnet.fit
  return(best.fit)
}

compute.test.mse <- function(test.data, model.fit){
  y.predicted <- predict(model.fit, newx=test.data$x)
  mse <- mean((test.data$y-y.predicted)^2)
  return(mse)
}

test.mse.cross.validation <- function(data, folds, glm.alpha, n.inner.folds){
  n.outer.folds=length(unique(folds))
  test.scores <- vector(length = n.outer.folds)
  for (j in 1:n.outer.folds) {
    test.indices <- which(folds==j, arr.ind = T)
    training.data <- data[-test.indices,]
    test.data <- data[test.indices,]
    best.fit <- find.best.lambda.cv(training.data, glm.alpha, n.inner.folds)
    test.scores[j] <- compute.test.mse(test.data, best.fit)
  }
  avg.score <- mean(test.scores)
  return(avg.score)
}

compute.selection.test.mse <- function(data, n.outer.folds, n.inner.folds){
  data <- shuffle.data(data)
  folds <- create.cv.folds(data, n.outer.folds)
  ridge.test.mse <- test.mse.cross.validation(data, folds, 0, n.inner.folds)
  lasso.test.mse <- test.mse.cross.validation(data, folds, 1, n.inner.folds)
  results <- list(ridge.test.mse, lasso.test.mse)
  names(results) <- c("ridge","lasso")
  return(results)
}

# 3a
data(riboflavin)
x <- riboflavin$x
y <- riboflavin$y
grid <- 10^seq(10,-2, length = 100)
ridge.res <- glmnet(x,y,family="gaussian",alpha=0,lambda=grid)
lasso.res <- glmnet(x,y,family="gaussian",alpha=1,lambda=grid)
par(mfrow=c(2,1))
plot(ridge.res)
plot(lasso.res)

# 3b
n.outer.folds <- 10
n.inner.folds <- 10
selection.test.mse <- compute.selection.test.mse(riboflavin, n.outer.folds, n.inner.folds)
# ridge performs better
best.fit.all.lasso <- find.best.lambda.cv(riboflavin, 1, n.inner.folds)
best.fit.all.ridge <- find.best.lambda.cv(riboflavin, 0, n.inner.folds)
