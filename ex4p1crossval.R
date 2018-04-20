# Computational Statistics exercise set 4
library(kknn)
# 1a
n <- 500
p <- 2
# define transformation functions for random variables
g1<-function(x){2*x/(1+abs(x)^1.5)} # Favour x-values with larger absolute value
g2<-function(x){x^3/abs(x)^1.5} # Favour x-values with smaller absolute value
g3<-function(x){x} # Keep the uniform distribution
plot(g1)
plot(g2)
plot(g3)

x<-runif(n*p,min=-1,max=1)
Z<-matrix(x,ncol=p)
X<-g1(Z)
plot(X)
X<-g2(Z)
plot(X)
X<-g3(Z)
plot(X)

# 1b
sampleX <- function(n=500, g=g1){
  x<-runif(n*p,min=-1,max=1)
  Z<-matrix(x,ncol=p)
  X<-g(Z)
  return(X)
}

# 1c
f1dim <- function(x){ sin(8*x)/(1+(4*x)^2) }
plot(f1dim)

f <- function(X.matrix){
  sapply(X = X.matrix[,1], FUN = f1dim)
}

# 1d
sampleY<-function(X, sigma=0.5){ return(f(X)+rnorm(n = n, mean = 0, sd = sigma))}

# 1e
M <- 1000
test.mse <- vector(mode = "numeric", length = M)
for(i_sim in 1:M){
  Xtrain<-sampleX(n = 500) # some training data
  Ytrain<-sampleY(Xtrain)
  dfTrain=data.frame(y=Ytrain,x=Xtrain) # wrap in a dataframe
  Xtest<-sampleX(n = 2000) # same for test data
  Ytest<-sampleY(Xtest)
  dfTest=data.frame(x=Xtest)
  fit.kknn <- kknn(y ~ ., dfTrain,dfTest,k=8)
  predTest=predict(fit.kknn) # predictions on dfTest
  squared_error <- (predTest - Ytest)^2
  test.mse[[i_sim]] <- mean(squared_error)
}
sim.test.mse <- mean(test.mse)

# 1f different validation methods
ValidationSet<-function(X,Y){
  n<-length(Y)
  s <- sample(1:n, size=n, replace=F)
  folds <- cut(seq(1,n), breaks=2, labels=FALSE)
  ind.test <- s[which(folds==1)]
  dfTrain=data.frame(y=Y[-ind.test],x=X[-ind.test,])
  dfTest=data.frame(x=X[ind.test,])
  fit.kknn <- kknn(y ~ ., dfTrain,dfTest,k=8)
  predTest=predict(fit.kknn)
  Ytest<-Y[ind.test]
  MSEEstimate=mean((predTest-Ytest)^2)
  return(MSEEstimate)
}

RepeatedValidationSet<-function(X,Y){
  MSEEstimate <- replicate(10, ValidationSet(X,Y))
  return(mean(MSEEstimate))
}

crossval <- function(X, Y, k.folds=10){
  n<-length(Y)
  s <- sample(1:n, size=n, replace=F)
  folds <- cut(seq(1,n), breaks=k.folds, labels=FALSE)
  folds.test.mse <- vector("numeric", k.folds)
  if(k.folds==n){
    "LOOCV reached"
  }
  for(i in 1:k.folds){
    ind.test <- s[which(folds==i)]
    dfTrain=data.frame(y=Y[-ind.test],x=X[-ind.test,])
    dfTest=data.frame(x=matrix(X[ind.test,], ncol = ncol(X)))
    fit.kknn <- kknn(y ~ ., dfTrain,dfTest,k=8)
    predTest=predict(fit.kknn)
    Ytest<-Y[ind.test]
    folds.test.mse[[i]]=mean((predTest-Ytest)^2)
  }
  MSEEstimate <- mean(folds.test.mse)
  VarEstimate <- (1/k.folds)*var(folds.test.mse)
  CV.results <- list(MSEEstimate, VarEstimate)
  return(CV.results)
}

crossval.mean <- function(X, Y, k.folds=10){
  return(crossval(X, Y, k.folds = k.folds)[[1]])
}

crossval.var <- function(X, Y, k.folds=10){
  return(crossval(X, Y, k.folds = k.folds)[[2]])
}

RepeatedCrossVal<-function(X,Y, k.folds=10){
  MSEEstimate <- replicate(10, crossval.mean(X,Y, k.folds = k.folds))
  return(mean(MSEEstimate))
}

LOOCV <- function(X, Y){
  LOOCV.results <- crossval.mean(X,Y,length(Y))
  return(LOOCV.results)
}

# 1g evaluate different estimates
EvaluateOnSimulation<-function(estimationFunction, iterations=200){
  result<-numeric(iterations)
  for (i in 1:iterations) {
    X<-sampleX()
    Y<-sampleY(X)
    result[i]= estimationFunction(X,Y)
  }
  return(result)
}

# estimate distribution of estimators by simulation
EstimatesVS <- EvaluateOnSimulation(ValidationSet)
EstimatesRVS <- EvaluateOnSimulation(RepeatedValidationSet)
EstimatesCV <- EvaluateOnSimulation(crossval)
EstimatesRCV <- EvaluateOnSimulation(RepeatedCrossVal)
EstimatesLOOCV <- EvaluateOnSimulation(LOOCV)

Estimates <- cbind(EstimatesVS,EstimatesRVS,EstimatesCV,EstimatesRCV,EstimatesLOOCV) #results from the 5 CV methods
boxplot(Estimates, names=c("VS","RVS","CV", "RCV", "LOOCV"))
abline(h=sim.test.mse, lty=2)

# 1h estimate bias and variance of estimators
mean.Estimates <- apply(X = Estimates, MARGIN = 2, FUN = mean)
Biases <- mean.Estimates - sim.test.mse
Variances <- apply(X = Estimates, MARGIN = 2, FUN = var)

# 1j check cv variance estimate
CV.Variance.estimates <- EvaluateOnSimulation(estimationFunction = crossval.var, iterations = 1000)
mean.CV.Variance.estimate <- mean(CV.Variance.estimates)
mean.CV.Variance.estimate - Variances["EstimatesCV"]
hist(CV.Variance.estimates)
abline(v=mean.CV.Variance.estimate, lty=2, col="red")
abline(v=Variances["EstimatesCV"], lty=2, col="blue")