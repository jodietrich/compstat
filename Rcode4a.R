# Bias-variance trade-off

# Non-linear function:
# Inputs: x, 
# Outputs: f(x)
f <- function(x){
  fx <- 0.3* x - 0.2*x^2 + 0.1*x^3 + sin(2*x)
  return(fx)
}

# plot function:
grid <- seq(from=-5,to=5,length=300)
par(mfrow=c(1,1))
plot(grid,f(grid),type="l")

# we are going to use loess smoothers (nonparametric regression), 
#   with different levels of smoothing:
span <- c(0.1,0.2,0.3,0.45,0.7,1)    # smoothing parameter for loess:
# small values mean little smoothing and hence complex models 

# parameters for data simulation:
sigma <- 1.5                        # standard devation of noise
n <- 100                            # sample size
x <- seq(from=-5,to=5, length=n)    # x-values (fixed throughout simulation)

# simulate data:
y <- f(x) + rnorm(n=length(x),mean=0,sd=sigma)

# fit loess smoothers and plot the data and the fitted regressions:
par(mfrow=c(2,3))
for (i in 1:length(span)){
  plot(x,f(x), type="l", lwd=2, main=paste("alpha=",span[i]))
  points(x,y)
  lo <- loess(y ~ x, span=span[i])
  lines(grid, predict(object=lo, grid),col="blue")
}

# repeat the above 20 times. 
# (we now only plots the fitted regression, not the data sets.) 
par(mfrow=c(2,3))
for (i in 1:length(span)){
   plot(x,f(x), type="l", lwd=2, main=paste("alpha=",span[i]))
   for(j in 1:20){
      y <- f(x) + rnorm(n=length(x),mean=0,sd=sigma)
      lo <- loess(y ~ x, span=span[i])
      lines(x, predict(object=lo, x),col="gray")
      abline(v=-3, lty=3)
   }
}

##################################################

# larger simulation

# parameters:
nsim <- 1000            # number of simulations
xtest <- -3             # x-value of test point

# create objects to store fitted values at training points and training MSE:
fit.train <- matrix(rep(NA,length(x)*length(span)),nrow=length(x))  
MSE.train <- matrix(rep(NA,nsim*length(span)),nrow=nsim)

# create objects to store fitted value and y-value at xtest:
fit.test <- matrix(rep(NA,nsim*length(span)),nrow=nsim)
y.test <- rep(NA,nsim)

for (i in 1:nsim){
  # generate new training data:
  y <- f(x) + rnorm(n=length(x),mean=0,sd=sigma)
  # generate new test data and store it in ith element of y.test
  y.test[i] <- f(xtest) + rnorm(length(xtest),mean=0,sd=sigma)
  
  # fit loess curves with different smoothing levels
  for (j in 1:length(span)){ 
    lo <- loess(y ~ x, span=span[j])
    # store fitted values at training points in jth column of fit.train
    fit.train[,j] <- predict(object=lo, x)
    # store fitted value at xtest in [i,j]th element of fit.test: 
    fit.test[i,j] <- predict(object=lo, xtest)
  }
  
  # store training MSE for different levels of smooth in ith row of MSE.train
  MSE.train[i,] <- apply((fit.train-y)^2,2,mean)
} 

# plot histograms of fitted values at xtest:
par(mfrow=c(2,3))
for (i in 1:length(span)){
  hist(fit.test[,i],xlim=range(fit.test),freq=F,
       main=paste("alpha=",span[i]),xlab="fitted value at x=2")
  lines(density(fit.test[,i]))
  abline(v=f(xtest))
}

# Check bias variance decomposition:
(ExpTestMSE <- apply((fit.test-y.test)^2,2,mean))
(Bias2 <- (apply(fit.test,2,mean)-f(xtest))^2)
(Var <- apply(fit.test,2,var))
(VarY <- var(y.test))
Bias2+Var+VarY - ExpTestMSE
# Note small errors because one cross-term does not fully disappear in simulation

# Plot results:
par(mfrow=c(1,1))
plot(span,ExpTestMSE,ylim=range(0,max(ExpTestMSE)), 
        type="l",lwd=2,col="purple", xlab="smoothing parameter alpha (small values indicate a flexible model)")
lines(span, Bias2,lwd=2,col="red")
lines(span,Var,lwd=2,col="blue")
abline(h=VarY,lty=2,lwd=2)
legend("topleft", c("Expected test MSE at xtest", "Bias^2", "Variance", "Irreducible error"),lwd=2, lty=c(1,1,1,2),col=c("purple", "red", "blue","black"))

