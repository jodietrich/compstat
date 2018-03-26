# computational statistics
# exercise set 2
# problem 1

# create data set
set.seed(0)
n<-100
z1<-rnorm(n)
z2<-rnorm(n)
M=matrix(c(1,1,0.1,-0.1),2,2)
X=t(M%*%rbind(z1,z2))
beta<-c(0.5,-1.0)
p <- length(beta) + 1
x1=X[,1]
x2=X[,2]
y=5+beta[1]*x1+beta[2]*x2 +rnorm(n)

#1a plot data
# plot(x1)
# plot(x2)
plot(X)
# plot(y~x1+x2)

#1b fit linear model
fit1<-lm(formula=y~x1+x2)
(lm_summary <- summary(fit1))
# plot(fit1)

# 1c t-value
coefficient_index = "x1"
std_err_bi <- lm_summary$coefficients[coefficient_index,2]
tval <-  fit1$coefficients[coefficient_index]/std_err_bi

# 1d
pval_bi <- 2*pt(-tval, df = n-p)

# 1e F-Test
fit2 <- lm(y~1)
anova(fit2, fit1)

# 1g Residual standard error
resstderr <- (1/sqrt(n-p))*norm(residuals(fit1), type = "2")

# 1h R**2
yhat=y-residuals(fit1)
ybar=mean(y)*replicate(n, 1)
Rsquared <- sum((yhat-ybar)**2)/sum((y-ybar)**2)

# 1i model with one variable
fit3 <- lm(y~x1)
summary(fit1)
summary(fit3)

