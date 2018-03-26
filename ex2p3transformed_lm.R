# computational statistics series 2 problem 3

# function definitions
one_hot <- function(label, n_labels){
  one_hot_vector <- rep(0, n_labels)
  one_hot_vector[label] <- 1
  return(one_hot_vector)
}

one_hot_month <- function(month_num){
  return(one_hot(month_num, 12))
}


#load data
airline <- scan("http://stat.ethz.ch/Teaching/Datasets/airline.dat")

# 3a plot data against time
plot(airline, type = "l")

# 3b plot log transformed data over time
log_airline <- log(airline)
plot(log_airline, type = "l")

# 3c linear model
# 1-12 repeatedly i.e. month for every index
time <- 1:length(log_airline)
month <- (0:(length(log_airline)-1) %% 12) + 1
encoded_months <- sapply(month, one_hot_month)
X <- cbind(time, t(encoded_months))
fit1 <- lm(log_airline~X-1)
summary(fit1)

# 3d plots to check model assumptions
plot(time, fitted(fit1))
plot(time, residuals(fit1))

# 3f model with seasons
s1 <- apply(X[,4:6], 1, sum)
s2 <- apply(X[,7:9], 1, sum)
s3 <- apply(X[,10:12], 1, sum)
s4 <- apply(X[,c(13,2,3)], 1, sum)
fit2 <- lm(log_airline~s1+s2+s3+s4+time-1)
summary(fit2)
anova(fit2, fit1)
