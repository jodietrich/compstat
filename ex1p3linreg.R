#exercise 1 problem 3
require(MASS)

# (a)

n_sims <-  100
n <-  40
error_sd <- 5
set.seed(21)
beta1 <- 1
beta2 <- 2

Sigma <- toeplitz(c(seq(from = 1, to = 0, by = -0.1), rep(0, 29)))

# always the same x values
x = seq(1, n, 1)

# initialize vector with results. reg is a vector of lm result lists.
reg <- list()
est_slopes <- c()
linf <- beta1 + beta2*x
for (i in 1:n_sims) {
  # add noise to the linear function
  noise1 <- error_sd*rnorm(length(x))
  noise2 <- error_sd * (1 - rchisq(length(x), df = 1)) / sqrt(2)
  noise3 <- error_sd * rnorm(length(x), mean = x^2 / 40 - 1, sd = 1)
  noise4 <- error_sd * mvrnorm(n = 1, mu = rep(0, length(x)), Sigma = Sigma)
  noise5 <- error_sd * rnorm(length(x), mean = 0, sd = x / 20)
  
  errors <- noise5
  
  y <- linf + errors
  curr_reg <- lm(y ~ x)
  reg[[i]] <- curr_reg
  est_slopes <- c(est_slopes, curr_reg$coefficients[2])
}

# get an impression of the error distribution
hist(errors)
mean(errors)
var(errors)

# plot one simulated response
chosen <- 1
summary(curr_reg)
plot(x, reg[[chosen]]$model$y)
plot(reg[[chosen]], which = 1)

# (b)
emp_mean_betahat2 <- mean(est_slopes)
emp_sd_betahat2 <- sd(est_slopes)

# (c)
x_matrix <- model.matrix(y ~ x)
var_betahat <- solve(t(x_matrix) %*% x_matrix, error_sd**2 * diag(nrow = 2))
var_betahat2 <- var_betahat[2, 2]
sd_betahat2 <- sqrt(var_betahat2)

# (d)
hist(est_slopes, freq = FALSE, xlim = c(0,4))
curve(dnorm(b, mean=beta2, sd=sd_betahat2), xname = "b", add = TRUE, col="darkblue", lwd=2)
