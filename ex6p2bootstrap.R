# The values are rounded to minutes (from 2000 to 2018).
boogg <- c(17, 26, 12, 6, 12, 18, 10, 12, 26, 13, 13, 11, 12, 35, 7, 21, 44, 10, 21)

# 1a plot
stripchart(boogg, method = "stack")
# fit gamma distribution
require(MASS)
fit.gamma <- fitdistr(boogg, "gamma")
shape.gamma <- fit.gamma$estimate[["shape"]]
rate.gamma <- fit.gamma$estimate[["rate"]]

# 1b histogram with density
hist(boogg, probability = T)
x <-  seq(1, 100, by=0.001)
lines(x = x, y = dgamma(x, shape = shape.gamma, rate = rate.gamma), col="red")

# 1c bootstrap by hand
B.boot <- 1000
samples.gamma <- rgamma(length(boogg)*B.boot, shape = shape.gamma, rate = rate.gamma)
samples.boot <- matrix(samples.gamma, ncol = length(boogg))
theta.function <- function(data){
  quant <- quantile(data, probs = 0.75)
}
theta.est <- theta.function(boogg)
theta.boot <- apply(samples.boot, 1, theta.function)

# 1d construct confidence intervals by hand
alpha <- 0.05
twosided.cutoffs <- c(alpha/2, 1-alpha/2)
(ci.quantile <- quantile(theta.boot, probs = twosided.cutoffs))
theta.hat.sd.hat <- sd(theta.boot)
normal.sd <- qnorm(twosided.cutoffs[2])*theta.hat.sd.hat
(ci.normal <- theta.est + c(-normal.sd, normal.sd))
