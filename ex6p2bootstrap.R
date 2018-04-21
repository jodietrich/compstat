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
samples.boot.para <- matrix(samples.gamma, ncol = length(boogg))
theta.function <- function(data){
  quant <- quantile(data, probs = 0.75)
}
theta.est <- theta.function(boogg)
theta.boot.para <- apply(samples.boot.para, 1, theta.function)

# 1d construct confidence intervals by hand
alpha <- 0.05
ci.boot.function <- function(original.statistic, boot.statistics, significance.level){
  twosided.cutoffs <- c(significance.level/2, 1-significance.level/2)
  ci.quantile <- quantile(boot.statistics, probs = twosided.cutoffs)
  theta.hat.sd.hat <- sd(boot.statistics)
  normal.sd <- qnorm(twosided.cutoffs[2])*theta.hat.sd.hat
  ci.normal <- theta.est + c(-normal.sd, normal.sd)
  theta.centered.boot <- boot.statistics - original.statistic
  centered.quantiles <- quantile(theta.centered.boot, probs = twosided.cutoffs)
  ci.reverserd <- original.statistic - rev(centered.quantiles)
  res.list <- list(ci.normal, ci.reverserd, ci.quantile)
  names(res.list) <- c("normal", "reversed (basic)", "quantile (percentile)")
  return(res.list)
}
ci.para.byhand <- ci.boot.function(theta.est, theta.boot.para, alpha)

# 1e construct the same confidence intervals with the boot package
require(boot)
gen.boot.sample <- function(original.sample, gamma.parameters) {
  n <- length(original.sample)
  boot.sample <- rgamma(n, shape = gamma.parameters[["shape"]], rate = gamma.parameters[["rate"]])
  return(boot.sample)
}
res.boot.parametric <- boot(boogg, theta.function, B.boot, sim = "parametric", ran.gen = gen.boot.sample, mle = fit.gamma$estimate)
ci.para.package <- boot.ci(res.boot.parametric, conf=1-alpha, type = c("norm","basic", "perc"))


# 1f compute the confidence intervals for nonparametric bootstrap by hand
samples.boot.nonpara <- t(replicate(B.boot, sample(boogg, length(boogg), replace = T)))
theta.boot.nonpara <- apply(samples.boot.nonpara, 1, theta.function)
ci.nonpara.byhand <- ci.boot.function(theta.est, theta.boot.nonpara, alpha)

ci.para.package
ci.para.byhand
ci.nonpara.byhand
# nonparametric confidence intervals are larger