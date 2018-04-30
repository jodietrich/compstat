# 3a load data and fit polynomial up to degree 3
polynomial.degree <- 3
filename <- "C:\\Users\\aaaa\\Documents\\Code\\compstat\\data_ex3.csv"

# plot distribution of errors
t <- seq(-4,4,.001)
z <- dgamma(t+1,2,2)
plot(t,z)

# plot data and fit
data <- read.csv(filename)
x <- data$x
y <- data$y
poly.x <- poly(x, degree = polynomial.degree, raw = T)
poly.fit <- lm(y~ poly.x)
poly.fit.summary <- summary(poly.fit)
all.points <- c(poly.fit$fitted.values, y)
ylim = c(min(all.points), max(all.points))
plot(x,y, ylim = ylim)
points(x,poly.fit$fitted.values, col = "red")

# 3b compute F-test pval
poly.fit.summary
global.fstatistic <- poly.fit.summary$fstatistic
fstat.pval <- 1-pf(global.fstatistic[1], global.fstatistic[2], global.fstatistic[3])

# 3c type I error simulation
n <- 20
N.sims <- 250
alpha <- 0.05 # significance level for F test

add.noise <- function(v){
  noise <- rgamma(length(v), shape=2, rate = 2) - 1
  v.noisy <- v + noise
}

compute.fstat <- function(y, x, degree = 3){
  stopifnot(length(x)==length(y))
  poly.x <- poly(x, degree = degree, raw = T)
  poly.fit <- lm(y~ poly.x)
  global.fstatistic <- summary(poly.fit)$fstatistic
  return(global.fstatistic)
}

pval.from.fstat <- function(fstat){
  fstat.pval <- 1-pf(fstat[1], fstat[2], fstat[3])
  return(fstat.pval)
}

pval.fstat <- function(y, x){
  fstat <- compute.fstat(y, x)
  pval <- pval.from.fstat(fstat)
  return(pval)
}

is.test.significant <- function(y, x, sign.level, test.func, ...){
  pval <- test.func(y, x, ...)
  significant <- pval <= sign.level
  return(significant)
}

significance.proportion <- function(x, y.matrix, sign.level, test.func){
  significance <- apply(y.matrix, 1, is.test.significant, x = x, sign.level = sign.level, test.func = test.func)
  significant.total <- sum(significance)
  significance.rate <- significant.total/length(significance)
  return(significance.rate)
}

x <- seq(from = 25, to = 30, length.out = n)
y.matrix.null <- apply(matrix(0, nrow = length(x), ncol = N.sims), 1, add.noise)
typeI.error.rate.ftest <- significance.proportion(x, y.matrix.null, alpha, pval.fstat)

# 3d power test
beta <- c(0.5, -0.003, 0.0001)
poly.function <- function(x, beta){
  res <- beta[1]*x + beta[2]*x^2 + beta[3]*x^3
  return(res)
}

y.from.x <- function(x, beta){
  y <- poly.function(x, beta) + add.noise(x)
  return(y)
}

y.matrix.alt <- apply(replicate(N.sims, x), 1, y.from.x, beta = beta)
power.ftest <- significance.proportion(x, y.matrix.alt, alpha, pval.fstat)

# 3e permutation test
data <- read.csv(filename)
x <- data$x
y <- data$y
N.perm <- 1000

fstat.func <- function(y, x, degree = 3){
  fstatistic <- compute.fstat(y, x, degree = degree)[1]
  return(fstatistic)
}

permutation.fstat.dist <- function(x, y, N.perm){
  y.permutations <- random.permutations(y, N.perm)
  fstats <- apply(y.permutations, 2, fstat.func, x=x, degree=3)
  return(fstats)
}

random.permutations <- function(v, N.perm){
  # each column is a permutation of v
  permutation.matrix <- apply(replicate(N.perm, v), 2, permute)
}

permute <- function(v){
  permutation <- sample(v, length(v), replace = F)
  return(permutation)
}

empirical.pval <- function(statistic, distribution){
  pval <- (sum(distribution >= statistic) + 1)/(length(distribution) + 1)
  return(pval)
}

permutation.test <- function(y, x){
  # returns p value
  fstat.distribution <- permutation.fstat.dist(x, y, N.perm)
  fstat.value <- fstat.func(y, x, degree = 3)
  pval <- empirical.pval(fstat.value, fstat.distribution)
  return(pval)
}

permutation.test(y, x)

# 3f test type I error rate and power
N.sims <- 250
y.matrix.null <- apply(matrix(0, nrow = length(x), ncol = N.sims), 1, add.noise)
y.matrix.alt <- apply(replicate(N.sims, x), 1, y.from.x, beta = beta)

typeI.error.rate.perm <- significance.proportion(x, y.matrix.null, alpha, permutation.test)
power.perm <- significance.proportion(x, y.matrix.alt, alpha, permutation.test)

