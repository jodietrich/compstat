# computational statistics series 8 problem 1

# for replication
set.seed(1)
n <- 30
p <- 50
# relevant covariate
x_true <- sample(c(0:1),size = n,replace = T)
# noise covariates
x <- matrix(sample(c(0:1),size=n*p,replace = T),ncol=p,nrow=n)
# combination of the two
x <- cbind(x_true, x)
# response
y <- ifelse(x[,1]==0, 0, sample(c(0:1), size = n, replace = T))

# 1a bonferroni correction
extract.pval.attr <- function(inner.list){
  return(inner.list$p.value)
}

n.tests <- dim(x)[2]
chisq.res <- apply(X = x, MARGIN = 2, FUN = chisq.test, y=y)
p.values.uncorrected <- sapply(chisq.res, extract.pval.attr)
min.pval.uncorrected <- min(p.values.uncorrected)
p.values.bonferroni <- pmin(p.values.uncorrected*n.tests,1)
min.pval.bonferroni <- min(p.values.bonferroni)
# only the first p-value is smaller than 1. It would be rejected for alpha=0.05.
# min pval corrected = 0.0931

# 1b Westfall Young permutation procedure
min.pval.chisq <- function(y.response,x.covariates.mat){
  chisq.res <- apply(X = x.covariates.mat, MARGIN = 2, FUN = chisq.test, y=y.response)
  p.values.uncorrected <- sapply(chisq.res, extract.pval.attr)
  min.pvalue <- min(p.values.uncorrected)
  return(min.pvalue)
}

empirical.pvalue.smallside <- function(statistic, statistic.distribution){
  as.extreme.count <- sum(statistic.distribution <= statistic)
  emp.pval <- (as.extreme.count+1)/(length(statistic.distribution)+1)
  return(emp.pval)
}

n.permutations <- 1000
y_permutations = replicate(n = n.permutations,expr = sample(y))
min.pval.distribution <- apply(y_permutations, 2, min.pval.chisq, x.covariates.mat=x)
westfal.young.pval <- empirical.pvalue.smallside(min.pval.uncorrected, min.pval.distribution)
# westfall young p value = 0.0190. less conservative
hist(min.pval.distribution)
