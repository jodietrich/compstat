require("MASS")
?immer

# 2a plot the data
Y1 = immer[["Y1"]]
Y2 = immer[["Y2"]]
plot(Y1,Y2)
abline(a=0,b=1)

# 2b program wilcoxon signed rank test myself
wilcoxon.signed <- function(x, y, N.permute){
  # tests whether x is significantly larger
  # returns p-value
  stopifnot(length(x) == length(y))
  d <-  x - y
  v <- signed.rank.stat(d)
  d.permutations <-  rand.flip.sign(d, N.permute)
  v.distribution <- apply(d.permutations, 1, signed.rank.stat)
  p.val <- empirical.pval(v, v.distribution)
  return(p.val)
}

empirical.pval <- function(statistic, distribution){
  pval <- (sum(distribution >= statistic) + 1)/(length(distribution) + 1)
  return(pval)
}

signed.rank.stat <- function(diff){
  absranks <- rank(abs(diff))
  greater.zero <- diff > 0
  v.stat <- absranks %*% greater.zero
  v.stat <- v.stat[1,1]
  return(v.stat)
}

rand.flip.sign <- function(diff, N){
  result.matrix <- matrix(nrow = N, ncol = length(diff))
  for(k in 1:N){
    sign.mask <- sample(c(1,-1), length(diff), replace = T)
    result.matrix[k,] <- diff*sign.mask
  }
  return(result.matrix)
}

N.permutations <- 100000
wilcox.pval.me <- wilcoxon.signed(Y1, Y2, N.permutations)

# 2c compare to R
wilcox.res.r <- wilcox.test(x = Y1, y = Y2, alternative = "greater", paired = T, exact = F)
wilcox.res.r$p.value
wilcox.pval.me
