prob_sample_in_boot <- function(n) {
  return((1-1/n)^n)
}

plot(prob_sample_in_boot, from = 1, to = 1000, log="x")
abline(h=exp(-1), lty=2)
