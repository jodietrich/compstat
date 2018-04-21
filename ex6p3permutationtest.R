# constants
ntypes <- 682
npack <- c(25, 30, 35, 40)
cards.per.pack <- 5
ncards <- npack*cards.per.pack
sign.lvl <- 0.05  # alpha
p <- 2/3
N.sim <- 100000
power.min <- 0.8

count.duplicates <- function(sample){
  nduplicates <- length(sample) - length(unique(sample))
  return(nduplicates)
}

sample.null <- function(n, types = ntypes){
  cards <- sample(1:types, size = n, replace = T)
  return(cards)
}

sample.alternative <- function(p.without.replace = p, n = ncards, types = ntypes){
  no.repl.size <- round(p*n)
  repl.size <- n - no.repl.size
  without.replacement.cards <- sample(1:types, size = no.repl.size, replace = F)
  replacement.cards <- sample(1:types, size = repl.size, replace = T)
  card.sample <- c(without.replacement.cards, replacement.cards)
  return(card.sample)
}

simulate.draws <- function(ndraws = N.sim, sample.function, N.samples){
  sample.matrix <- replicate(ndraws, sample.function(N.samples))
  return(sample.matrix)
}

power.analysis <- function(null.statistics, alt.statistics, alpha = sign.lvl){
  rejection.end <- quantile(null.statistics, probs = alpha)
  alt.acceptance.count <- sum(alt.statistics <= rejection.end)
  power <- alt.acceptance.count/length(alt.statistics)
  return(power)
}

power.analysis.loop <- function(null.statistics, alt.statistics, alpha = sign.lvl){
  n.distributions <- dim(null.statistics)[1]
  stopifnot(dim(null.statistics)[1] == dim(alt.statistics)[1])
  powers <- c()
  for(i in 1:n.distributions){
    powers <- c(powers, power.analysis(null.statistics[i,], alt.statistics[i,], alpha))
  }
  return(powers)
}

results.plots <- function(null.statistics, alt.statistics, N.sample.sizes = ncards, alpha = sign.lvl){
  n.distributions <- dim(null.statistics)[1]
  stopifnot(dim(null.statistics)[1] == dim(alt.statistics)[1])
  for(i in 1:n.distributions){
    histogram.plot(null.statistics[i,], alt.statistics[i,], N.sample.sizes[i], alpha)
  }
}

histogram.plot <- function(null.statistics, alt.statistics, N.sample.size, alpha = sign.lvl){
  p1 <- hist(null.statistics, plot = F)
  p2 <- hist(alt.statistics, plot = F)
  plot( p1, col=rgb(0,0,1,1/4), main = paste("Ncards = ", N.sample.size), xlab = "number of duplicates", xlim = c(0,40))
  plot( p2, col=rgb(1,0,0,1/4), xlim = c(0,40), add=T)
  rejection.end <- quantile(null.statistics, probs = alpha)
  abline(v = rejection.end, lty = 1, col = "red")
}

run.simulation <- function(sample.function, N.sample.sizes, statistic = count.duplicates){
  sample.list <- vector("list", length = length(ncards))
  statistics <- matrix(data = NA, nrow = length(ncards), ncol = N.sim)
  for (j in 1:length(N.sample.sizes)) {
    sample.list[[j]] <- simulate.draws(N.sim, sample.function, N.sample.sizes[j])
    statistics[j,] <- apply(sample.list[[j]], 2, statistic)
  }
  res.list <- list(N.sample.sizes, sample.list, statistics)
  names(res.list) <- c("n cards", "sample list", "statistics")
  return(res.list)
}

set.seed(1)
res.null <- run.simulation(sample.null, ncards)
res.alt <- run.simulation(sample.alternative, ncards)
powers <- power.analysis.loop(res.null$statistics, res.alt$statistics)
results.plots(res.null$statistics, res.alt$statistics)
# 0.8 power first exceeded at 150 cards