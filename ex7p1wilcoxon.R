X <-  c(0.5, 3)
Y <-  c(0, 2)

wilcox.test(x = X, y = Y, alternative = "greater", paired = T)
