# Computational statistics exercise set 3 problem 1
library(ggplot2)

# 1a read in data and make pairs plot
url <- "https://ww2.amstat.org/publications/jse/datasets/fruitfly.dat.txt"
data <- read.table(url)
data <- data[,c(-1,-6)] # remove id and sleep
names(data) <- c("partners","type","longevity","thorax")
pairs(data)

# 1b scatterplot
type_f <- as.factor(data$type)
partners_f <- as.factor(data$partners)
qplot(thorax, longevity, data = data, color= partners_f, shape = type_f) + scale_color_manual(values=c("red","green","blue"))

# 1c separate plots by number of partners
data_partner_split <- split(data, partners_f)
qplot(thorax, longevity, data = data_partner_split[[1]], shape = as.factor(type), main = "0 partners") + geom_point(color="red") 
qplot(thorax, longevity, data = data_partner_split[[2]], shape = as.factor(type), main = "1 partner") + geom_point(color="green")
qplot(thorax, longevity, data = data_partner_split[[3]], shape = as.factor(type), main = "8 partners") + geom_point(color="blue")

# 1d create dummy variables for different types and number of partners and make a box plot
type_0 <- 0==data$type
type_1 <- 1==data$type
partners_0 <- 0==data$partners
partners_1 <- 1==data$partners
partners_8 <- 8==data$partners
pt_09 <- partners_0
pt_10 <- (partners_1 & type_0)
pt_11 <- (partners_1 & type_1)
pt_80 <- (partners_8 & type_0)
pt_81 <- (partners_8 & type_1)
# boxplot(data$thorax~type_f+partners_f)
# boxplot(data$thorax~pt_09+pt_10+pt_11+pt_80+pt_81)
boxplot(data$thorax[c(pt_09)], data$thorax[c(pt_10)], data$thorax[c(pt_11)], data$thorax[c(pt_80)], data$thorax[c(pt_81)],
        names = c("pt_09", "pt_10", "pt_11", "pt_80", "pt_81"))

# test whether there are between group differences in the thorax length
anova(lm(data$thorax~1), lm(data$thorax~pt_10+pt_11+pt_80+pt_81))

# 1e effect of type of female on longevity
# test without thorax
fit1 <- lm(data$longevity~type_1, subset = partners_1)
fit1thorax <- lm(data$longevity~type_1+data$thorax, subset = partners_1)
summary(fit1)
summary(fit1thorax)

# 1f
fit2 <- lm(data$longevity~partners_f+type_f+partners_f*type_f)
summary(fit2)

# 1g full model
fullfit <- lm(data$longevity~pt_10+pt_11+pt_80+pt_81)
reducedfit <- lm(data$longevity~I(pt_11+pt_10)+I(pt_80+pt_10)+I(pt_81-pt_10))
summary(fullfit)
summary(reducedfit)
anova(reducedfit, fullfit)
