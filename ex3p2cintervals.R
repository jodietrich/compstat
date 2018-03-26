# Computational statistics exercise 3 problem 2
# Jonathan Dietrich

# 1a look at data
url <- "https://raw.githubusercontent.com/jawj/coffeestats/master/lifeexp.dat"
data <- read.table(url, sep="\t", header=T, row.names=1)
data <- data[,c("LifeExp","People.per.TV","People.per.Dr")]

hist(data$LifeExp)
hist(data$People.per.TV)
hist(data$People.per.Dr)
pairs(data)
country_selection = 1:3
# best life expectancy
data[order(data$LifeExp, decreasing = T)[country_selection],]
# highest number of people per TV
data[order(data$People.per.TV, decreasing = T)[country_selection],]
# highest number of people per doctor
data[order(data$People.per.Dr, decreasing = T)[country_selection],]

# 1b fit linear regression
# remove countries with missing values
cdata <- subset(data, complete.cases(data))
LifeExp <- cdata$LifeExp
x1 <- log2(cdata$People.per.TV)
x2 <- log2(cdata$People.per.Dr)
fit <- lm(LifeExp~x1+x2)
summary(fit)

# 1c highest cook's distance countries
outliers <- order(cooks.distance(fit), decreasing = T)[1:2]
row.names(cdata)[outliers]

# 1d confidence and prediction intervals
# new country predicted life expectancy
new_x <- c(1, log2(c(50, 3000)))
newdf <- data.frame(x1=new_x[2], x2=new_x[3])
predict(object = fit, newdata = newdf, interval = "confidence")
predict(object = fit, newdata = newdf, interval = "predict")

# 1e diagnostics plots
plot(fit, which = c(1,5))

# 1f exclude outliers and recompute confidence and prediction intervals
reduced_data <- cdata[-outliers,]
x1r <- log2(reduced_data$People.per.TV)
x2r <- log2(reduced_data$People.per.Dr)
fitr <- lm(reduced_data$LifeExp~x1r+x2r)
new_xr <- new_x
names(new_xr)[2:3] <- c("x1r", "x2r")
newdfr <- data.frame(x1r=new_xr[2], x2r=new_xr[3])
predict(object = fitr, newdata = newdfr, interval = "confidence")
predict(object = fitr, newdata = newdfr, interval = "predict")



