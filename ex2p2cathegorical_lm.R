# prepare data
library(ISLR)
data(Carseats) #use ?Carseats for an explaination of the dataset
shelveloc=Carseats$ShelveLoc
sales=Carseats$Sales
advertising=Carseats$Advertising
# fit using automatic coding
fit<-lm(sales~shelveloc+advertising)
summary(fit)

# 2a a1, a2
# boolean vectors for easy construction of a1, a2, b1,...
bad<- levels(shelveloc)[1]==shelveloc
good<- levels(shelveloc)[2]==shelveloc
medium<- levels(shelveloc)[3]==shelveloc
a1<-medium*1
a2 <- good*1
fit_a <- lm(sales~a1+a2+advertising)
summary(fit_a)

# 2b b1, b2
b1 <- bad*1
b2 <- good*1
fit_b <- lm(sales~b1+b2+advertising)
summary(fit_b)

#2cd c1, c2, c3
c1 <- bad*1
c2 <- medium*1
c3 <- good*1
fit_c <- lm(sales~c1+c2+c3+advertising-1)
summary(fit_c)

# 2e
max(abs(fitted(fit_a)-fitted(fit_b)))
max(abs(fitted(fit_b)-fitted(fit_c)))
max(abs(fitted(fit_a)-fitted(fit_c)))

# 2g
d <- a1+a2
fit_d <- lm(sales~d+advertising)
anova(fit_d, fit_a)


