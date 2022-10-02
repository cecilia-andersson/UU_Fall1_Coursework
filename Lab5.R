## Regression and Correlation Lab
## Cecilia Andersson
## 28-09-2022

# Simulating different types of variance on two correlated traits
# Shared: variation in genes shared among two traits
# PrivateA/B: trait variance independent of each other,
# due to the effects of genes associated with one trait, but not the other
# EnvA/B: trait variance due to different effects environment has on
# each trait

shared <- rnorm(1000, 0, 20)
private.A <- rnorm(1000, 0, 20)
private.B <- rnorm(1000, 0, 20)
env.A <- rnorm(1000, 0, 10)
env.B <- rnorm(1000, 0, 10)

A <- shared + private.A + env.A
B <- shared + private.B + env.B


plot(A,B)

# Looks like a slight positive correlation (by eye), with a pretty big spread

## 5.1a Can you calculate the sample correlation coeff between A/B?

# r = cov(x,y)/sqrt(varA*varB)
sample_rho <- cov(A,B)/(sqrt(var(A)*var(B)))
sample_rho
# r gives proportion of variation in x and y that covaries, and we know
# the covariation is the variance of the 'shared' genes

## 5.1b

# true rho: using given values
# true_rho <- var(shared) / sqrt((tvar(A)*tvar(B)))
# true_rho <- (20^2)/ sqrt((20^2+20^2+10^2)^2)
true_rho <- (20^2)/900
true_rho


cor(A,B)

# cor(A,B) gives the sample rho

## 5.2 Simple Linear Regression

# set wd to downloads/lab5
crime <- read.csv("AmericanCrime.txt")
crime <- read.delim("AmericanCrime.txt")
# read.delim necessary for further processing


simple.1a <- lm(violent.crime~police.funding, data = crime)
simple.1b <- lm(log10(violent.crime)~police.funding, data = crime)
par(mfrow=c(2,2))
plot(simple.1a)
plot(simple.1b)

## coef. intercept (dependent var) estimate = Y-INTERCEPT of lsrl
## coef. police.funding (indep var) = SLOPE of lsrl !!!!

# wonky AF, dude. Try a log scale? The spread among residual values is huge

# 5.2b We need to deal with the data first, this is not okay to just start
# working with lmao

# Ask about Residuals v. Leverage graph

summary(simple.1b)

plot(lm(log10(violent.crime) ~ police.funding, data = crime))


# 5.2c Honestly, I'm having a hard time with this.

# Regression coefficients: 
#   Intercept may be the mean of the residuals of violent crime (logged)
#   Police funding is police funding dependent on violent crime?*

# How are t and F stats calculated?
#   F: sum of squares of the regression / (sum of squares of error/48)
#   Basically, the distance from the LSRL / distance of LSRL from the mean

# Degrees of freedom make sense? 1: yes because two variables (crime/funding)
#   48: yes, because 50 samples(states) - 2, one for each variable

# What does r2 value mean? 
#   Proportion of variance in y explained by x; 0.116 is not huge
#   So crime not very explained by police funding

# 5.2d

Number.of.oranges <- rnorm(length(crime$violent.crime), 100, 10)
Number.of.apples <- rnorm(length(crime$violent.crime), 100, 10)
mod2a <- lm(log10(violent.crime) ~ police.funding , data = crime)
mod2b <- lm(log10(violent.crime) ~ police.funding + Number.of.oranges +
              Number.of.apples , data = crime)
summary(mod2a)
summary(mod2b)

# So it looks like when you take into account the additional variables,
# you can increase adjusted R2-- telling you more 'accurately' how much
# of the variance is explained by the variable of interest, becuase it gets
# rid of the noise from the 'apples' and 'oranges' data

library(car)
library(MASS)
simple.3 <- rlm(violent.crime ~ police.funding , data = crime)
summary(simple.3)
Anova(simple.3)

# 5.2e
#   Compare p-values:
#   Compare t-values:
#   Compare coefficients:
#   What does robust regression do?

# 5.3a: Violent crime rate and police funding-- I do have a hypothesis,
# which is that areas with higher violent crime have higher police funding.
# Honestly, I think we will see correlation for sure, but if we're
# looking at whether high violent crime causes high funding (or whether
# high funding has an effect on crime), then we would use regression.
# So, maybe correlation to start?

summary(crime)
vcrime.hs <- lm(violent.crime~perc.highschool, data = crime)
vcrime.college <- lm(violent.crime~perc.college, data = crime)
plot(vcrime.hs)
plot(vcrime.college)

# I think perc.college is dependent on perc.highschool. Knowing about
# American crime, I also think police funding is dependent on violent crime,
# And I think violent crime and crime.rate are dependent on perc.highschool.

plot(crime)

library(corrplot)

corrplot(cor(crime))

# So, it looks like crime and violent crime are positively correlated
# And so are police funding and crime/violent crime. This makes sense, with
# either of them being the independent variable.
# There's a negative correlation between highschool grad rate and both
# types of crime, and a weak positive correlation between hs/college
# ed and police funding. Which is interesting!

uni.1a <- lm(log10(violent.crime) ~ police.funding, data = crime)
uni.1b <- lm(log10(violent.crime) ~ perc.highschool, data = crime)
multi.1 <- lm(log10(violent.crime) ~ police.funding + perc.highschool, data = crime)

summary(multi.1)


library(scatterplot3d)
scatterplot3d(crime$police.funding, crime$perc.highschool,
              crime$violent.crime, pch=20)

multi.2 <- lm(log10(violent.crime) ~ crime.rate + police.funding + perc.highschool + perc.college, data =
                crime)

summary(multi.2)

## think about after uploaded answers


# 5.3c

multi.3 <- lm(log(violent.crime) ~ police.funding + perc.highschool + perc.college, data = crime)

summary(multi.3)

library(sandwich)
library(RcmdrMisc)

partial.cor(crime)
