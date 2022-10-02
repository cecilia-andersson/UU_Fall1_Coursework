## ANOVA Lab
## Cecilia Andersson
## 16-09-2022

## 4.1 One-Way ANOVA

# Change WD to Lab4, then read in diet.txt

diet <- read.csv("diet.txt")
diet <-read.delim("diet.txt")
head(diet)
tail(diet)
str(diet)

# Calculating weight loss

diet$weight.loss <- diet$pre.weight - diet$weight6weeks

head(diet)
tail(diet)
# Create boxplot of weight loss depending on diet type

boxplot(weight.loss ~ Diet, data = diet, col = "light grey", ylab = "Weight loss (kg)",
        xlab = "Diet type", main = "Diets Suck")
abline(h=0, col="blue")

# Trying ANOVA

# ANOVA-specific: analysis of variance
mod1a <- aov(weight.loss ~ Diet, data = diet)

# Linear model; more general
mod1b <- lm(weight.loss ~ Diet, data = diet)


## 4.1a

# Plotting four plots side-by-side from mod1a
# ASSUMPTIONS:
# 1. Response is continuous. Check, weight is continuous.
# 2. At least one indep. variable is discrete. Check, Diet type is discrete
#     ** if this isn't true, should apply a regression
# 3. Variance within groups is equal among groups
#     ** This isn't quite true, based on boxplots? By eye, A has less than B/C
# 4. Residuals are normally distributed
#     ** Check on this lol

par(mfrow=c(2,2))
plot(mod1a)

# TOP LEFT: Residuals vs Fitted
# residuals = difference between expected and observed
# fitted values = what the model predicts the mean to be for that diet
# vertical spread is the +/- distance each value is from the mean

# Though the data are spread at three vertical lines, they are spread equally above
# and below zero/ the line of best fit for all three diets.
## confirms homogeneity of variance


# TOP RIGHT: Normal Q-Q
# "Standardized Residuals" vs theoretical quantiles

# This also looks good-- the residuals sit very closely along the line
# line of best fit, with about an equal number above and below
# Relatively straight diagonal line implies normality :)

# BOTTOM LEFT: Scale-Location
# sqrt(Standardized residuals) vs Fitted Values

# The line is a little tilted upwards, but overall horizontal, suggesting
# relative "homoscedasticity", which means constant variance.
# This confirms my boxplot eyeball about the constant variance assumption


# Check out histogram of residuals

hist(resid(mod1a))
# Woooo pretty normal!


# Trying with lm

plot(mod1b)


# Checking out summaries of each

summary(mod1a)
# F value: variance between groups/variance within groups
# High f-value means significance can be attributed to inter-group differences
# significant p-value: 0.0066. Df = 2
# This means that there (likely) is a significant difference in effect
# among the three diets

summary(mod1b)


## 4.1.b What do "coefficients" mean?
diet
wl.a<-c(diet$weight.loss[0:14], diet$weight.loss[44:53])
mean(wl.a)
wl.b<-c(diet$weight.loss[15:28], diet$weight.loss[54:64])
mean(wl.b)
wl.c<-c(diet$weight.loss[29:43], diet$weight.loss[65:76])
mean(wl.c)

# So, what it looks like is the coefficients are the distance
# from the mean of Diet A (mean =3.3) weight loss, 
# for Diet B (-0.032; mean = 3.268)
# and Diet C (+1.8481; mean = 5.148148)

# So they are comparing the means, standardized against Diet A
# Then calculating the t-value of the two other diets based on their difference in means
# and standard error: finding which is contributing to the difference
# First (intercept) line Pr(>|t|) tells us that there is a significant difference somewhere
# Diet B not signif different, but Diet C is


##4.1.c. What do SS and MS stand for?
## SS = sum of squares, which the sum of the variance among 
  ## Huge for the residuals (410), meaning lots of variance
  ## Smaller for treatment (60.5)
  ## Not seeing anything for 'total'
## MS = mean square, which is the mean of the variances
  ## Larger for treatment (30.264)
  ## than for residuals (5.622)
  ## still no totals?

## F-value is 

library(car)
Anova(mod1a, type=2)
Anova(mod1b, type=2)

TukeyHSD(mod1a)
plot(TukeyHSD(mod1a))

## 4.1d Which groups differ, and was this expected?
#A-C and B-C differ; this makes sense, as C's distribution was
#way different from either A or B, which were more similar.
## The confidence limits: 95% confidence that the 
# difference in means between the stated diets is within that range.
# So, for C-A, we are 95% confident that the true difference in means
# between diets C and A lies between the lower (0.3) and upper (1.9) limit
# Even on the lower end, there is a difference in means

# 4.2

mod2a <- lm(weight.loss ~ Diet + sex, data=diet)
mod2a


## 4.2.a
## it doesn't look like sex has an effect on diet
## Based on this, including sex in this way doesn't change my 
## conclusion about the effect of the diet

mod2b <- lm(weight.loss ~ Diet + sex + Diet:sex, data = diet)
mod2c <- lm(weight.loss ~ Diet * sex, data = diet)

summary(mod2b)
summary(mod2c)


library(lattice)
bwplot(weight.loss~ Diet|sex, data=diet)

# it does look like sex has an effect; dietc:sexM is no longer significant
# in both models

## weight loss in diet c is only significant for women, not for men!

install.packages("effects")
library(effects)
plot(allEffects(mod2b))

# Wow woow okay this shows very clearly that there is a significant
# difference in weight loss among diets (c mostly) for women,
# but not for men


## 4.3- GLMs

# Poisson data:
A <- rpois(10000, 1) #poisson variable with mean=variance=1
B <- rpois(10000, 1.2) # '' mean=variance=1.2
pois.data <- data.frame(c(rep("A", 10000), rep("B", 10000)), c(A,B))

mod.pois <- glm(pois.data[,2]~pois.data[,1], family="poisson")

summary(mod.pois)

Anova(mod.pois)

# 4.3.a


exp(-0.00441)
# 0.9955997

exp(0.17777)
# 1.194551

##These are the 

# Trial in class (simulation so numbers different)
exp(-0.002904)
#0.9971002

###BRUHHHH this is the mean of the data you inputted

exp(0.185559)
#1.203891

#Binomial model (logistic regression):
A <- rbinom(10000, 1, 0.2) #generates a binomial-variable probability of success = 0.2
B <- rbinom(10000, 1, 0.1) #generates a binomial-variable probability of success = 0.1
binom.data <- data.frame(c(rep("A", 10000), rep("B",10000)), c(A,B)) #Create a dataset
mod.binom <- glm(binom.data[,2] ~binom.data[,1], family = "binomial")

summary(mod.binom)
Anova(mod.binom)


exp(-1.41848)/(1+exp(-1.41848))
# 0.1949
## close to the probability

exp(-1.39068)/(1+exp(-1.39068))
#0.1992

exp(((-1.39068)+(-0.82515))/(1+exp((-1.39068)+(-0.82515))))
# 0.1356157



## 'Random' f-value takes into account the variance caused by the 'bird x prev' effect
## in bird example, the 'preview' effect is significant, but the preview effect when taking
## into account the 'bird x prev' effect, the preview effect is no longer significant


jimson <- read.csv("Jimson.txt")