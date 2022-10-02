# P-value, type I and type II errors

Year1 <- rnorm(10, 30, 10)
Year4 <- rnorm(10, 30, 10)

par(mfrow=c(1,2))
# Telling computer to display graphs in one row, 2 columns (so side-by-side)

hist(Year1)
hist(Year4)

t.test(Year1, Year4, var.equal=TRUE)

# 2.1 a:
# estimated means are 29.8 and 31.6. The different outputs are because
# there is a small sample size, and the individual points are random,
# leading to some variance.

# p-value is the probability that this result would happen given the
# null hypothesis is true. Since the p-value is 0.6, this outcome
# is very likely if the null hypothesis is true. Which tracks-- we
# know the means are the same.

# df is degrees of freedom, or n-1 for each sample set (10-1, twice)
# t-score: large, means different. small, means similar


P.value <- 1:1000
for (i in 1:1000) {
  Year1 <- rnorm(10, 30, 10)
  Year4 <- rnorm(10, 30, 10)
P.value[i] <- t.test(Year1, Year4, var.equal=T)$p.value}
#p-values of 1000 experiments with same sample size and variance

hist(P.value, breaks=100)
#histogram of p-values of those experiments

sign.tests <- ifelse(P.value<0.05, 1, 0)
#matrix of significant vs not significant p-values

sum(sign.tests)
#total number of significant trials

# 2.1b.
# 51 of 1000 tests were significant, which is about 5%

# Classmates: probably also have about 5% of results being significant
# (meaning you can reject null that means are the same)
# which is what is expected: with a p-value of 0.05,
# there is a 5% chance of having a Type 1 error-- rejecting the null
# hypothesis when it is true.


# 2.1c. Changing sample size
# Changing sample size doesn't affect type 1 error because
# by definition, the p-value of 0.05 allows for 5% possibility of T1Error

P.value <- 1:1000
for (i in 1:1000) {
  Year1 <- rnorm(1000, 30, 10)
  Year4 <- rnorm(1000, 30, 10)
  P.value[i] <- t.test(Year1, Year4, var.equal=T)$p.value}


hist(P.value, breaks=100)

sign.tests <- ifelse(P.value<0.05, 1, 0)

sum(sign.tests)

# Still 54 

#2.1d: change means so they are different

P.value <- 1:1000
for (i in 1:1000) {
  Year1 <- rnorm(2000, 29, 10)
  Year4 <- rnorm(2000, 30, 10)
  P.value[i] <- t.test(Year1, Year4, var.equal=T)$p.value}


hist(P.value, breaks=100)

sign.tests <- ifelse(P.value<0.05, 1, 0)

sum(sign.tests)

# small sample size, still about 5% significant, even though
# the means are different. Large type II error here, accept null tho false.
# large sample size, much more are significant.
# This is because the means ARE different, and this is emphasized
# in a larger sample size

# 2.2 

P.value <- 1:100
for (i in 1:100) {
  Year1 <- rnorm(100, 30, 10)
  Year4 <- rnorm(100, 30, 10)
  P.value[i] <- t.test(Year1, Year4, var.equal=T)$p.value}

hist(P.value, breaks = 100)

hist(p.adjust(P.value, "bonferroni"))

hist(p.adjust(P.value, "fdr"))


# 2.3

Bio <- rnorm(2000, 0, 1)
Tech <- rnorm(1000, 0, 2)
Total <- Bio + Tech

var(Bio)
var(Tech)
var(Total)

# 2.3a. i. Doubling technical replicates
# Reduces noise within biological replicates
# 2.3a. ii. Doubling biological replicates
# Reduces noise around mean; this would be more favorable

# In this case, 1000 biological replicates is already a LOT.
# I think doubling them would not be price advantaged, and
# would definitley be work-heavy compared to technical replicates.


# 2.3c. BONUS:

# SEM = sqrt((variance)/sample size)

# Bio SEM = sqrt((4)/1000)

# Tech SEM = sqrt(1/1000)

# Total SEM = sqrt((Total variance)/sample size)
sqrt(2000)
bio_SEM <- 1/(sqrt(1000))
tech_SEM <- sqrt(5)/sqrt(2000)
total_SEM <- sqrt(5/1000)
