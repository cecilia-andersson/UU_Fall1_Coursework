## Basic Concepts II Lab
## Cecilia Andersson
## 12-09-2022

# Change working directory to Downloads/L3
two.sample <- read.csv("two.sample.csv")

# Look at data
head(two.sample)
summary(two.sample)

# Check to see if it fits normal curve
par(mfrow=c(1,2))
hist(two.sample$mutations[two.sample$parent.sex=="Mother"], xlab = "Mutations", main = "Mother")
hist(two.sample$mutations[two.sample$parent.sex=="Father"], xlab = "Mutations", main = "Father")

# Seems each follow normal curve, proceed to t.test

my.test <- t.test(mutations ~ parent.sex, var.equal=T, data = two.sample)
my.test

# P-value is 0.34, so cannot reject Ho that the means are the same.
# Degrees of freedom were 198, or 99 for each parent sex
# Father mut mean: 24.12, Mother mut mean: 23.01

# Welch two-sample t.test 

welch.test <- t.test(mutations ~ parent.sex, var.equal = F, data = two.sample)
welch.test

# In this case, p-value is the same and means are very close. Degrees of freedom
# is 197.9, which was weird to me

# Read in paired t-test dataset

paired <- read.csv("paired.csv")

# Perform paired t-test

paired.test <- t.test(paired$Mother, paired$Father, paired = T)
paired.test

# Degrees of freedom changed to 99 because linking the two groups
# changed the test number from 200 to 100, since the mother and father
# in each trio are no longer independent of one another
# P-value is very small, so very significant!
# Output includes mean of the differences rather than each group's
# mean, which highlights the consistent difference in mean between
# mothers and fathers linked together

# Use boxplots to check out the spread of the data, visually

ts.plot <- boxplot(mutations ~ parent.sex, data = two.sample)
pt.plot <- boxplot(paired$Mother-paired$Father) #look at difference

# In two sample, it looks like the means are very close and well
# within the spread of each other, so don't look significantly diff.
# But in paired, it's obvious that almost all have a mean difference of
# around 1 mutation, showing that the means are almost certainly not
# the same.


# 3.2

?Distributions
?pnorm

# pnorm with mean 10, sd 2

# probability individual's trait value is 13 or higher:
1 - pnorm(q = 13, mean = 10, sd = 2)

# 0.0668


## I understood the question incorrectly and tried to
## deduce % from guessing q using logic and pnorm function,
## skip to line 92 lol

# Looking for trait value below which are the bottom 20% of trait values
# My strategy is  guess and check lol
pnorm(q=9, mean = 10, sd = 2)
# about 30%

pnorm(q=8.5, mean = 10, sd = 2)
# closer, about 23%

pnorm(q=8.33, mean = 10, sd = 2)
# close as I wanna get, around 20.2%


# trying again with qnorm:
qnorm(p=0.2, mean = 10, sd = 2)

# trait value is 8.317! so very close to the guessed q value
# but now I know what pnorm and qnorm are for lmfao, i was doing
# too much


# 3.2b Poisson distr to generate 1000 read samples for gene with
# mean read count of 10. Plot data (with hist)

?rpois

rpois(1000, 10)

hist(rpois(1000, 10))

# It doesn't quite look normal-- there is a tail of data extending

# Trying a transformation: try log first, then sqrt, then arcsin
### This is something I need to talk to peers about


hist(rpois(log(1000), 10))
hist(rpois(log(1000), 100))
#nope
hist(rpois(sqrt(1000), 10))
hist(rpois(sqrt(1000), 100))
# little better but also nope
hist(rpois(1000, 10))
hist(rpois(1000, 100))
# still nope?

# ! From lecture: as n gets larger than ~30, distr of means approaches
# normal distr


# 3.3a

?chisq.test

# Vector of probabilities: 

matrix.data <- c(400, 800, 550, 190, 50, 10)
chisq.matrix <- matrix(matrix.data, nrow = 2, ncol = 3)

chisq.matrix

chisq.test(chisq.matrix)

# p-value is very small, which makes sense because it's clear pops are
# very different

# 3.3b: Hardy-Weinberg

# HW math: on paper--
# 1. calculated total number of As and a's in pops (As: 2*AA+Aa, as: Aa+2*aa)
# 2. divided each by total number of alleles (2000, 2 for each member
# of population) to find allele frequency overall
# 3. Used HW formula to find expected values of each: AA = p2, Aa = 2pq
# aa = q2. Multiplied expected genotype frequency by # in pop (1000)
# to find expected # in pop with that genotype
# 4. Popped those numbers in a matrix with corresponding observed
# frequency in malaria vs nonmalaria samples
# 5. Performed chisq tests on those two groups individually
# 6. 1-pchisq using X^2 and df = 2



# list of expected HW values: malaria AA, nonmalaria AA, mal Aa, ...
hw.values <- c(456, 801, 439, 188, 106, 11)


# matrices of observed values and expected values
malaria.hw <- c(456, 400, 439, 550, 106, 50)
nonmalaria.hw <- c(800, 801, 190, 188, 10, 11)

matrix.mal <- matrix(malaria.hw, nrow = 2, ncol = 3)
matrix.mal
matrix.nonmal <- matrix(nonmalaria.hw, nrow = 2, ncol = 3)

chisq.mal <- chisq.test(matrix.mal)
chisq.mal
# is significant, meaning this pop is not at HW equilib

chisq.nonmal <- chisq.test(matrix.nonmal)
chisq.nonmal
# is not significant, meaning this pop may be at HW equilib

?pchisq

#malaria areas
1-pchisq(36.224, df = 2)
# p=1.38 e-8; very small p-value, reject Ho that pop is at HW equilib

#non-malaria areas
1-pchisq(0.0588, df = 2)
# p=0.97; not at all a small p-value, accept Ho that pop is at HW equilib
