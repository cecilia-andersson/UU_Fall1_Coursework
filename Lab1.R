# Lab 1: Introduction to R
# Cecilia Andersson
# 2022-08-30


## READ IN 'measurements' FILE
setwd("~/Downloads/Lab1")
measurements <- read.table("mesurements.txt", header=T)
# Header=T means the text in the first row is col names (true).
# Header=F would have R set own column names


## PLOTS

# Scatter plot of height and weight

plot(measurements$weight~measurements$height, xlab = "Participant Height",
     ylab = "Participant Weight", main = "Height and Weight Relationship")

# Box plot of age

boxplot(measurements$age, xlab = "x", ylab = "Age",
        main = "Participant Age Spread")

# Line plot of bmi and weight

plot(measurements$weight ~ measurements$bmi, type = "l",
     xlab = "Participant BMI", ylab = "Participant Weight")

## VARIABLE SUMMARIES

# height

summary(measurements$height)

# weight

summary(measurements$weight)

# age

summary(measurements$age)

# bmi

summary(measurements$bmi)


measurements_summary <- data.frame()

## ADDING BMI AS VARIABLE

height_meters <- measurements$height / 100
bmi <- measurements$weight / (height_meters^2)
measurements$bmi <- bmi

