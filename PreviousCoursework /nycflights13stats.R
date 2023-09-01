
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))
suppressPackageStartupMessages(library(nycflights13))

##1
upperO <- quantile(flights$dep_delay, .997, na.rm = TRUE)
lowerO <- quantile(flights$dep_delay, .003, na.rm = TRUE)
upperO
lowerO
outliers <- which(flights$dep_delay > upperO | flights$dep_delay < lowerO)
noOflights <- flights[-outliers,]
Q1 <- (nrow(flights) - length(outliers)) / nrow(flights) *100
Q1

##2
Q2 <- cor.test(noOflights$dep_delay, noOflights$distance)
Q2

##3
modelflights <- lm(noOflights$dep_delay ~ noOflights$distance, dat = noOflights)
Q3 <- summary(modelflights)
Q3

##4
Q4 <- lm.beta(modelflights)
Q4

##5
#logistic regression you dummy
