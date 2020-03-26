#to quickly show the connection between linear regression and ML we reformulate the Galten study
library(tidyverse)
library(HistData)
set.seed(1983)

galton_heights <-GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#we are tasked with building a ML algorithm that predicts the son's height using the fathers
#lets start by generating some test and traing sets
library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

#if we were ignoring the height, and guessing the height, our prediction would be the average
avg <- mean(train_set$son)
avg
  #our squared loss can be calculated, and we need to see if we can do better
  mean((avg - test_set$son)^2)

#if a pair xy follows a bivariate normal distributions, the conditional expectation is equivelant to the regression line
#we also use the least squares method for estimating the slope and intercept
fit <- lm(son~father, data = train_set)
fit$coef

#by calculating the squared loss we can see which method is better, guessing or LR
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)