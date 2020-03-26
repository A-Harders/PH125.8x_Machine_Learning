#config
library(caret)
library(dslabs)
library(tidyverse)
library(gridExtra)
library(plotrix)
set.seed(1995, sample.kind="Rounding")

indexes <- createResample(mnist_27$train$y, 10)

# Q1 - how many times do 3, 4, and 7 appear in Resample01?
sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

# Q2 - how many times does 3 occur across all resamples?
sum(indexes[[1]] == 3) +
  sum(indexes[[2]] == 3) +
  sum(indexes[[3]] == 3) +
  sum(indexes[[4]] == 3) +
  sum(indexes[[5]] == 3) +
  sum(indexes[[6]] == 3) +
  sum(indexes[[7]] == 3) +
  sum(indexes[[8]] == 3) +
  sum(indexes[[9]] == 3) +
  sum(indexes[[10]] == 3)

x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

# Q3 - create a monte carlo of 10,000 generating a random dataset and estimating the 75th quantile; what is the expected value and SE
set.seed(1, sample.kind="Rounding")
B <- 10^5
MC <- replicate(B, {
  y <- rnorm(100, 0 ,1)
  quantile(y, 0.75)
})

mean(MC)
sd(MC)

# Q4 - we cant use a monte carlo in practice, use 10 bootstraps to find the expected value and se
set.seed(1, sample.kind="Rounding")
y <- rnorm(100,0,1)
N <- 100
B <- 10

set.seed(1, sample.kind="Rounding")
M_star <- replicate(B, {
  X_star <- sample(y, N, replace=TRUE)
  quantile(X_star, 0.75)
})

mean(M_star)
sd(M_star)

# Q5 - we cant use a monte carlo in practice, use 10,000 bootstraps to find the expected value and se
set.seed(1, sample.kind="Rounding")
y <- rnorm(100,0,1)
N <- 100
B <- 10^5

set.seed(1, sample.kind="Rounding")
M_star <- replicate(B, {
  X_star <- sample(y, N, replace=TRUE)
  quantile(X_star, 0.75)
})

mean(M_star)
sd(M_star)
