# config
library(tidyverse)
library(dslabs)
library(matrixStats)

# Q1 - make a matrix 100 by 10
x <- matrix(rnorm(100*10), 100, 10)

# Q2 - find the dims
dim(x)
nrow(x) # or dim(x)[1]
ncol(x) # or dim(x)[2]

# Q3 - add the scalar 1 to row 1, scalar 2 to row 2 etc
sweep(x, 1, 1:nrow(x), "+")
x + seq(nrow(x))

# Q4 - add the scalar 1 to col 1, scalara 2 to col 2 etc
sweep(x, 2, 1:ncol(x), FUN = "+")

# Q5 - find the row and col means
rowMeans(x)
colMeans(x)

# Q6 - find the proportion of the of pixels in the grey zone (<50-205>) for the training set of mnist
if(!exists("mnist")) mnist <- read_mnist()
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

x[x>205] <-0
x[x<50] <-0
x[x>1] <-1
mean(rowMeans(x))
