# config
library(tidyverse)
library(dslabs)
library(matrixStats)
if(!exists("mnist")) mnist <- read_mnist()
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

# see notebook for arithmetic explanation
# the below code can be used to scale each row of a matrix
(x - rowMeans(x)) / rowSds(x)

# for columns it the arithmetic doesnt work
# we need to transpose the matrix first and then transpose it back after the function
t( t(x) - colMeans(x))

# we can also use the function sweep() that works similarly to apply()
X_mean_0 <- sweep(x, 2, colMeans(x))

# the default oepration is subtract but we can tell sweep() to do other operations
x_standardised <- sweep(X_mean_0, 2, colSds(x), FUN = "/")

# we dont learn the mathematics of matrix multiplication here but here are the opertors
    # to do the cross product we use the %*% opertor
    t(x) %*% x
    
    # or we can use the cross product function
    crossprod(x)
    
    # to find the inverse of a matrix we use solve()
    solve(crossprod(x))
    
    # we can find the qr decoposition by using qr()
    qr(x)
    