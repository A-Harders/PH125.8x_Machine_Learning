# config
library(tidyverse)
library(dslabs)
library(matrixStats)
if(!exists("mnist")) mnist <- read_mnist()
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

# just as we can turn vectors in matrices, we can turn matrices into vectors
my_vector <- 1:15
mat <- matrix(my_vector, 5, 3)
mat

as.vector(mat)

# we can build a histogram of our predictors by using the vector
qplot(as.vector(x), bins = 30, color = I("black"))

# we see a clear dichotomy of parts with ink and parts without,
# low value regions represent smudges and can be removed with logical operators
new_x <- x
new_x[new_x < 50] <- 0

    # to look at what this does we will use a smaller matrix
    mat <- matrix(1:15, 5, 3)
    mat[mat < 3] <- 0    
    mat    
    
    # we can use more complex operations with matrices also
    mat <- matrix(1:15, 5, 3)
    mat[mat>12] <- 0
    mat
    
# using the dichotomy we can see that the data is mostly binary, ink or no ink
# using logical operators we can binarise the data
bin_x <- x
bin_x[bin_x < 255/2] <- 0 # less than 255/2 are 0
bin_x[bin_x > 255/2] <- 1 # greater than 255/2 are 1

# we can also convert it into a matrix using logicals, and coerce it into numbers
bin_X <- (x > 255/2)*1

# by converting the information into 0s and 1s we dont lose much information
par(mfrow=c(1,3))
image(1:28, 1:28, matrix(x[3,], 28, 28)[,28:1], main="Original")
image(1:28, 1:28, matrix(bin_x[3,], 28, 28)[,28:1], main="Binarised")
image(1:28, 1:28, matrix(bin_X[3,], 28, 28)[,28:1], main="Coerced")