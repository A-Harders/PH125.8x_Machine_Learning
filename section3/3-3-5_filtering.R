# config
library(tidyverse)
library(dslabs)
library(matrixStats)
if(!exists("mnist")) mnist <- read_mnist()
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

# each column represents a pixel, so we use the colSds() function to find the SD of each pixel
sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))
image(1:28, 1:28, matrix(sds, 28,28)[,28:1])

# we know how the extract columns of a matrix from an earlier course
x[,c(351,352)] # 351st and 352nd columns and rows
x[c(2,3),] # 2nd and 3rd rows

# we can use logical indices to determine which columns or rows to keep
new_x <- x[ ,colSds(x) > 60] # keeps columns with SD greater than 60
dim(new_x)

# NOTE: if you select a single column or row from a matrix, it is no longer a matrix it is a vector
class(x[,1])
dim(x[1,])

# operations on matrices may result in vectors, in which case the drop argument can be used to preserve the class
class(x[, 1, drop=FALSE])
dim(x[1, , drop=FALSE])
