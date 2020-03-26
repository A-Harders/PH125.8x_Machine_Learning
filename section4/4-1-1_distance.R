# config
library(tidyverse)
library(dslabs)

# we can look at the distance between 2 and 7 from our previous example
if(!exists("mnist")) mnist <- read_mnist()
set.seed(1995)
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

x <- mnist$train$images[ind,] # predictors
y <- mnist$train$labels[ind] # features

# we will focus on the first 3 observations
y[1:3] # note the labels
x_1 <- x[1,] # first object (2)
x_2 <- x[2,] # second object (7)
x_3 <- x[3,] # third object (2)

# we anticipate the difference between the same numbers to be smaller than the different numbers
sqrt(sum((x_1-x_3)^2)) # comparing 2 & 2
sqrt(sum((x_1-x_2)^2)) # comparing 2 & 7
sqrt(sum((x_2-x_3)^2)) # comparing 7 & 2

    # using matrix algebra we can actually do this faste by using the cross-product
    sqrt(crossprod(x_1-x_3)) # comparing 2 & 2
    sqrt(crossprod(x_1-x_2)) # comparing 2 & 7
    sqrt(crossprod(x_2-x_3)) # comparing 7 & 2
    
# we can calculate the distance between all observations relatively quickly by using dist()
d <- dist(x)
    
    # note that it makes its own object class of dist to denote this
    class(d)

# to access the entries using row and column indices, we can coerce the dist into a matrix
as.matrix(d)[1:3,1:3]

# we can also see an image of these distances using the image() function
image(as.matrix(d))

    # we can order by our labels we can see the variance in an image
    image(as.matrix(d)[order(y), order(y)])
    # red squares indicate digits that are the same are closer together
    # there appears to be more uniformity in the 7s as they appear more red in the graph

# we can also compute distance between predictors
# note: we need to transpose before calculating the dist() of the observations
d_1 <- dist(t(x))
dim(as.matrix(d_1)) # we get a 784 x 784 matrix

# if we pick a predictor we can see which other predictors are close to it
# if we pick a pixel, we can see which pixels are ink or not ink together
d_p_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_p_492, 28, 28))
