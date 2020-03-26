#config
library(tidyverse)
library(dslabs)

# every pixel records a number between 0 and 255
if(!exists("mnist")) mnist <- read_mnist()

# it is often more convenient to save predictors in a matrix and the outcomes in a vector
class(mnist$train$images)

# the original dataset is 60000 digits, so we will use a more manageable subset, of 1000 predictors and the first 1000 labels
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]
