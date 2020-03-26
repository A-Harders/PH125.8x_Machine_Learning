# config
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

# the rowSums() function takes a matrix and sums the value by row
sums <- rowSums(x)
sums

# the rowMeans() function takes a matrix and means the value by row
means <- rowMeans(x)
means

# we can use a box plot to show how the pixel intensity changes from digit to digit
data_frame(labels = as.factor(y), row_averages = means) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

# the apply function allows us to apply anuy function to a matrix
  # the below are a copy of the rowMeans() and rowSums() functions
apply(x, 1, mean)
apply(x, 1, sum)
