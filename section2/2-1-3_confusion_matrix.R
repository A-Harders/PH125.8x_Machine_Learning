# config
library(caret)
library(dslabs)
library(tidyverse)
data(heights)

# initialising data sets
y <- heights$sex
x <- heights$height

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights[-test_index,]
test_set <- heights[test_index, ]

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))

# using the table() function, we can create a confusion matrix showing predicted and actual value
table(predicted = y_hat, actual = test_set$sex)

# the confusionMatrix() function computes all the metrics we require
confusionMatrix(data = y_hat, reference = test_set$sex)
