# the mnist dataset is very common among machine learning competitions, we can get it below
library(dslabs)
library(caret)
mnist <- read_mnist()

# it consists of predefined train and test sets
names(mnist)

# each components includes a matrix with features in columns
dim(mnist$train$images)

# it includes a vector with the classes as integers
class(mnist$train$labels)
table(mnist$train$labels)

# as we want this to run on low powered machines in under an hour, we take a subset
set.seed(123, sample.kind = "Rounding")
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$train$images), 1000)
x_test <- mnist$train$images[index,]
y_test <- factor(mnist$train$labels[index])
