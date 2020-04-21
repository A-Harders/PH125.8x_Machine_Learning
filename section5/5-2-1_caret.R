#config
library(tidyverse)
library(dslabs)
library(caret)
data("mnist_27")

# the caret package enables us to train diffrent models using a consistent method
train_glm <- train(y~., method = "glm", data = mnist_27$train)
train_knn <- train(y~., method = "knn", data = mnist_27$train)

# it also gives us a standardised way to make predictions based on those models
y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

# we can also quickly interpret the predictions using the included confusionMatrix function
confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]