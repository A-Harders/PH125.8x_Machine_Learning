#config
library(tidyverse)
library(dslabs)
library(caret)
library(rpart)
library(randomForest)

# we use the 2008 poll data to demonstrate how we can create a random forest
fit <- randomForest(margin~., data = polls_2008)
plot(fit)

# here we can show the final result of the prediction vs actual data
polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

# we use the 2 or 7 data to demonstrat random forests further
train_rf <- randomForest(y~., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# the resulting tree was too wiggly, so we can sue the caret package to optimise the parameters
fit <- train(y~., method = "Rborist", 
             tuneGrid = data.frame(predFixed = 2, minNode = c(3,50)),
             data = mnist_27$train)

confusionMatrix(predict(fit, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
