#config
library(tidyverse)
library(dslabs)
library(caret)
library(rpart)
data("mnist_27")

# lets see how classification trees perform on 2 or 7
train_rpart <- train(y~., method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len=25)),
                     data = mnist_27$train)
plot(train_rpart)

# we can derive the overall accuracy of our model now also
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
