# config
library(tidyverse)
library(dslabs)
library(caret)

# we can look at the distance between 2 and 7 from our previous example
if(!exists("mnist")) mnist <- read_mnist()
set.seed(1995)
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

x <- mnist$train$images[ind,] # predictors
y <- mnist$train$labels[ind] # features

knn_fit <- knn3(y~., data = mnist_27$train, k=5)

# to understand overtraining we can look at the accuracy between our train and test sets
    # training set accuracy
    y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
    confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]

    # test set accuracy
    y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
    confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]
  
# k=1 is the most overtrained a model can be, as it only uses a single point to train the model
    # training on k=1
    knn_fit_1 <- knn3(y~., data = mnist_27$train, k=1)
    y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
    confusionMatrix(data = y_hat_knn_1, reference = mnist_27$train$y)$overall["Accuracy"]

    # test on k=1
    knn_fit_1 <- knn3(y~., data = mnist_27$train, k=1)
    y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
    confusionMatrix(data = y_hat_knn_1, reference = mnist_27$test$y)$overall["Accuracy"]

# we some overtraining at k=5 so lets try a much large k, k=401
    # k=1 is the most overtrained a model can be, as it only uses a single point to train the model
    # training on k=1
    knn_fit_401 <- knn3(y~., data = mnist_27$train, k=401)
    y_hat_knn_401 <- predict(knn_fit_401, mnist_27$train, type = "class")
    confusionMatrix(data = y_hat_knn_401, reference = mnist_27$train$y)$overall["Accuracy"]
    
    # test on k=1
    knn_fit_401 <- knn3(y~., data = mnist_27$train, k=401)
    y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
    confusionMatrix(data = y_hat_knn_401, reference = mnist_27$test$y)$overall["Accuracy"]

# for compartive purposes we can map all odd numbers between 3 and 251
# for comparitive purposes we map both training and test sets: this is not correct in practice but is just for comparison
library(purrr)

ks <- seq(3, 251, 2)

accuracy <- map_df(ks, function(k){
  fit <- knn3(y~., data = mnist_27$train, k=k)
  
  y_hat <- predict(fit, mnist_27$train, type ="class")
  train_error <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  test_error <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

# the jaggedness is due to the accuracy being a random variable
accuracy %>%
  mutate(ks = ks) %>%
  ggplot(aes(x=ks)) +
  geom_line(aes(y=train), color = "darkred") +
  geom_point(aes(y=train), color = "darkred") +
  geom_line(aes(y=test), color = "blue") +
  geom_point(aes(y=test), color = "blue")

# the k that maximises accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)
