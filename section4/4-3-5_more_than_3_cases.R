# config
library("caret")
library("tidyverse")

# we need to generate a data set with the 3 classes (1, 2, 7)
if(!exists("mnist"))mnist <- read_mnist()

set.seed(3456, sample.kind = "Rounding")
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127]
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)

# then we need to get the quadrants
row_column <- expand.grid(row=1:28, col=1:28)

    # create a temporary object to figure out the quadrants
    upper_left_ind <- which(row_column$col <= 14 &
                              row_column$row <= 14)
    lower_right_ind <- which(row_column$col > 14 &
                               row_column$row > 14)    
    x <- x >200    
   
# binarise the values, above 200 is ink, below is now ink
x <- cbind(rowSums(x[ , upper_left_ind])/rowSums(x),
           rowSums(x[ , lower_right_ind])/rowSums(x))

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])
test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])

# we can visualise the training set
train_set %>%
  ggplot(aes(x_1, x_2, color = y)) +
  geom_point()

# now we can train a qda model
train_qda <- train(y~., method = "qda",
                   data = train_set)

# so what changed?
    # first we estimated three conditional probabilities in a matrix
    predict(train_qda, test_set, type = "prob") %>% head()
    
    # second the standard predict function shows what it predicts
    predict(train_qda, test_set) %>% head()

    # third the confusion matrix is now 3x3
    confusionMatrix(predict(train_qda, test_set), test_set$y)

    # finally we can visualise the regions are called 1s, 2s and 7s
    train_set %>% mutate(y = factor(y)) %>%
      ggplot(aes(x_1, x_2, fill = y, color=y)) +
      geom_point(show.legend = FALSE) +
      stat_ellipse(type="norm")

# now we can train it as lda
train_lda <- train(y~., method = "lda",
                    data = train_set)

    # we can compare the confusion matrix
    confusionMatrix(predict(train_lda, test_set), test_set$y)

# then we can compare the outcome of KNN
train_knn <- train(y~., method = "knn", tuneGrid = data.frame(k = seq(15,51,2)),
                   data = train_set)

    # we can compare the confusion matrix
    confusionMatrix(predict(train_knn, test_set), test_set$y)

# then we can compare the accuracy of all 3 methods:
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]
