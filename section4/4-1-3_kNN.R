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

# first we need to set the standard to which we are going to compare knn to
# we will use logistic regression
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

# we should look at the knn3() function reference file for note on what is capable
?knn3

# we notice two ways to call the knn3() function
    # first is the formula dataframe method
    knn_fit <- knn3(y~., data = mnist_27$train, k=5)
    
    # second is to call a matrix of predictors and the vector of outcomes
    x <- as.matrix(mnist_27$train[,2:3])
    y <- mnist_27$train$y
    knn_fit <- knn3(x, y, k=5)
    
    # the default k number is 5 but we can write it explicitly in the formulas
    
# the predict() function can produce either the probability for each class or the outcome with the highest probability
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class") # class gives the actual outcome

#now that we have this we can compute the accuracy using a confusion matrix
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]