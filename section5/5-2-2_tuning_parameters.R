# config
library(tidyverse)
library(dslabs)
library(caret)
install.packages("gam")

# we can use the following to see which parameters are optimised
getModelInfo("knn")

# the modelLookup() function shows succinct information eregarding the method
modelLookup("knn")

# by running the train() function with default values, we can see the cross validation
train_knn <- train(y~., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)

# we can use the tuneGrid argument to expand beyond the default optimisation values
train_knn <- train(y~., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9,71,2)))
ggplot(train_knn, highlight = TRUE)

    # we can access the parameter that gives the best accuracy
    train_knn$bestTune
    # we can see the result of the most accurate training
    train_knn$finalModel

# cross-validation was done pruely on the training set
# to compare the accuracy on the training and test set we can do the below
  confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                  mnist_27$test$y)$overall["Accuracy"]

# we can use the trainControl() function to modify the train() functions default values
control <- trainControl(method = "cv", number = 10, p = 0.9)
train_knn_cv <- train(y~., method = "knn", data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)  

# train() also provides the SD for each parameter, we can plot Accuaracy vs SD
train_knn$results %>%
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x=k, 
                    ymin=Accuracy - AccuracySD,
                    ymax=Accuracy + AccuracySD))


# we can plot out the border and see it is still quite squiggly
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])

# the gamLoess method is a smooth kernel method that requires 2 parameters
modelLookup("gamLoess")

# we will change span but leave degree unchanged, we must provide a column for it though
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

# now we can train the model using the grid
train_loess <- train(y~.,
                     method = "gamLoess",
                     tuneGrid = grid,
                     data = mnist_27$train)
    # we can see the accuracy of the different spans
    ggplot(train_loess, highlight = TRUE)
    
    # we then select the best-performing and compare to knn
    confusionMatrix(data = predict(train_loess, mnist_27$test),
                    reference = mnist_27$test$y)$overall["Accuracy"]

    # we can see that the border is far smoother, but performs similarly to knn
    plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])