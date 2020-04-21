# config
library(tidyverse)
library(caret)
library(dslabs)

set.seed(1, sample.kind = "Rounding")
data("mnist_27")

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

# train the mnist model with the 10 models
fits <- lapply(models, function(model){
  print(model)
  train(y~., method = model, data = mnist_27$train)
})

names(fits) <- models

# Q2 - create a matrix of predictions, the matrix should be 200 rows and 10 columns
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)
pred

# Q3 - report the mean accuracy across all models
glm_acc <- sum(ifelse(pred[,1]==mnist_27$test$y, 1, 0))/200 # glm
lda_acc <-sum(ifelse(pred[,2]==mnist_27$test$y, 1, 0))/200 # lda
nb_acc <-sum(ifelse(pred[,3]==mnist_27$test$y, 1, 0))/200 # naive_bayes
sl_acc <-sum(ifelse(pred[,4]==mnist_27$test$y, 1, 0))/200 # svmLinear
knn_acc <-sum(ifelse(pred[,5]==mnist_27$test$y, 1, 0))/200 # knn
gam_acc <-sum(ifelse(pred[,6]==mnist_27$test$y, 1, 0))/200 # gamLoess
mul_acc <-sum(ifelse(pred[,7]==mnist_27$test$y, 1, 0))/200 # multinom
qda_acc <-sum(ifelse(pred[,8]==mnist_27$test$y, 1, 0))/200 # qda
rf_acc <-sum(ifelse(pred[,9]==mnist_27$test$y, 1, 0))/200 # rf
ada_acc <-sum(ifelse(pred[,10]==mnist_27$test$y, 1, 0))/200 # adaboost
sum(glm_acc, lda_acc, nb_acc, sl_acc, knn_acc, gam_acc, mul_acc, qda_acc, rf_acc, ada_acc)/10
    #EDX CORRECT ANSWER
    acc <- colMeans(pred == mnist_27$test$y)
    acc
    mean(acc)

# Q4 - build an ensemble prediction by majority vote, compute the accuracy
row_acc <- rowMeans(pred == 2)
p_mod <- ifelse(row_acc > 0.5, 2,7)
y_hat <- confusionMatrix(factor(p_mod), mnist_27$test$y)$overall["Accuracy"]
y_hat

# Q5 - which individual methods perform better than the ensemble
ind <- acc > mean(p_mod == mnist_27$test$y)
sum(ind)
models[ind]

# Q6 - obtain the minimum accuracy estimates, report the mean
reAcc <- sapply(fits, function(object){
  mean(object[["resample"]][["Accuracy"]])
})
mean(reAcc)
    #EDX CORRECT ANSWER
    acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
    mean(acc_hat)

# Q7 - only use methods with an estimated accuracy of > 0.8
ind <- reAcc >= 0.8
mean(reAcc[ind])
    #EDX CORRECT ANSWER
    ind <- reAcc >= 0.8
    votes <- rowMeans(pred[,ind] == "7")
    y_hat <- ifelse(votes>=0.5, 7, 2)
    mean(y_hat == mnist_27$test$y)