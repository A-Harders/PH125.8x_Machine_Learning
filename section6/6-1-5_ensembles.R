# config
# run 6-1-1
library(tidyverse)
library(caret)
library(dslabs)

# this will also take a few minutes
fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

fit_knn <- knn3(x[,col_index], y, k=3)

# ensembling is combining the results of different algorithms
# computing new class probabilities by taking the average of the class probabilities
p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)
