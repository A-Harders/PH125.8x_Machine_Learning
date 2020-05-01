# config
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

# create the test and training sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

# q9 - check the proportions of the train and test sets
    # q9a - what is the proportion of benign in the train set
    mean(train_y == "B")
    
    # q9b - what is the proportion of benign in the test set
    mean(test_y == "B")

# q10 - the predict_kmeans() takes two arguments, the matrix and k-means object
    predict_kmeans <- function(x, k) {
      centers <- k$centers    # extract cluster centers
      # calculate distance to cluster centers
      distances <- sapply(1:nrow(x), function(i){
        apply(centers, 1, function(y) dist(rbind(x[i,], y)))
      })
      max.col(-t(distances))  # select cluster with min distance to center
    }
    
    # q10a - perform k-means on the train set and predict the overall accuracy
    k <- kmeans(train_x, centers = 2)
    prediction12 <- as.factor(predict_kmeans(test_x, k))
    predictionBM <- as.factor(ifelse(prediction12 == 1, "B", "M"))
    confusionMatrix(test_y, predictionBM)

    