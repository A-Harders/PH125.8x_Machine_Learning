# config
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

factor(y)

# Q1 - create test and training sets
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE) # this is the answer
test <- iris[test_index,]
train <- iris[-test_index,]

max(test$Petal.Length)

# Q2 - find which feature yields the greatest overall accuracy when predicitng species
  # my answer - cycling through the different features
  intervals <- seq(min(iris$Petal.Length), max(iris$Petal.Length),0.1)
  accuracy <- map_dbl(intervals, function(x){
    y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
      factor(levels = levels(train$Species))
    mean(y_hat == train$Species)
  })
  
  intervals[which.max(accuracy)]

  # the correct answer according to EDX, which calculates all at once
  foo <- function(x){
    rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
    sapply(rangedValues,function(i){
      y_hat <- ifelse(x>i,'virginica','versicolor')
      mean(y_hat==train$Species)
    })
  }
  
  predictions <- apply(train[,-5],2,foo)
  sapply(predictions,max)	

# Q3 - use the smart cutoff value to calculate the overall accuracy in the test
  # my answer - cycling through the different features
  y_hat <- ifelse(test$Petal.Length > intervals[which.max(accuracy)], "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
  
  # the correct answer according to EDX, which calculates all at once
  predictions <- foo(train[,3])
  rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
  cutoffs <-rangedValues[which(predictions==max(predictions))]
  
  y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
  mean(y_hat==test$Species)

# Q4 - we may have overtrain our model, which feature in our test data is best for overall accuracy
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}

predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)	

# Q5 - perform exploratory data analysis
plot(iris,pch=21,bg=iris$Species)

  # we notice that Petal.Length & Petal.Width in combination could be more informative than separately
  # Optimise our model using training data and find the overall accuracy of the test

    # Petal.Length best cutoff
    intervals <- seq(min(iris$Petal.Length), max(iris$Petal.Length),0.1)
    accuracy <- map_dbl(intervals, function(x){
      y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
        factor(levels = levels(train$Species))
      mean(y_hat == train$Species)
    })
    
    cutoff_P_Length <- intervals[which.max(accuracy)]
  
    # Petal.Width best cutoff
    intervals <- seq(min(iris$Petal.Width), max(iris$Petal.Width),0.1)
    accuracy <- map_dbl(intervals, function(x){
      y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
        factor(levels = levels(train$Species))
      mean(y_hat == train$Species)
    })
    
    cutoff_P_Width <- intervals[which.max(accuracy)]

  # Create model
    y_hat <- ifelse(test$Petal.Length > cutoff_P_Length, "virginica", 
                    ifelse(test$Petal.Width > cutoff_P_Width, "virginica", "versicolor")) %>%
      factor(levels = levels(test$Species))
    mean(y_hat == test$Species)
    
  # EDX Answer
    petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
    petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)
    
    length_predictions <- sapply(petalLengthRange,function(i){
      y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
      mean(y_hat==train$Species)
    })
    length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7
    
    width_predictions <- sapply(petalWidthRange,function(i){
      y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
      mean(y_hat==train$Species)
    })
    width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5
    
    y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
    mean(y_hat==test$Species)
    