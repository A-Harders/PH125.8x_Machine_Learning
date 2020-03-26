# config
library(caret)
library(dslabs)
library(tidyverse)
data(heights)

# we start by defining our predictor and expected outcome
# we only have heights and as predictor and sex as outcome
y <- heights$sex
x <- heights$height

# we can use the createDataPartition() function to split our data into test or training
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
head(test_index)

# we can use the index to define the training and test set
train_set <- heights[-test_index,]
test_set <- heights[test_index, ]

# we will define 2 algorithms and evaluate their overall accuracy
  # the first is purely guessing the sex
  y_hat <- sample(c("Male", "Female"),
                  length(test_index), replace = TRUE)
  mean(y_hat == test_set$sex)

  # the second is a ML algorithm using the outcomes (set as a factor)
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
           factor(levels = levels(test_set$sex))
  mean(y_hat == test_set$sex)
  
# on both our models we get around 50% because we are guessing
# but can we make it better by doing exploratory data analysis?
heights %>% group_by(sex) %>%
  summarize(mean(height), sd(height))

# we see that males are on average taller than females, but how do we make use of this?
# simply we could predict "male" if the height is within 2 SD of the average male height
y_hat <- ifelse(x > 62, "Male","Female") %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# we can examine the accuaracy from different cutoffs also, and take the value that provides the best result
# caution needs to be taken, as this can lead to overfitting, but with our simple exampke it isnt an issue
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

  # we can graph out our best accuracy over our cutoff sequence
  tibble(cutoff, accuracy) %>%
    ggplot(aes(cutoff, accuracy)) +
    geom_point() +
    geom_line()

  # we can find the best accuracy has a much better overall accuracy than guessing
  max(accuracy)
  
  # using the max accuracy we can find the best cutoff
  best_cutoff <- cutoff[which.max(accuracy)]
  best_cutoff
  
  # and apply it to our formula
  y_hat <- ifelse(x > 65, "Male","Female") %>%
    factor(levels = levels(test_set$sex))
  y_hat <- factor(y_hat)
  mean(y_hat == test_set$sex)