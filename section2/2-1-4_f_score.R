# config
library(caret)
library(dslabs)
library(tidyverse)
data(heights)

y <- heights$sex
x <- heights$height

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
head(test_index)

train_set <- heights[-test_index,]
test_set <- heights[test_index, ]

# we can maximise our result using the F-score instead of the overall accuracy
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

# we can graph out our F-score like we did in 2-1-1
tibble(cutoff, F_1) %>%
ggplot(aes(cutoff, F_1)) +
  geom_point() +
  geom_line()

# we can also see how changing the cutoff to the best F-score 
# balances the specificty and sensitivity of our confusion matrix
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat, reference = test_set$sex)
