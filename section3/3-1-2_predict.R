#config
library(tidyverse)
library(HistData)
set.seed(1983)

galton_heights <-GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

fit <- lm(son~father, data = train_set)

#instead of writing out the forumla for a regression line, we can use the predict() function
y_hat <- predict(fit,test_set)
mean((y_hat - test_set$son)^2)

#we can see that we gett the same result as using the regression line formula
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

#predicT() does not always return objects of the same type, for specifics we need to reference the help files
?predict.glm
?predict.lm