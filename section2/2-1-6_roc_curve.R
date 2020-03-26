# config
library(caret)
library(dslabs)
library(tidyverse)
library(ggrepel)
data(heights)

y <- heights$sex
x <- heights$height

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
head(test_index)

train_set <- heights[-test_index,]
test_set <- heights[test_index, ]

# we can see the bias of the data set
# by just increasing the guess rate of Male we can increase the accuracy
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(p,1-p)) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# we can build a ROC curve based on the proportion of Males picked
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

# we can build an ROC curve for the height to compare specificity vs sensitivity
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
height_cutoff %>% qplot(FPR, TPR, data = ., xlab = "1 - Specificity", ylab = "Sensitivity")

# and by plotting them together on the same graph we can display the overall advantage of one method over the other
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

# when using ROC curves it is good practice to add the cutoffs used to the points
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# ROCs dont show measures that depend on prevalence 
# we use precision recall plots to show situations where prevelance matters
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(p, 1-p)) %>%
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point() +
  xlab("recall") +
  ylab("precision")
