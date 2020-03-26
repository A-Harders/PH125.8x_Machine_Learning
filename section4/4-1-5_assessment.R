#config
library(caret)
library(dslabs)
library(tidyverse)
library(dplyr)
library(e1071)
options(digits=6)

# Q1 - set seed to 1, split heights data into training and test, sapply knn function and calculate f1 scores
data="heights"
set.seed(1, sample.kind="Rounding")
ks <- seq(1, 101, 3)
index <- createDataPartition(heights$sex, times=1, p=0.5, list=FALSE)
test <- heights %>% slice(index)
train <- heights %>% slice(-index)

f_1 <- map_df(ks, function(k){
  fit <- knn3(sex~height, data=train, k=k)
  
  y_hat <- predict(fit, test, type ="class")
  train_error <- F_meas(data = y_hat, reference = test$sex)
  
  tibble(train = train_error, k = k)
})

f_1 %>%
  mutate(ks = ks) %>%
  arrange(desc(train))

    #EDX Correct Answer
    set.seed(1)
    test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
    test_set <- heights[test_index, ]
    train_set <- heights[-test_index, ]     
    
    ks <- seq(1, 101, 3)
    F_1 <- sapply(ks, function(k){
      fit <- knn3(sex ~ height, data = train_set, k = k)
      y_hat <- predict(fit, test_set, type = "class") %>% 
        factor(levels = levels(train_set$sex))
      F_meas(data = y_hat, reference = test_set$sex)
    })
    plot(ks, F_1)
    max(F_1)
    ks[which.max(F_1)]
    
# Q2 - find the accuracies of the tissue_gene_expression
library(purrr)
data(tissue_gene_expression)
set.seed(1)
df <- as.data.frame(tissue_gene_expression)

index <- createDataPartition(df$y, times=1, p=0.5, list=FALSE)
test <- df[index,]
train <- df[-index,]

ks <- seq(1,11,2)

accuracy <- map_df(ks, function(k){
  fit <- knn3(y~., train, k=k)
  y_hat <- predict(fit, test, type ="class")
  error <- confusionMatrix(y_hat, test$y)$overall["Accuracy"]

 tibble(ks=k,accuracy = error)  
})

accuracy

    #EDX Correct Answer
    set.seed(1)
    y <- tissue_gene_expression$y
    x <- tissue_gene_expression$x
    test_index <- createDataPartition(y, list = FALSE)
    sapply(seq(1, 11, 2), function(k){
      fit <- knn3(x[-test_index,], y[-test_index], k = k)
      y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                       type = "class")
      mean(y_hat == y[test_index])
    })
    