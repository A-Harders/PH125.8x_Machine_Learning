#config
library(tidyverse)
library(dslabs)
library(caret)
library(rpart)

# Q1 - use the train() function to estimate the accuracy, changing the CP to find the most accurate
set.seed(1991, sample.kind = "Rounding")
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

train_rpart <- train(x, y, 
                     method = "rpart", 
                     tuneGrid = data.frame (cp=seq(0,0.1,0.01)))
ggplot(train_rpart)

    # EDX CORRECT ANSWER
    fit_rpart <- with(tissue_gene_expression,
                train(x, y, method = "rpart",
                      tuneGrid = data.frame(cp = seq(0,0.1,0.01)),
                      control = rpart.control(minsplit = 0)))
    ggplot(fit_rpart)                

# Q2 - there are only 6 nodes, change tht minsplit to 0, report the accuracy
set.seed(1991, sample.kind = "Rounding")
fit_rpart <- with(tissue_gene_expression,
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0,0.1,0.01)),
                  control = rpart.control(minsplit = 0)))
confusionMatrix(fit_rpart)

# Q3 - plot the tree from Q2, which gene is at the first split
plot(fit$finalModel, margin = 0.1)
text(fit$finalModel, cex = 0.75)

# Q4 - can we predict the tissue type with fewer than 7 genes? create a Random Forest, what is the mtry that is most accurate
set.seed(1991, sample.kind = "Rounding")
fit_rf <- with(tissue_gene_expression,
               train(x, y, method = "rf", nodesize = 1,
                     tuneGrid = data.frame(mtry = seq(50,200,25))))
plot(fit_rf)
fit_rf$bestTune

# Q5 - varImp() the model
imp <- varImp(fit_rf)
imp

# Q6 - taking the genes from fit_rpart, find them in the random forest model and show their importance
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)
