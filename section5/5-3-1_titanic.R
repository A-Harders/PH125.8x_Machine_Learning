# config
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

options(digits = 3)

# clean the data
titanic_clean <- titanic_train %>% 
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>% # count famliy members
  select(Survived, Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Q1 - create the test and training sets, find the proportion of survivors in the train_set
set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times= 1, p = 0.2, list = FALSE)
test_set <- titanic_clean %>% slice(test_index)
train_set <- titanic_clean %>% slice(-test_index)

test_set
train_set
mean(train_set$Survived == 1)

# Q2 - predict based on guessing who lives or survived on the test set
set.seed(3, sample.kind = "Rounding")
y_hat <- sample(c(1,0), length(test_set$Survived), replace = TRUE)
mean(y_hat == test_set$Survived)

# Q3a - what proportion of train set: 
    # females survived?
    train_set %>%
      filter(Sex == "female") %>%
      summarize(mean(Survived == 1))
    
        # EDX preferred method
        train_set %>%
          group_by(Sex) %>%
          summarize(Survived = mean(Survived ==1)) %>%
          filter(Sex == "female") %>%
          pull(Survived)
    
    # males survived?
    train_set %>%
      filter(Sex == "male") %>%
      summarize(mean(Survived == 1))
    
        # EDX preferred method
        train_set %>%
          group_by(Sex) %>%
          summarize(Survived = mean(Survived ==1)) %>%
          filter(Sex == "male") %>%
          pull(Survived)

# Q3b - predict survival using sex, female survive male dont
y_hat <- test_set %>%
  mutate(y_hat = factor(ifelse(Sex=="female", 1, 0)))
mean(y_hat$y_hat == y_hat$Survived)

    # EDX preferred method
    sex_model <- ifelse(test_set$Sex =="female", 1, 0)
    mean(sex_model == test_set$Survived)    

# Q4a - which passenger classes were more likely to survive than die
train_set %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived == 1))

# Q4b - predict survival using Pclass
class_model <- ifelse(test_set$Pclass == 1, 1, 0)
mean(class_model == test_set$Survived)

# Q4c - which sex combinations were more likely to survive than die
train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1))

# Q4d - predict survival using Sex & Pclass
sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass %in% c(1,2), 1, 0)
mean(sex_class_model == test_set$Survived)

# Q5a - create confusionMatrix for each of our models
confusionMatrix(data = factor(sex_model), reference = test_set$Survived)
confusionMatrix(data = factor(class_model), reference = test_set$Survived)
confusionMatrix(data = factor(sex_class_model), reference = test_set$Survived)
    # Q5b - what is the maximum value of balanced accuracy
    # sex_model confusionMatrix gives us this

# Q6- calculate the F1 scores for each model, which has the highest?
F_meas(data = factor(sex_model), reference = test_set$Survived)
F_meas(data = factor(class_model), reference = test_set$Survived)
F_meas(data = factor(sex_class_model), reference = test_set$Survived)

# Q7 - train the models on LDA & QDA, using Fare as the predictor, report the Accuracy
set.seed(1, sample.kind = "Rounding")
lda_model <- train(Survived~Fare, method = "lda", data = train_set)
lda_y_hat <- predict(lda_model, test_set)
confusionMatrix(lda_y_hat, reference = test_set$Survived)$overall["Accuracy"]

set.seed(1, sample.kind = "Rounding")
qda_model <- train(Survived~Fare, method = "qda", data = train_set)
qda_y_hat <- predict(qda_model, test_set)
confusionMatrix(qda_y_hat, reference = test_set$Survived)$overall["Accuracy"]

# Q8 - train the models on logistic regression, using various predictors, report the Accuracy
set.seed(1, sample.kind = "Rounding")
glm1_model <- train(Survived~Age, method = "glm", data = train_set)
glm1_y_hat <- predict(glm1_model, test_set)
confusionMatrix(glm1_y_hat, reference = test_set$Survived)$overall["Accuracy"]

set.seed(1, sample.kind = "Rounding")
glm2_model <- train(Survived~Sex+Pclass+Fare+Age, method = "glm", data = train_set)
glm2_y_hat <- predict(glm2_model, test_set)
confusionMatrix(glm2_y_hat, reference = test_set$Survived)$overall["Accuracy"]

set.seed(1, sample.kind = "Rounding")
glm3_model <- train(Survived~., method = "glm", data = train_set)
glm3_y_hat <- predict(glm3_model, test_set)
confusionMatrix(glm3_y_hat, reference = test_set$Survived)$overall["Accuracy"]
  ?train

# Q9a - train the model using the training set, using kNN, finding the optimal k
set.seed(6, sample.kind = "Rounding")
knn_model <- train(Survived~., method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3,51,2)))
knn_model$bestTune

# Q9b - plot the kNN model, report the k with the heighst accuracy
ggplot(knn_model, highlight = TRUE)

# Q9c - Find the accuracy of the kNN model on the test set
confusionMatrix(predict(knn_model, test_set, type="raw"), test_set$Survived)$overall["Accuracy"]

    # EDX preferred method
    knn_preds <- predict(knn_model, test_set)
    mean(knn_preds == test_set$Survived)

# Q10 - train a new kNN, using the 10-fold cross-validation, with 10% partitions
set.seed(8, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = 0.9)
knn_model <- train(Survived~., method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3,51,2)),
                   trControl = control)

knn_model$bestTune # find the best k

knn_preds <- predict(knn_model, test_set)
mean(knn_preds == test_set$Survived) # find the accuracy

# Q11a - train a decions tree with rpart, report optimal cp and accuracy
set.seed(10, sample.kind = "Rounding")
rpart_model <- train(Survived~., method="rpart",
                     data = train_set,
                     tuneGrid = data.frame(cp = seq(0,0.05,0.002)))

rpart_model$bestTune # find the best cp
ggplot(rpart_model, highlight = TRUE)

rpart_preds <- predict(rpart_model, test_set)
mean(rpart_preds == test_set$Survived) # find the accuracy

# Q11b - inspect the model and plot the decision tree, which variables were used in the CART
rpart_model$finalModel
plot(rpart_model$finalModel, margin = 0.1)
text(rpart_model$finalModel, cex = 0.75)
    # Q11c - using decision rules generated by the final model, predict whether the below would survive
    # for a fancy decision tree and rules
    library(rattle)
    library(rpart.plot)
    library(RColorBrewer)
    fancyRpartPlot(rpart_model$finalModel)

# Q12 - train a random forest, testing the mtry values from 1 to 7, ntree 100
set.seed(14, sample.kind = "Rounding")
rf_model <- train(Survived~., method="rf",
                  ntree = 100, data = train_set,
                  tuneGrid = data.frame(mtry = seq(1,7)))

rf_model$bestTune # find the best mtry

rf_preds <- predict(rf_model, test_set)
mean(rf_preds == test_set$Survived) # find the accuracy

varImp(rf_model) # find the variable importance (report the top)
