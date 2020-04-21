#config
library(tidyverse)
library(dslabs)
library(caret)
data("olive")
olive %>% as_tibble()
table(olive$region)

# we are trying to predict the region an olive is grown based on its fatty acid content, 
# so we remove the area column
olive <- select(olive, -area)

# lets find the accuracy of using kNN
fit <- train(region ~., method = "knn", tuneGrid = data.frame(k = seq(1,15,2)), data = olive)
ggplot(fit)

# 97% accuracy is good but by doing some data exploration we can see the distribution of predictors
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())
    
    # we see that one fatty acid is only present in southern italy
    olive %>% gather(fatty_acid, percentage, -region) %>%
      filter(fatty_acid == "eicosenoic") %>%
      ggplot(aes(region, percentage, fill = region)) +
      geom_boxplot()
    
    # we see another 2 that spearate Northern Italy from Sardinia
    olive %>% gather(fatty_acid, percentage, -region) %>%
      filter(fatty_acid %in% c("linoleic","oleic")) %>%
      ggplot(aes(region, percentage, fill = region)) +
      geom_boxplot() +
      facet_wrap(~fatty_acid, scales = "free") +
      theme(axis.text.x = element_blank())

# using the distributions we can plot a prediction rule (by eye)
p <- olive %>%
  ggplot(aes(eicosenoic, linoleic, color = region)) +
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) +
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)

# we use the 2008 poll data to explain the continuous outcome algoirthm regression tree
# we estimate the conditional expectation with the poll margin and the day
data("polls_2008")
qplot(day, margin, data= polls_2008)

# we can split the data using the rpart() function: further info in the word doc
library(rpart)
fit <- rpart(margin ~., data = polls_2008)

# we can visualise the splits to understand how the rpart function derived the regression tree
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

polls_2008 %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col = "red")

# by setting the CP to 0 and the minsplit to 2 wewe split the tree all the way down to the original data
fit <- rpart(margin~., data = polls_2008, control = rpart.control(cp = 0, minsplit=2))
polls_2008 %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col ="red")

# we can prune the tree by deifning a cp criterion, making the model ignore those outside the CP
pruned_fit <- prune(fit, cp = 0.01)
polls_2008 %>%
  mutate(y_hat = predict(pruned_fit)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# we can use cross-validation to choose the best CP.
# we plot it and then pick the best result, just like with any other tuning parameter
train_rpart <- train(margin ~., method = "rpart", tuneGrid = data.frame (cp=seq(0,0.05, len=25)), data = polls_2008)
ggplot(train_rpart)

# we can access the final model and plot it in
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex =0.75)
polls_2008 %>%
  mutate(y_hat = predict(train_rpart)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")


