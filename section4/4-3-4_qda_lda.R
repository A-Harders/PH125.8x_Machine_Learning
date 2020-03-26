#config
library("caret")
data("mnist_27")

# as we have two predictors we assume their conditional distribution is bivariate normal
# we need to estimate two means, two SDs, and correlation to approximate conditional distributions
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), sd_1 = sd(x_1), sd_2 = sd(x_2), r = cor(x_1, x_2))
params

# we can also visualise the approach, plotting the data and contour plots
mnist_27$train %>% mutate(y=factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type ="norm", lwd = 1.5)
    # this shows a curve representing a region including 95% of the points
    # giving an idea of what the two normal densities look like

# once we have the distributions we can fit the model to obtain the predictors
train_qda <- train(y~., method = "qda", data = mnist_27$train)
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

# the conditional probability isnt as good as the kernel smoothers, perhaps our assumption of normality doesnt hold
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm") +
  facet_wrap(~y)

# as the number of parameters increases it becomes unpractical to compute all correlations
# a simple solution to having too many parameters is to assume a correlation structure across classes
    # we can use one pair of SDs and one correlation
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), sd_1 = sd(x_1), sd_2 = sd(x_2), r = cor(x_1, x_2))

params <- params %>% mutate(sd_1 = mean(sd_1), sd2 = mean(sd_2), r=mean(r))

# forcing this assumption aligns the boundary, just as with logistic regression
# the method is called Linear Discriminant Analysis (LDA) due to this
train_lda <- train(y~., method="lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(dat = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
