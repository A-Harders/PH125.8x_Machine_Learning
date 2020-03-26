# config
library(tidyverse)
library(dslabs)
library(caret)

# load the test data set
data("mnist_27")

# we can plot these using ggplot
mnist_27$train %>%
  ggplot(aes(x_1, x_2, color = y)) +
  geom_point()

# we can try a logistic regression function to predict 2s and 7s
fit <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")

# when it is larger than 0.5 it predicts 7, otherwise it predicts 2
p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
confusionMatrix(data = y_hat, reference = mnist_27$test$y)

# we compare our results to the true conditional probabilities
mnist_27$true_p %>%
  ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()

  # we imporve the graph by changing the colours and adding a curve, separating x_1 & x_2
  mnist_27$true_p %>%
    ggplot(aes(x_1, x_2, fill=p, z=p)) +
    geom_raster() +
    scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
    stat_contour(breaks=c(0.5), color="black")

# we can compare the above with our prediction by building a boundary at 0.5 (where we start to predict 7 instead of 2)
p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2, z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black")

# we see that our boundary is a line, and will miss several points that a curve would capture
p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() + 
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color = y), data = mnist_27$test)
