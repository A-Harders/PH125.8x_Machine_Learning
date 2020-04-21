#config
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1, sample.kind = "Rounding") #set.seed(1, sample.kind = "Rounding") if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# Q1 - which code uses rpart() to fit a regression tree and saves the result to fit?
fit <- rpart(y~., data = dat)

# Q2 - print the tree
plot(fit)
text(fit)

# Q3 - create a scatter plot and the predicted values of the fit
dat %>% mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(x,y)) +
  geom_step(aes(x, y_hat), col="red")

# Q4 - run randomForest() toand remake the scatter plot to show the difference in the results
library(randomForest)
fit <- randomForest(y~x, data = dat)
dat %>% mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

# Q5 - plot the randomForest result to see if it has converged or requires more trees
plot(fit)

# Q6 - the default value for randomForest() is too flexible, change the node size to 50 and max nodes to 25
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
dat %>% mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
    
    # plotted out of curiosity, not test related
    plot(fit)
