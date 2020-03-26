#config
set.seed(1, sample.kind="Rounding") # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#Q1 - replicate, partition into test and training dat$y, train lm(y~x), predict(), calc RMSE, report mean and SD
RMSEs <- replicate(100,{
  test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y~x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(RMSEs)
sd(RMSEs)

#Q2 - create a function that builds that data set, runs replicate from above, calculates means & SDs
n <- c(100,500,1000,5000,10000)
lm_different <- function(n) {
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  RMSEs <- replicate(100,{
    test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y~x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  return(mean(RMSEs))
}

set.seed(1, sample.kind="Rounding")

sapply(n, lm_different)

#Q3 - the RMSE does not change much as n gets larger, but the variability decreases

#Q4 - Q1 but with a correlation that is larger
set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
RMSEs <- replicate(100,{
  test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y~x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(RMSEs)
sd(RMSEs)

#Q5 - when increasing the correlation, x has more predictive power and provides a better estimate

#Q6&7 - compare between x_1, x_2, x_1+x_2 - report the lowest RMSE
set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
data.frame() %>% setNames(c("y", "x_1", "x_2"))

test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fit <- lm(y~x_1+x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat - test_set$y)^2))

#Q8 - Q6 but with higher correlation
set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fit <- lm(y~x_1+x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat - test_set$y)^2))
