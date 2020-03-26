#config
library("caret")
data("heights")

# we use the example of selecting sex based on height, we generate the sample first
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times =1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# as the normal distribution is a good approximation of the conditional distributions,
# we can approximate the conditional distributions by estimating the mean and SD
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

# the prevelance is the probability Y=1 and is denoted by pi
pi <- train_set %>%
  summarize(pi=mean(sex=="Female")) %>%
  .$pi
pi

# we use our estimates of mean and sd to get the actual rule
x <- test_set$height

  # we get the conditional distributions
  f0 <- dnorm(x, params$avg[2], params$sd[2])
  f1 <- dnorm(x, params$avg[1], params$sd[1])

  # we use Bayes theorem to compute the naive Bayes estimate of the conditional probability
  p_hat_bayes <- f1*pi/(f1*pi+f0*(1-pi))
  
  #it looks similar to a logistics regression
  qplot(x, p_hat_bayes, geom="line")
  