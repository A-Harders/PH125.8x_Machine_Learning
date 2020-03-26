# config
library(caret)
library(dslabs)
library(tidyverse)
library(gridExtra)
set.seed(1995, sample.kind="Rounding")

# we have the income distribution of a population, and the median income is ~45k
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m

# we dont have the entire populations worth of data, just a sample of 250
N <- 250
X <- sample(income, N)
M <- median(X)
M

#Key Questions
    # Can we construct a confidence interval
    # whats the distribution of the sample median

# using a monte carlo simulation we see the distribution of the sample median is approximately normal (which is to be expected)
B <- 10^4
Ms <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(Ms, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(Ms)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(Ms)
sd(Ms)
    # the issue is that in practice we dont have access tot he distribution
    # in the past we used the central limit theorem, but the CLT applies to averages not medians

# bootstrap permits us to approximate a Monte Carlo simulation without access to the entire distribution
# we can see how close by using the bootstrap vs's our monte carlos results
B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

tibble(monte_carlo = sort(Ms), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) +
  geom_abline()

    # we can also look at the quantiles to find the confidence interval
    quantile(Ms, c(0.05, 0.95))
    quantile(M_star, c(0.05, 0.95))

# compared to the CLT the bootstrap model is far more accurate
median(X) + 1.96*sd(X)/sqrt(N)*c(-1,1)

# if the distribution is normal, we can use the bootstrap to esitmate the mean and se, to then build a confidence interval
mean(Ms) + 1.96*sd(Ms)*c(-1,1)
mean(M_star) + 1.96*sd(M_star)*c(-1,1)
