#config
library(dslabs)
library(caret)
library(tidyverse)

# load the data source from the 2008 election
data("polls_2008")

# local weighted regression, assumes that the function is locally linear
# that is that a curve, if looked at close enough, is locally straight
# it allows us to consider larger windows
total_days <- diff(range(polls_2008))
span <- 21/total_days # we can also experiment with how different window sizes lead to different estimates

fit <- loess(margin ~ day, degree = 1, span = span, data=polls_2008)

polls_2008 %>%
  mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = 0.5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")

# the geom_smooth() function uses loess, but they are rarely optimal
polls_2008 %>%
  ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth()

# the geom_smooth() function can be modified though
polls_2008 %>%
  ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(color = "red", span = 0.15, method.orgs = list(degree =1))
