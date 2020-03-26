#config
library(dslabs)
library(caret)
library(tidyverse)

# load the data source from the 2008 election
data("polls_2008")

# bin smoothers takes the average for every point, forming an estimate of the underlying curve
span <- 7
fit <- with(polls_2008,
            ksmooth(day, margin, x.points = day, kernel = "box", bandwidth = span))

polls_2008 %>% 
  mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = 0.5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")

# by using a weighted average in our kernel we get a much smoother curve
span <- 7
fit <- with(polls_2008,
            ksmooth(day, margin, x.points = day, kernel = "normal", bandwidth = span))

polls_2008 %>% 
  mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = 0.5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")
