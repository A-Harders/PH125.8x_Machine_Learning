# config
library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# Q1 - proportion of Females inclass and online
dat %>%
  filter(type == "online") %>%
  group_by(sex) %>%
  summarize(n()/111)

dat %>%
  filter(type == "inclass") %>%
  group_by(sex) %>%
  summarize(n()/39)

x_online_prob <- 111/150

# Q2 - create a ML algorithm and find the accuracy based on the percentages above
y_hat <- ifelse(x == "online", "Male", "Female") %>%
  factor(levels = c("Male","Female"))
mean(y_hat == y)
?sample

# Q3 - create a confusionMatrix() for y and y_hat
confusionMatrix(data = y_hat, reference = y) # this is technically cheating and covers Q4-Q6 also
cm <- table(y_hat, y) # Question asked for this code


# Q4 - what is the sensitivity of our predicition?
sensitivity(data = y_hat, reference = y)

# Q5 - what is the specificity of our predicition?
specificity(data = y_hat, reference = y)

# Q6 - what is the prevelance of our predicition
dat %>%
  group_by(sex) %>%
  summarize(n()/150)
