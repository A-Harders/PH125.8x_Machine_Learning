# config
library(tidyverse)
library(lubridate)
library(dslabs)

# Q1 - compute the number of ratings for each movie, plot it against the year
      # use the square root transformation, which year has the highest median
movielens %>%
  group_by(year) %>%
  summarize(count_rating = sqrt(n())) %>%
  ggplot(aes(year, count_rating, label = year)) +
  geom_point() +
  geom_label()

    # EDX CORRECT ANSWER
    movielens %>% group_by(movieId) %>%
      summarize(n = n(), year = as.character(first(year))) %>%
      qplot(year, n, data = ., geom = "boxplot") +
      coord_trans(y = "sqrt") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Q2 - select the top 25 rated movies (n/year) from 1993 to 2018, what is the avg of shawshank
movielens %>%
  group_by(title) %>%
  filter(year >= 1993, year <= 2018) %>%
  summarize(n = n(), year = as.character(first(year)))

movielens %>%
  filter(title == "Shawshank Redemption, The") %>%
  group_by(year) %>%
  summarize(avg_rating = mean(rating))

movielens %>%
  group_by(year) %>%
  filter(title == "Forrest Gump") %>%
  summarize(n = n()/24)

    # EDX CORRECT ANSWER
    movielens %>% 
      filter(year >= 1993) %>%
      group_by(movieId) %>%
      summarize(n = n(), years = 2018 - first(year),
                title = title[1],
                rating = mean(rating)) %>%
      mutate(rate = n/years) %>%
      top_n(25, rate) %>%
      arrange(desc(rate))

# Q3 - stratify movies by rating per year, plot average rating vs rating per year
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

# Q4 - given Q3, which value would be most appropriate to fill blank entries
    # ANSWER - Fill in the missing values with a lower value than the average rating

# Q5 - process the timestamp data into a usable date format
movielens <- mutate(movielens, date = as_datetime(timestamp))
movielens

# Q6 - compute the average rating for each week, plot against date
movielens %>% 
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

# Q7 - given Q6, which formula is the most appropriate
    # ANSWER - Yu,i=μ+bi+bu+f(du,i)+εu,i , with f a smooth function of du,i

# Q8 - define a category for the ratings, which has the lowest average rating
movielens %>%
  group_by(genres) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  filter(n >= 1000) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(rating)
    
    # EDX CORRECT ANSWER
    movielens %>% group_by(genres) %>%
      summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
      filter(n >= 1000) %>% 
      mutate(genres = reorder(genres, avg)) %>%
      ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
      geom_point() +
      geom_errorbar() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Q9 - given Q8, which formula is the most appropriate
    # ANSWER - Yu,i=μ+bi+bu+∑Kk=1xku,iβk+εu,i, with xku,i=1 if gu,i is genre k