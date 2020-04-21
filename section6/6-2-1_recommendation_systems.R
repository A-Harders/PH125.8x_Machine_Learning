# config
library(dslabs)
library(tidyverse)

# the GroupLens research lab generated a database of over 20 million movie ratings
# we use a small subset of this to create and train recommendation systems
data("movielens")

# we can see the movielens table is tidy format, and contians thousands of rows
head(movielens)
dim(movielens)

# each row represents a rating by one use to one movie,
# we can see the number of unique movies and users
movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# converting the entire dataframe to matrix would crash R so we take a subsection
keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)
tab <- movielens %>%
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)
tab %>% knitr::kable()

# we can see how sparsely populated the matrix is by visualising
users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

# lets look at the data to better understand the challenge
    # some movies get rated more often than others
    movielens %>% 
      dplyr::count(movieId) %>% 
      ggplot(aes(n)) + 
      geom_histogram(bins = 30, color = "black") + 
      scale_x_log10() + 
      ggtitle("Movies")
    
    # some users are more active than others
    movielens %>%
      dplyr::count(userId) %>%
      ggplot(aes(n)) +
      geom_histogram(bins = 30, color = "black") +
      scale_x_log10() +
      ggtitle("Movies")
    
# the first step is to create a test set, using the caret package as normal
library(caret)
set.seed(755, sample.kind = "Rounding")    
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                 p=0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

    # to ensure we dont include users and movies in the test set that arent in the training set
    # we can use the semi_join function
    test_set <- test_set %>%
      semi_join(train_set, by = "movieId") %>%
      semi_join(train_set, by = "userId")

# now we can set ourselves a win condition, to quantify how successful we are
# the netflix challenge used the RMSE, and so will we
# the function computes the RMSE for a vector of ratings and their predictors
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

