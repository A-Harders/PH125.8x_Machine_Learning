# config
library(tidyverse)
library(dslabs)

# recreate the data
library(caret)
set.seed(755, sample.kind = "Rounding")    
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p=0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# exploring our first model, we find the largest mistakes when only using the movie effects
test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%
  select(title, residual) %>%
  slice(1:10) %>%
  knitr::kable()

# to see why the worst performing predictions are obscure movies, we look at the worst base on movie effect
movie_titles <- movielens %>%
  select(movieId, title) %>%
  distinct()
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i) %>%
  slice(1:10) %>%
  knitr::kable()

    # here are the top 10 worst movie by movie effect
    movie_avgs %>% left_join(movie_titles, by="movieId") %>%
      arrange(b_i) %>%
      select(title, b_i) %>%
      slice(1:10) %>%
      knitr::kable()

    # they are all obscure and not rated very often
    train_set %>% dplyr::count(movieId) %>%
      left_join(movie_avgs) %>%
      left_join(movie_titles, by="movieId") %>%
      arrange(desc(b_i)) %>%
      select(title, b_i, n) %>%
      slice(1:10) %>%
      knitr::kable()
    
    train_set %>% dplyr::count(movieId) %>%
      left_join(movie_avgs) %>%
      left_join(movie_titles, by="movieId") %>%
      arrange(b_i) %>%
      select(title, b_i, n) %>%
      slice(1:10) %>%
      knitr::kable()

# by regularising, we can minimise the movie effect for those with lower numbers of ratings
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

    # to see how the estimates shrink, we plot the regularised vs LSE
    tibble(original = movie_avgs$b_i,
           regularised = movie_reg_avgs$b_i,
           n = movie_reg_avgs$n_i) %>%
      ggplot(aes(original, regularised, size = sqrt(n))) +
      geom_point(shape = 1, alpha =0.5)

    # now we can look at our top 10 best movies, based on regularisation
    train_set %>%
      count(movieId) %>%
      left_join(movie_reg_avgs) %>%
      left_join(movie_titles, by="movieId") %>%
      arrange(desc(b_i)) %>%
      select(title, b_i, n) %>%
      slice(1:10) %>%
      knitr::kable()
    
    # we can look at our worst 10 best movies, based on regularisation
    train_set %>%
      count(movieId) %>%
      left_join(movie_reg_avgs) %>%
      left_join(movie_titles, by="movieId") %>%
      arrange(b_i) %>%
      select(title, b_i, n) %>%
      slice(1:10) %>%
      knitr::kable()

# the results seem to make sense, but has regularisation improved our RMSE
predictied_ratings <- train_set %>%
  left_join(movie_reg_avgs, by="movieId") %>%
  mutate(pred= mu+b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, train_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularised Movie Effect",
                                 RMSE = model_3_rmse))
rmse_results %>% knitr::kable()

# lambda is a tuning parameter, and we can use cross-validation to choose it
lambdas <- seq(0,10,0.25)

mu <- mean(train_set$rating)
just_the_sum <- train_set %>%
  group_by(movieId) %>%
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- train_set %>%
    left_join(just_the_sum, by='movieId') %>%
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu+b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, train_set$rating))
})

qplot(lambdas, rmses)
lambdas[which.min(rmses)]

# we can also use this for user effect, using a similar process as movie effect
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>%
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- train_set %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(pred = mu+b_i+b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, train_set$rating))
})

qplot(lambdas, rmses)

min_lam <- lambdas[which.min(rmses)]
min_lam

rmse_results <- bind_rows(rmse_results,
                          tibble(method = "Regularisation M + U Effect Model",
                                     RMSE=min(rmses)))
rmse_results %>% knitr::kable()
