# config
library(tidyverse)
library(dslabs)

# we start with the simplest model; predicitng the same rating for all users and movies
    # mu represents the true rating for all movies and users
    # the estimate that minimizes the RMSE is the least square estimate of mu, in this case its the average
    mu_hat <- mean(train_set$rating)
    mu_hat    
    
    # we then compare the average rating, by computing the RMSE on the test set data
    naive_rmse <- RMSE(test_set$rating, mu_hat)
    naive_rmse    
    
    # the average minimises the RSME when using this model
    predictions <- rep(2.5, nrow(test_set))
    RMSE(test_set$rating, predictions)    
    
# as we are comparing models we will create a table
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results

# we know that some movies have higher ratings than others
    # we can augment our last model by adding the effect of the average movie rating
    # we can use these squares to estimate bs
    # fit <- lm(rating ~ as.factor(userId), data = movielens)
        # as there are thousands of bs, the lm funcsiton is very slow, DO NOT RUN
    
    # in this siutation we know that the LSE is the average of Yui
    mu <- mean(train_set$rating)
    movie_avgs <- train_set %>%
      group_by(movieId) %>%
      summarize(b_i = mean(rating - mu))
    
    movie_avgs %>%
      qplot(b_i, geom="histogram", bins = 10, data = ., color = I("black"))
    
    # now we can evaluate the RMSEof our new model
    predicted_ratings <- mu + test_set %>%
      left_join(movie_avgs, by='movieId') %>%
      .$b_i

    model_1_rmse <- RMSE(predicted_ratings, test_set$rating)    
    rmse_results <- bind_rows(rmse_results,
                              tibble(method = "Movie Effect Model",
                                         RMSE = model_1_rmse))    
    rmse_results %>% knitr::kable()

# in the same way we evaluate movie ratings, we can look at user ratings
    # lets explore the data by computing the average rating for frequently rating users
    train_set %>%
      group_by(userId) %>%
      summarize(b_u = mean(rating)) %>%
      filter(n()>=100) %>%
      ggplot(aes(b_u)) +
      geom_histogram(bins = 30, color = "black")
   
    # we can fit with lm() but THIS WILL CAUSE THE COMPUTER TO CRASH
    # lm(rating ~ as.factor(movieId), as.factor(userId))
    
    # instead we compute the overall mean and the movie effect, then we compute the user effect
    # by taking the average after removing the overall mean and the movie from the ratings
    user_avg <- train_set %>%
      left_join(movie_avgs, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = mean(rating - mu - b_i))
    
    # now we can evaluate the model by predicting values and computing the RMSE
    predicted_ratings <- train_set %>%
      left_join(movie_avgs, by = 'movieId') %>%
      left_join(user_avg, by = 'userId') %>%
      mutate(pred = mu + b_i + b_u) %>%
      .$pred

    model_2_rmse <- RMSE(predicted_ratings, train_set$rating)    
    rmse_results <- bind_rows(rmse_results,
                              tibble(method = "Movie + User Effects Model",
                                     RMSE = model_2_rmse))    
    
    rmse_results %>% knitr::kable()
    
    