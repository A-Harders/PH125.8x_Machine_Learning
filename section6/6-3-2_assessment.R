# config
library(tidyverse)
library(dslabs)
options(digits=7)

# building the data

    # simulate the student numbers
    set.seed(1986, sample.kind="Rounding")
    n <- round(2^rnorm(1000, 8, 1))
    
    # assign the true quality for each school
    set.seed(1, sample.kind="Rounding")
    mu <- round(80+2*rt(1000,5))
    range(mu)
    schools <- tibble(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
    
        # display the top 10
        schools %>% 
          top_n(10, quality) %>%
          arrange(desc(quality))

    # now we have the students take a test
    set.seed(1, sample.kind="Rounding")
    mu <- round(80+2*rt(1000,5))    
    scores <- sapply(1:nrow(schools), function(i){
        scores <- rnorm(schools$size[i], schools$quality[i], 30)
        scores
    })

    schools <- schools %>%
        mutate(score=sapply(scores, mean))

    schools    

# Q1 - show the top 10 schools, report the ID to the top, average of the 10th
top_10 <- schools %>%
    top_n(10, score) %>%
    arrange(desc(score))

top_10

# Q2 - what is the median school size overall, what is the median school size of the top 10
median(schools$size)
median(top_10$size)

# Q3 - we see that smaller schools produce better results, do the same analysis as Q1-2 on the bottom
bottom_10 <- schools %>%
    arrange(score) %>%
    head(10)

median(bottom_10$size)

# Q4 - plot size vs score, highlight the top 10 based on true quality
top_10_true <- schools %>%
    arrange(desc(quality)) %>%
    head(10)

schools %>%
    ggplot(aes(size, score)) +
    geom_point() +
    geom_point(data=top_10_true, aes(size, score), color="red", size=5)

# Q5 - regularise the data, what is the top school after?
overall <- mean(sapply(scores, mean))
alpha <- 25
score_reg <- sapply(scores, function(x) overall + sum(x-overall)/length(x)+alpha)

reg_schools<- schools %>%
    mutate(score_reg=score_reg) %>%
    top_n(10, score_reg) %>% arrange(desc(score_reg))

# Q6 - find teh best alpha from 10 to 250
alpha <- seq(10,250)

overall <- mean(sapply(scores, mean))
just_the_sum <- sapply(scores, function(x) sum(x-overall))
n_i <- sapply(scores, function(x) length(x))

schools_wo_alpha <- schools %>%
    mutate(just_the_sum=just_the_sum, n_i=n_i)

schools_wo_alpha

app_alphas <- sapply(alpha, function(a){
    predicted_ratings <- schools_wo_alpha %>%
        mutate(reg_scores=just_the_sum/(n_i+a)) %>%
        mutate(pred = overall+reg_scores) %>%
        .$pred
    return(RMSE(predicted_ratings, schools$quality))
})

qplot(alpha, app_alphas)
alpha[which.min(app_alphas)]
    #EDX CORRECT ANSWER
    alphas <- seq(10,250)
    rmse <- sapply(alphas, function(alpha){
        score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
        sqrt(mean((score_reg - schools$quality)^2))
    })
    plot(alphas, rmse)
    alphas[which.min(rmse)]

# Q7 - rank the schools based on the new averages, ID of top school, regularised avg score of 10th
overall <- mean(sapply(scores, mean))
alpha <- 135
score_reg <- sapply(scores, function(x) overall + sum(x-overall)/(length(x)+alpha))

reg_schools<- schools %>%
    mutate(score_reg=score_reg) %>%
    top_n(10, score_reg) %>% arrange(desc(score_reg))

reg_schools

# Q8 - a common mistake is shrinking values towards 0 that arent centered areound 0
#    - if we dont subtract the overall average before shrinking, what value of alpha gives the minimum RMSE
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
    score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
    sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]
