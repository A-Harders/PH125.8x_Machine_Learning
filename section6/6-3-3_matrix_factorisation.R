# config
library(tidyverse)
library(dslabs)

# we create a small dataset of movies and users with many entries each
train_small <- movielens %>%
  group_by(movieId) %>%
  filter(n() >= 50|movieId==3252) %>% # #3252 is Scent of a Woman, used in example
  ungroup() %>%
  group_by() %>%
  filter(n() >= 50) %>%
  ungroup()

# we then generate the matrix
y <- train_small %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

    # to facilitate exploration we add row and col names
    rownames(y) <- y[,1]
    y <- y[,-1]    
    
    colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])
    
    # we then convert the residuals, by removing the col and row averages
    y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
    y <- sweep(y, 2, colMeans(y, na.rm=TRUE))

# if the model describes the signal, then the residuals should be independent; but they arent
    # this plot of the godfather and godfather 2 shows correlation
    m_1 <- "Godfather, The"
    m_2 <- "Godfather: Part II, The"    
    qplot(y[,m_1], y[,m_2], xlab = m_1, ylab = m_2) 
    
    # this plot of the godfather and goodfellas also shows corrleation
    m_1 <- "Godfather, The"
    m_3 <- "Goodfellas"    
    qplot(y[,m_1], y[,m_3], xlab = m_1, ylab = m_3) 

    # there is also correlation between other movies
    m_4 <- "You've Got Mail"
    m_5 <- "Sleepless in Seattle"    
    qplot(y[,m_4], y[,m_5], xlab = m_4, ylab = m_5) 
    
    # by looking at the pairwise correlation, we can see that movies from the same genre can have correlated ratings
    cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use = "pairwise.complete") %>%
      knitr::kable()
    
    # with this we can create the factors for the gangster and romcom, and the users
    set.seed(1, sample.kind="Rounding")
    options(digits = 2)
    Q <- matrix(c(1,1,1,-1,-1), ncol=1)
    rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
    P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
    rownames(P) <- 1:nrow
    
    X <- jitter(P%*%t(Q))
    X %>% knitr::kable(align = "c")
    
    cor(X)
    
        # here we can see the 1 for gangster and -1 for romcom
        t(Q) %>% knitr::kable(align="c")
        
        # and the 2 for gangster lovers, romcom hater
        # the -2 for ganster haters, romcom lovers
        # the 0 for those impartial to either
        P
        
# in reality it is more complex than gangster vs romcom, we can see this by adding scent of a woman
    # which requires a new layer to the factor, Al Pacino and non-Al Pacino movies
    set.seed(1, sample.kind="Rounding")
    options(digits = 2)
    m_6 <- "Scent of a Woman"
    Q <- cbind(c(1,1,1,-1,-1,-1),
               c(1,1,-1,-1,-1,1))
    rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
    P <- cbind(rep(c(2,0,-2), c(3,5,4)),
               c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
    rownames(P) <- 1:nrow(X)
    
    X <- jitter(P%*%t(Q), factor=1)
    X %>% knitr::kable(align = "c")
    
    cor(X)
    
        # here we can see the 2 factors
        t(Q) %>% knitr::kable(align="c")
        
        # here we can see the 2 coefficients
        P

    # now we can find the correlation from the data we constructed
    six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
    tmp <- y[,six_movies]
    cor(tmp, use="pairwise.complete")
    