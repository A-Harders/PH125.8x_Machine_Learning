# config
library(tidyverse)
library(dslabs)

# to compute singular value decomposition we need to turn all NAs to 0
y <- train_small %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

rownames(y) <- y[,1]
y <- y[,-1]    

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)
    
    # the vectors q (movies) are called principal components
    dim(pca$rotation)
    
    # the vectors p (users) are stored in this matrix
    dim(pca$x)
    
    # the pca function returns a component with the variability of each principle component
    plot(pca$sdev)
    
    # we can see with just a few of these we can explain a large percentage of data
    var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
    plot(var_explained)
    
    # we plot the principal components  to see if they are capturing something important
    # in our case we label the points with the movie that each is related to
    library(ggrepel)
    pcs <- data.frame(pca$rotation, name = colnames(y))
    pcs %>% ggplot(aes(PC1, PC2)) + geom_point() +
      geom_text_repel(aes(PC1, PC2, label=name),
                      data = filter(pcs,
                                    PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))
    
    # PC1 shows the difference between critically acclaimed movies and hollywood blockbusters
        # Critically acclaimed
        pcs %>% select(name, PC1) %>%
          arrange(PC1) %>% slice(1:10)
        
        # Hollywood blockbusters
        pcs %>% select(name, PC1) %>%
          arrange(desc(PC1)) %>% slice(1:10)
        
    # PC2 shows the difference between nerd favorites and artsy movies
        # Nerd Favorites
        pcs %>% select(name, PC2) %>%
          arrange(PC2) %>% slice(1:10)
        
        # Artsy
        pcs %>% select(name, PC2) %>%
          arrange(desc(PC2)) %>% slice(1:10)

# PCA has shown that a matrix factorisation approach can find important structures
