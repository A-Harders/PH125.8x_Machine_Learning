# config
library(tidyverse)
library(caret)
library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

# Q1 - plot the first two components with color, which tissue is a cluster?
qplot(tissue_gene_expression$x[,1], tissue_gene_expression$x[,2],
      col=tissue_gene_expression$y)
    #EDX CORRECT ANSWER
    pc <- prcomp(tissue_gene_expression$x)
    data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
               tissue = tissue_gene_expression$y) %>%
      ggplot(aes(pc_1, pc_2, color = tissue)) +
      geom_point()

# Q2 - compute the average across all predictors, plot again the first PC
pc$x[,2]
pc_mean <- sapply(pc$x, function(object){
  mean(object)
})

df <- data.frame(pc_mean = pc_mean, pc_1 = pc$x[,1],
                 tissue = tissue_gene_expression$y)

df %>% ggplot(aes(pc_1, pc_mean, color = tissue)) +
  geom_point()

d <- dist(df)

# Q3 - redo the PCA only after removing the centre
x <- with(tissue_gene_expression, sweep(x, 1, mean(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()
