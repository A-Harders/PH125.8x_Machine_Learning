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
avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])

# Q3 - redo the PCA only after removing the centre
x <- with(tissue_gene_expression, sweep(x, 1, mean(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# Q4 - make a boxplot of the first 10 pcs, for the 7th which two tissues have the greatest median differences
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

# Q5 - plot the percent variance by PC number
plot(summary(pc)$importance[3,])
