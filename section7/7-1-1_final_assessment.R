# config
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

# q1 - data set questions
    # q1a - how many samples and predictors
    dim(brca$x)
    
    # q1b - proportion of malignant samples
    mean(ifelse(brca$y == "M", 1, 0))
    mean(brca$y == "M")

    # q1c - which column number has the highest mean
    which.max(colMeans(brca$x))
    
    # q1d - which column number has the lowest sd
    which.min(colSds(brca$x))

# q2 - sweep twice to scale each column
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN='/')
    
    # q2a - what is the sd of the first column
    sd(x_scaled[,1])
    
    # q2b - what is the median of the first column
    median(x_scaled[,1])
  
# q3 - calculate the distance between all samples using the scaled matrix
d <- as.matrix(dist(x_scaled))

    # q3a - what is the average distance between the first sample and benign samples
    ind <- which(brca$y=="B")    
    mean((d)[1,ind])

    # q3b - what is the average distance between the first sample and malignant samples
    ind <- which(brca$y=="M")
    mean((d)[1,ind])
    
# q4 - make a heatmap of the relationship between features using the scaled matrix
d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

# q5 - perform heirarchical clustering, cut the tree into 5
    # q5a - which predictor is in its own group
    h <- hclust(d_features)
    groups <- cutree(h, k = 5)
    split(names(groups), groups)

# q6 - perform principal component analysis
pcs <- prcomp(x_scaled)
var_explained <- cumsum(pcs$sdev^2/sum(pcs$sdev^2))
plot(var_explained)

    # q6a - what proportion of variance is explained by the first component
    var_explained[1]
    
    # q6b - how many PCs required to explain 90%
    var_explained[1:7] # first greater than 0.9 is the answer
    
# q7 - plot the first two PCs, with color representing tumor
PC1 <- pcs$x[,1]
PC2 <- pcs$x[,2]
tumor <- brca$y

qplot(PC1, PC2, col=tumor)

    # EDX CORRECT ANSWER
    data.frame(pcs$x[,1:2], type = brca$y) %>%
      ggplot(aes(PC1, PC2, color = type)) +
      geom_point()

# q8 - make a boxplot of the first 10 pcs, grouped by tumor type
    # q8a - which PCs dont overlap IQRs for tumor type
    data.frame(pcs$x[,1:10], type = brca$y) %>%
      gather(key = "PC", value = "value", -type) %>%
      ggplot(aes(PC, value, fill = type)) +
      geom_boxplot()