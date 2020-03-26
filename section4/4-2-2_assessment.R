# config
library(caret)
library(dslabs)
library(tidyverse)

set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

# Q1 - which code correctly performs cross-validation
fit <- train(x_subset, y, method = "glm")
fit$results

# Q2 - which code correctly creates a vector of the p-values called pvals?
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value

# Q3 - create an index using the p-value cutoff of 0.01
ind <- which(tt$p.value <= 0.01)
length(ind)

# Q4 - find the accuracy of subset x
x_subset <- x[,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

# Q5 - Cross-validate again against kNN
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101,301,25)))
ggplot(fit)

# Q7 - use the train() function and find what k value gives the best result
data("tissue_gene_expression")
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

fit <- train(x, y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))
ggplot(fit)
