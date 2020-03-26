# config
library(dslabs)
data(tissue_gene_expression)

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

# Q1 - calculate the Euclidean distance between observations
d <- dist(x)

# Q2 - compare distances between 1&2, 39&40, 73&74
as.matrix(d)[1:2,1:2]
as.matrix(d)[39:40,39:40]
as.matrix(d)[73:74,73:74]

    # edx best answer
    ind <- c(1, 2, 39, 40, 73, 74)
    as.matrix(d)[ind,ind]    

# Q3 - create an image of the distances
image(as.matrix(d))
