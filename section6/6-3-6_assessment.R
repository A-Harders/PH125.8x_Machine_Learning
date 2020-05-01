# config
library(tidyverse)
library(dslabs)

data("tissue_gene_expression")

# Q1 - remove the rowMeans and compute the distance
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))
d

# Q2 - make a hierarchical clustering plot, which tissue is furthest left
hc1 <- hclust(d)
plot(hc1)

# Q3 - run a kmeans() around 7 and report on liver
cl <- kmeans(tissue_gene_expression$x, centers = 7)
table(cl$cluster, tissue_gene_expression$y)

# Q4 - select the 50 most variable genes, create a heatmap
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
