# config
# run 6-1-1
library(matrixStats)

# we can compute the SD of each column and plot them on a histogram
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

# the caret package includes a function that recommends features to be removed due to near 0 variance
nzv <- nearZeroVar(x)
    # we can plot it as an image, the maroon are columns that are to be removed
    image(matrix(1:784 %in% nzv, 28, 28))

# we can see how many remaining columns there are after removing the low variate predictors
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)
