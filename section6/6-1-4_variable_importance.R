# config
# run 6-1-1

# Rborist doesnt include variable importance calculations
# we will use rf and not pre-process to show the variable importance concept
library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y, ntree = 50)

# we can use the importance() function, which gives us further interpretability
imp <- importance(rf)
imp

# we can then make an image using the importance values, giving us a heatmap
image(matrix(imp, 28,28))

# visualisation is important in discerning why we're failing, and is different per application
# for the digits, we find digits we were quite certain of but were incorrect
    # first we compare kNN
    p_max <- predict(fit_knn, x_test[,col_index])
    p_max <- apply(p_max, 1, max)
    ind <- which(y_hat_knn != y_test)
    ind <- ind[order(p_max[ind], decreasing = TRUE)]
    rafalib::mypar(3,4)
    for(i in ind[1:12]){
      image(matrix(x_test[i,], 28, 28)[,28:1],
            main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                          " but is a ",y_test[i]),
            xact="n", yaxt="n")
    }

    # then we compare rf
    p_max <- predict(fit_rf, x_test[,col_index])$census 
    p_max <- p_max / rowSums(p_max)
    p_max <- apply(p_max, 1, max)
    ind  <- which(y_hat_rf != y_test)
    ind <- ind[order(p_max[ind], decreasing = TRUE)]
    for(i in ind[1:12]){
      image(matrix(x_test[i,], 28, 28)[, 28:1], 
            main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                          " but is a ",y_test[i]),
            xact="n", yact="n")
    }

