# config
# run 6-1-1 & 6-1-2

# caret package requires column names to the feature matrices
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)

# we will start with kNN
    # first step is to optimise the number of neighbours
    control <- trainControl(method = "cv", number = 10, p = 0.9)
    train_knn <- train(x[, col_index], y,
                       method = "knn",
                       tuneGrid = data.frame(k=c(1,3,5,7)),
                       trControl = control)
    
        # this code takes several minutes to complete,
        # it is good practice to test on subsets of code
        # to appreciate the time it will take
        n <- 1000 # number of rows to test
        b <- 2 # number of cross-validation folds
        index <- sample(nrow(x), n)
        control <- trainControl(method = "cv", number = b, p = 0.9)
        train_knn <- train(x[index,col_index], y[index],
                           method = "knn",
                           tuneGrid = data.frame(k=c(1,3,5,7)),
                           trControl = control)
        ggplot(train_knn)
        
    # once we're done optimising, we can fit the entire dataset
    fit_knn <- knn3(x[ ,col_index], y, k=3)
        
    # and we can evaluate the model
    y_hat_knn <- predict(fit_knn,
                         x_test[, col_index],
                         type="class")
    
    cm <- confusionMatrix(y_hat_knn, factor(y_test))   
    
    cm$overall["Accuracy"]  
    cm$byClass[,1:2]

# Random Forest, rborist package for performance advantage
    # as random forest is more computationally taxing we start by using a subset
    # even with the subset it will take a few minutes to run
    library(Rborist)
    control <- trainControl(method="cv", number = 5, p = 0.5)    
    grid <- expand.grid(minNode = c(1,5), predFixed = c(10, 15, 25, 35, 50))    
    
    train_rf <- train(x[,col_index],
                      y,
                      method = "Rborist",
                      nTree = 50,
                      trControl = control,
                      tuneGrid = grid,
                      nSamp = 5000)    
    
    # we can see the final result by plotting the model
    ggplot(train_rf, highlight = TRUE)
    
    # we can select the best parameters best on the bestTune
    train_rf$bestTune
    
    # now we can optimise our final tree
    # this will also take a few minutes
    fit_rf <- Rborist(x[, col_index], y,
                      nTree = 1000,
                      minNode = train_rf$bestTune$minNode,
                      predFixed = train_rf$bestTune$predFixed)
    
    # and we can evaluate the model
    y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[,col_index])$yPred])
    cm <- confusionMatrix(y_hat_rf, y_test)
    
    cm$overall["Accuracy"]    
    cm$byClass[,1:2]
    
# we can now see the image and our prediction
library(rafalib)
rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt = "n", yaxt = "n")
}

