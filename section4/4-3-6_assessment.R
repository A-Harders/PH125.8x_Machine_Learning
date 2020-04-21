#config
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

set.seed(1993, sample.kind = "Rounding") #if using R 3.6 or later set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

# Q1 - train a model using lda, what is the accuracy
fit_lda <- train(x, y, method="lda")
fit_lda$results["Accuracy"]

# Q2 - look at the finalModel of fit_lda, plot the means vectors against each other, decide which 2 are driving the algorithm
fit_lda$finalModel["means"] %>% data.frame()

t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# Q3 - train the model using qda, what is the accuracy (reload the config!)
fit_qda <- train(x, y, method="qda")
fit_qda$results["Accuracy"]

# Q4 - look at the finalModel of fit_qda, plot the means vectors against each other, decide which 2 are driving the algorithm
fit_qda$finalModel["means"] %>% data.frame()

t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# Q5 - the mean values of each predictor are not informative, preProcess the lda to scale each column, which 2 genes are driving it now
fit_lda <- train(x, y, method="lda", preProcess = "center")
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name, hippocampus)) +
  geom_point() +
  coord_flip()

d <- apply(fit_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)

# Q6 - repeat the LDA analysis and find the accuracy for ALL tissue types
set.seed(1993, sample.kind="Rounding")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

fit_lda <- train(x, y, method="lda", preProcess = "center")
fit_lda$results["Accuracy"]

confusionMatrix(fit_lda)
