#config
library("caret")
data("heights")

# Run 4-3-2_naive_bayes.R first

# if we use the rule conditional probability must be greate than 0.5 to predict female, accuracy is effected due to low sensitivity
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# we can see that we have very high specificity to account for the low prevalence
specificity(data = factor(y_hat_bayes), reference =factor(test_set$sex))

# naive Bayes allows us to balance specificity and sensitivity by changing pi hat instead of the cutoff in conditional probability
p_hat_bayes_unbiased <- f1*0.5/(f1*0.5+f0*(1-0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")

    #noting the difference between the sensitivity and specificity
    sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
    specificity(data = factor(y_hat_bayes_unbiased), reference =factor(test_set$sex))
    
    #this plot shows it visually
    qplot(x, p_hat_bayes_unbiased, geom="line") +
      geom_line(aes(x, p_hat_bayes), col = "red") + 
      geom_hline(yintercept = 0.5, lty = 2) +
      geom_vline(xintercept = 67, lty = 2)
    