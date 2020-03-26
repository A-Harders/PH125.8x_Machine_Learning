#config
library(dslabs)
library(caret)
library(tidyverse)

# load the data source from the 2008 election
data("polls_2008")
qplot(day, margin, data= polls_2008)