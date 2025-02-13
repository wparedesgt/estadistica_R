#### Robust Summaries ###

library(tidyverse)
library(dslabs)
library(rafalib)

set.seed(1)
x=c(rnorm(100,0,1)) ##real distribution
x[23] <- 100 ##mistake made in 23th measurement
boxplot(x)

