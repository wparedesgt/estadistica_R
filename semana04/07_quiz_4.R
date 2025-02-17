### Quiz 4 ###

library(tidyverse)
library(dslabs)
library(rafalib)
library(UsingR)

boxplot(mtcars$mpg)

wilcox.test(c(1,2,3), c(4,5,6))$p.value

wilcox.test(c(1,2,3), c(400,500,600))$p.value

data("nym.2002")
time <- sort(nym.2002$time)

plot(time/median(time), ylim = c(1/4,4))
abline(h = c(1/2, 1, 2))


plot(log2(time/median(time)), ylim = c(-2,2))
abline(h = 1:1)
