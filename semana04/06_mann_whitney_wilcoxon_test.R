#### Test Mann-Whitney-Wilcoxon 

library(tidyverse)
library(rafalib)
library(dslabs)

data("ChickWeight")

head(ChickWeight)

plot(ChickWeight$Time, ChickWeight$weight, col = ChickWeight$Diet)

chick <- ChickWeight %>% 
  reshape(idvar = c('Chick', 'Diet'), 
          timevar = 'Time', 
          direction = 'wide')

head(chick)

chick <- na.omit(chick)

x <- chick %>% 
  filter(Diet == 1) %>% 
  select(weight.4) %>% 
  unlist()

y <- chick %>% 
  filter(Diet == 4) %>% 
  select(weight.4) %>% 
  unlist()

wilcox.test(x,y)

x_with_outlier <- c(x, 200)

t.test(x_with_outlier,y)$p.value

wilcox.test(x_with_outlier,y)$p.value

mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

t.test(x,y+10)$statistic - t.test(x,y+100)$statistic 
