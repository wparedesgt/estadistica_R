#### Test de asociaciones ###

library(tidyverse)
library(rafalib)
library(dslabs)

tab <- matrix(c(3,1,1,3), 2,2)

rownames(tab) <- c("Poured Before","Poured After")
colnames(tab) <- c("Guessed before","Guessed after")

tab

fisher.test(tab, alternative = 'greater')

### Test ####

d <- read.csv('datos/assoctest.csv')

table(d)

chisq.test(table(d))
fisher.test(table(d))


