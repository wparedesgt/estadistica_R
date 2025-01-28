#####  Week02 ###############

library(tidyverse)
library(dslabs)
library(rafalib)

######## Variables Aleatorias ###########

dat <- read.csv('datos/femaleMiceWeights.csv')

control <- dat %>% 
  filter(Diet == 'chow') %>%
  select(Bodyweight) %>% 
  unlist()

treatment <- dat %>%
  filter(Diet == 'hf') %>% 
  select(Bodyweight) %>% 
  unlist()

control
treatment

mean(treatment)
mean(control)

mean(treatment) - mean(control)

population <- read.csv('datos/femaleControlsPopulation.csv')
population <- unlist(population)

mean(sample(population, 12))
mean(control)
mean(treatment)


################ Distribuciones Null y valores P ####################

obs <- mean(treatment) - mean(control)

population <- read.csv('datos/femaleControlsPopulation.csv')
population <- unlist(population)

n <- 10000
nulls <- vector('numeric', n)

for (i in 1:n) {
  control <- sample(population, 12)
  treatment <- sample(population, 12)
  nulls[i] <- mean(treatment) - mean(control)
}
max(nulls)
hist(nulls)

mean(nulls > obs)
mean(abs(nulls) > obs)

### Distribuciones y aproximaciones normales ###


