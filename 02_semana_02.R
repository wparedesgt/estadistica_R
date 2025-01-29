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
  control <- sample(population, 3)
  treatment <- sample(population, 3)
  nulls[i] <- mean(treatment) - mean(control)
}
max(nulls)
hist(nulls)

mean(nulls > obs)
mean(abs(nulls) > obs)

### Distribuciones y aproximaciones normales ###

mypar()

qqnorm(nulls)
qqline(nulls)

#### Construyendo otro ejemplo

library(tidyverse)
library(rafalib)
library(dslabs)

dat <- read.csv('datos/femaleMiceWeights.csv')
population <- read.csv('datos/femaleControlsPopulation.csv')
population <- unlist(population)

control <- dat %>% 
  filter(Diet == 'chow') %>% 
  select(Bodyweight) %>% 
  unlist()
treatment <- dat %>% 
  filter(Diet == 'hf') %>% 
  select(Bodyweight) %>% 
  unlist()


N <- length(treatment)
obs <- mean(treatment) - mean(control)
se <- sqrt(
  var(treatment)/N+
    var(control)/N)
tstat <- obs/se

2*(1 - pnorm(tstat))

n <- 10000
nulls <- vector('numeric', n)

for (i in 1:n) {
  control <- sample(population, N)
  treatment <- sample(population, N)
  nulls[i] <- (mean(treatment) - mean(control)) / se
}

mypar()

qqnorm(nulls)
abline(0,1)


######### T Test ##########

library(tidyverse)
library(rafalib)
library(dslabs)

dat <- read.csv('datos/femaleMiceWeights.csv')

control <- dat %>% 
  filter(Diet == 'chow') %>% 
  select(Bodyweight) %>% 
  unlist()
treatment <- dat %>% 
  filter(Diet == 'hf') %>% 
  select(Bodyweight) %>% 
  unlist()

ttest <- t.test(treatment, control)
ttest

qqnorm(control)
qqline(control)

qqnorm(treatment)
qqline(treatment)
