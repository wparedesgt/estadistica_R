###### Inferencia Estadistica ####

library(tidyverse)
library(dslabs)
library(rafalib)

#### REpasando series T ###

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

N <-  length(treatment)
obs <- mean(treatment) - mean(control)

se <- sqrt(
  var(treatment) / 3 +
    var(control) /3)

tstat <- obs /se

2 * (1- pnorm(tstat))

######

population <- read.csv('datos/femaleControlsPopulation.csv')
population <- unlist(population)

n <- 10000
nulls <- vector("numeric", n)
for (i in 1:n) {
  control <- sample(population, 10)
  treatment <- sample(population, 10)
  nulls[i] <- mean(treatment) - mean(control)/ se
}

mypar()

qqnorm(nulls)
abline(0,1)


############# Confidence Intervals #####

library(tidyverse)
library(dslabs)
library(rafalib)


chowPopulation <- read.csv('datos/femaleControlsPopulation.csv')
chowPopulation <- unlist(chowPopulation)

mu_chow <- mean(chowPopulation)
mu_chow

N <- 30
set.seed(1)
chow <- sample(chowPopulation,N)
mean(chow)

se <- sd(chow)/sqrt(N)
se

Q <- qnorm(1- 0.05/2)

-Q <(mean(chow) - mean(chowPopulation))/se < Q

pnorm(2) - pnorm(-2)


interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
interval
mean(chowPopulation)

interval[1] < mu_chow & interval[2] > mu_chow

B <- 250
mypar()
plot(mean(chowPopulation) + c(-7,7), c(1,1), type = 'n', 
     xlab = 'weight', ylab = 'interval', 
     ylim = c(1,B))
abline(v = mean(chowPopulation))

i <- 1

for (i in 1:B) {
  chow <- sample(chowPopulation,N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
  covered <- mean(chowPopulation) <= interval[2]
  color <- ifelse(covered, 1, 2)
  lines(interval, c(i,i), col = color)
}

## Cambiando el ejemplo con N = 5

mypar()

plot(mean(chowPopulation) + c(-7,7), c(1,1), type = 'n', 
     xlab = 'weight', ylab = 'interval', 
     ylim = c(1,B))
abline(v = mean(chowPopulation))


Q <- qt(1-0.05/2, df = 4)
N <- 5

for (i in 1:B) {
  chow <- sample(chowPopulation,N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
  covered <- mean(chowPopulation) <= interval[2]
  color <- ifelse(covered, 1, 2)
  lines(interval, c(i,i), col = color)
}
