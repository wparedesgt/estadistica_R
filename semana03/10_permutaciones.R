#### Test de Permutaciones ###

library(tidyverse)
library(dslabs)
library(rafalib)

# RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
# set.seed(1)

dat <- read.csv('datos/femaleMiceWeights.csv')

control <- dat %>% 
  filter(Diet == 'chow') %>% 
  select(Bodyweight) %>% 
  unlist()

treatmet <- dat %>% 
  filter(Diet == 'hf') %>% 
  select(Bodyweight) %>% 
  unlist()

obsdiff <- mean(treatmet) - mean(control)

N <- 12

avgdiff <- replicate(1000, {
  all <- sample(c(control, treatmet))
  newcontrols <- all[1:N]
  newtreatments <- all[(N+1):(2*N)]
  return(mean(newtreatments) - mean(newcontrols))
})

hist(avgdiff)
abline(v = obsdiff, col = 'red', lwd = 2)

### estimacion de valor P o p-value ##
 
(sum(abs(avgdiff) >  abs(obsdiff)) + 1) / (length(avgdiff) + 1)


## Creando el experimento con un set de datos mas pequeño ###


# RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
# set.seed(1)

N <- 5
control <- sample(control, N)
treatmet <- sample(treatmet, N)
obsdiff <- mean(treatmet) - mean(control)

avgdiff <- replicate(1000, {
  all <- sample(c(control, treatmet))
  newcontrols <- all[1:N]
  newtreatments <- all[(N+1):(2*N)]
  return(mean(newtreatments) - mean(newcontrols))
})

hist(avgdiff) 
abline(v = obsdiff, col = 'red', lwd = 2)

(sum(abs(avgdiff)) > abs(obsdiff) + 1) / (length(avgdiff) + 1) 



