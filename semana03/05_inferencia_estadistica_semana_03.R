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


### Calculos poderosos ######

library(tidyverse)
library(dslabs)
library(rafalib)

dat <- read.csv("datos/mice_pheno.csv") #Previously downloaded 


# Filtrar solo hembras y separar por dieta
controlPopulation <- filter(dat, Sex == "F" & Diet == "chow") %>%  
  select(Bodyweight) %>% unlist
hfPopulation <- filter(dat, Sex == "F" & Diet == "hf") %>%  
  select(Bodyweight) %>% unlist


mu_hf <- mean(hfPopulation)
mu_control <- mean(controlPopulation)
print(mu_hf - mu_control)
print((mu_hf - mu_control)/mu_control * 100) #percent increase


# Realizar el muestreo
options(digits = 7)  # Establecer precisión numérica
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")  ## Importante
set.seed(1)
N <- 5
hf <- sample(hfPopulation, N)
control <- sample(controlPopulation, N)

# Prueba t
t.test(hf, control)$p.value


N <- 12

hf <- sample(hfPopulation, N)
control <- sample(controlPopulation, N)
t.test(hf, control)$p.value


N <- 30 

hf <- sample(hfPopulation, N)
control <- sample(controlPopulation, N)
t.test(hf, control)$p.value



N <- 12
alpha <- 0.05
B <- 2000

reject <- function(N, alpha=0.05){
  hf <- sample(hfPopulation,N) 
  control <- sample(controlPopulation,N)
  pval <- t.test(hf,control)$p.value
  pval < alpha
}


reject(12)

rejections <- replicate(B,reject(N))

mean(rejections)

Ns <- seq(5, 50, 5)

power <- sapply(Ns,function(N){
  rejections <- replicate(B, reject(N))
  mean(rejections)
})

plot(Ns, power, type="b")


N <- 30
alphas <- c(0.1,0.05,0.01,0.001,0.0001)
power <- sapply(alphas,function(alpha){
  rejections <- replicate(B,reject(N,alpha=alpha))
  mean(rejections)
})

plot(alphas, power, xlab="alpha", type="b", log="x")


calculatePvalue <- function(N) {
  hf <- sample(hfPopulation,N) 
  control <- sample(controlPopulation,N)
  t.test(hf,control)$p.value
}

Ns <- seq(10,200,by=10)
Ns_rep <- rep(Ns, each=10)

pvalues <- sapply(Ns_rep, calculatePvalue)

plot(Ns_rep, pvalues, log="y", xlab="sample size",
     ylab="p-values")
abline(h=c(.01, .05), col="red", lwd=2)

N <- 12
hf <- sample(hfPopulation, N)
control <- sample(controlPopulation, N)
diff <- mean(hf) - mean(control)
diff / mean(control) * 100
t.test(hf, control)$conf.int / mean(control) * 100

sd_pool <- sqrt(((N-1)*var(hf) + (N-1)*var(control))/(2*N - 2))
diff / sd_pool


