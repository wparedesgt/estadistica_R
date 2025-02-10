###Simulaciones de Monte Carlo###

library(tidyverse)
library(dslabs)
library(rafalib)
library(downloader)


RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
set.seed(1)


url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- "datos/mice_pheno.csv"
if (!file.exists(filename)) download(url,destfile=filename)

dat <- read.csv("mice_pheno.csv")

controlPopulation <- dat %>% 
  filter(Sex == "F" & Diet == "chow") %>%  
  select(Bodyweight) %>% 
  unlist()

ttestgenerator <- function(n) {
  #note that here we have a false "high fat" group where we actually
  #sample from the chow or control population. 
  #This is because we are modeling the null.
  cases <- sample(controlPopulation,n)
  controls <- sample(controlPopulation,n)
  tstat <- (mean(cases)-mean(controls)) / 
    sqrt( var(cases)/n + var(controls)/n ) 
  return(tstat)
}

ttests <- replicate(1000, ttestgenerator(10))


hist(ttests)

qqnorm(ttests)
abline(0,1)


ttests <- replicate(1000, ttestgenerator(3))
qqnorm(ttests)
abline(0,1)


ps <- (seq(0,999)+0.5)/1000
qqplot(qt(ps,df=2*3-2),ttests,xlim=c(-6,6),ylim=c(-6,6))
abline(0,1)


qqnorm(controlPopulation)
qqline(controlPopulation)


controls<- rnorm(5000, mean=24, sd=3.5) 


ttestgenerator <- function(n, mean=24, sd=3.5) {
  cases <- rnorm(n,mean,sd)
  controls <- rnorm(n,mean,sd)
  tstat <- (mean(cases)-mean(controls)) / 
    sqrt( var(cases)/n + var(controls)/n ) 
  return(tstat)
}


ttests <- replicate(1000, ttestgenerator(3))
qqnorm(ttests)
abline(0,1)
