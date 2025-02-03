#### Test Semana 03 ####

library(tidyverse)
library(dslabs)
library(rafalib)
library(downloader)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- paste0('datos/',basename(url))
download(url, destfile=filename)
babies <- read.table("datos/babies.txt", header=TRUE)

btw.nonsmoke <- babies %>% 
  filter(smoke == 0)  %>% 
  select(bwt) %>% 
  unlist()

btw.smoke <- babies %>% 
  filter(smoke == 1) %>% 
  select(bwt) %>% 
  unlist()

mean(btw.nonsmoke) - mean(btw.smoke)
popsd(btw.nonsmoke)
popsd(btw.smoke)

set.seed(1)
N <- 25

dat.ns <- sample(btw.nonsmoke, 25)
dat.s <- sample(btw.smoke, 25)

obs <- mean(dat.ns) - mean(dat.s)  ### Aqui el error

se <- sqrt(
  var(dat.ns) / N +
    var(dat.s) /N)

tval <- obs /se
tval

# Recordemos que resumimos nuestros datos utilizando una estadística t porque sabemos que en situaciones en las que la hipótesis nula es verdadera (lo que queremos decir cuando decimos "bajo la hipótesis nula") y el tamaño de la muestra es relativamente grande, este valor t tendrá una distribución normal estándar aproximada. Como conocemos la distribución del valor t bajo la hipótesis nula, podemos determinar cuantitativamente cuán inusual sería el valor t observado si la hipótesis nula fuera verdadera.
# 
# El procedimiento estándar es examinar la probabilidad de que una estadística t que realmente siga la hipótesis nula tenga un valor absoluto mayor que el valor absoluto del valor t que acabamos de observar; esto se llama prueba bilateral.
# 
# Hemos calculado esto restando uno al área bajo la curva normal estándar entre -abs(tval) y abs(tval). En R, podemos hacer esto utilizando la función pnorm(), que calcula el área bajo una curva normal desde el infinito negativo hasta el valor dado como su primer argumento:


pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
pval



### Ejercicios de intervalos de confidencia


library(tidyverse)
library(dslabs)
library(rafalib)
library(downloader)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- paste0('datos/',basename(url))
download(url, destfile=filename)
babies <- read.table("datos/babies.txt", header=TRUE)

btw.nonsmoke <- babies %>% 
  filter(smoke == 0)  %>% 
  select(bwt) %>% 
  unlist()

btw.smoke <- babies %>% 
  filter(smoke == 1) %>% 
  select(bwt) %>% 
  unlist()

mean(btw.nonsmoke) - mean(btw.smoke)
popsd(btw.nonsmoke)
popsd(btw.smoke)

set.seed(1)
N <- 25

dat.ns <- sample(btw.nonsmoke, 25)
dat.s <- sample(btw.smoke, 25)

obs <- mean(dat.ns) - mean(dat.s)  ### Aqui el error

se <- sqrt(
  var(dat.ns) / N +
    var(dat.s) /N)

tval <- obs /se
tval


# Set the seed
set.seed(1)

# Sample size
N <- 25

# Obtain samples
dat.ns <- sample(btw.nonsmoke, N, replace = FALSE)
dat.s <- sample(btw.smoke, N, replace = FALSE)

# Sample statistics
mean.diff <- mean(dat.ns) - mean(dat.s)
s.ns <- sd(dat.ns)
s.s <- sd(dat.s)

# Pooled standard deviation
S_p <- sqrt(((N - 1) * s.ns^2 + (N - 1) * s.s^2) / (2 * N - 2))

# Standard error
SE <- S_p * sqrt(2 / N)

# Critical t-value for 99% confidence and 2N-2 degrees of freedom
t_star <- qt(0.995, df = 2 * N - 2)

# Margin of error
margin_error <- t_star * SE

# Add and subtract margin of error for confidence interval
c(mean.diff - margin_error, mean.diff + margin_error)

