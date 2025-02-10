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


# Set the seed for reproducibility
set.seed(1)

# Take samples of size N=25 from each group
N <- 25
dat.ns <- sample(btw.nonsmoke, N)
dat.s <- sample(btw.smoke, N)

# Calculate sample means
mean_diff <- mean(dat.ns) - mean(dat.s)

# Calculate pooled standard error
# First, calculate pooled standard deviation
s_pooled <- sqrt(((N-1)*var(dat.ns) + (N-1)*var(dat.s))/(2*N-2))

# Calculate standard error
SE <- s_pooled * sqrt(1/N + 1/N)

# For 99% confidence interval with 2N-2 degrees of freedom:
df <- 2*N - 2  # = 48 degrees of freedom
t_critical <- qt(0.995, df)  # Using 0.995 for 99% CI (0.99 + (1-0.99)/2)



#### REspuesta

N <- 25
set.seed(1)
dat.ns <- sample(btw.nonsmoke, N) 
dat.s <- sample(btw.smoke, N) 
qt(0.995,48)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )
##note that if you define dat.s before dat.ns, you get a different answer
##due to sampling randomness
##tolerance is set to accept both answers


# Set seed
set.seed(1)
N <- 5

# Take random samples
dat.ns <- sample(btw.nonsmoke, N)
dat.s <- sample(btw.smoke, N)

# Perform t-test
t.test(dat.ns, dat.s)$p.value


# Or more explicitly:
result <- t.test(dat.ns, dat.s)
result$p.value
