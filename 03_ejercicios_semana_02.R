######### Ejercicios Semana 02 ############
library(tidyverse)
library(dslabs)
library(rafalib)


############## Ejercicio 01 ###############

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- paste0('datos/',basename(url))
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

mean(x)
set.seed(1)
abs(mean(sample(x, 5)) - mean(x))
set.seed(5)
abs(mean(sample(x, 5)) - mean(x))

############# Ejercicio 02 #################

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- paste0('datos/',basename(url))
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

# Set the seed for reproducibility
set.seed(1)

# Initialize a vector to store the averages
averages <- numeric(1000)

# Use a for-loop to take 1,000 random samples
for (i in 1:1000) {
  # Take a random sample of 5 mice from the population
  sample_mice <- sample(x, size = 5)
  
  # Calculate the average of the sample
  averages[i] <- mean(sample_mice)
}

# Check the first few averages
head(averages)

summary(averages)
hist(averages, main = "Distribution of Sample Averages", xlab = "Sample Average", col = "lightblue")

# Calculate the mean of the population
population_mean <- mean(x)

# Calculate the absolute differences between the sample averages and the population mean
differences <- abs(averages - population_mean)

# Identify averages that are more than 1 gram away
more_than_one_gram <- differences > 1

# Calculate the proportion
proportion <- sum(more_than_one_gram) / length(averages)

# Print the result
proportion

############################

# Set the seed for reproducibility
set.seed(1)

# Initialize a vector to store the averages of 10,000 samples
averages <- numeric(10000)

# Loop to take 10,000 random samples of 5 mice and calculate their means
for (i in 1:10000) {
  sample_mice <- sample(x, size = 5)
  averages[i] <- mean(sample_mice)
}

# Calculate the mean of the population
population_mean <- mean(x)

# Calculate the absolute differences between the averages and the population mean
differences <- abs(averages - population_mean)

# Calculate the proportion of averages more than 1 gram away
proportion <- sum(differences > 1) / length(averages)

# Print the result
proportion


##### Ejercicios de distribucion y probabilidad ####

library(tidyverse)
library(dslabs)
library(rafalib)
library(gapminder)

data("gapminder")
head(gapminder)

mean(x <= 40)

x <- gapminder %>% 
  filter(year == 1952) %>% 
  select(lifeExp) %>%
  unlist()

hist(x, 
     main = "Histogram of Life Expectancy in 1952", 
     xlab = "Life Expectancy", 
     col = "lightblue", 
     border = "black")

prop <- function(q){
   mean(x <= q)
}

prop(40)

qs <- seq(from = min(x), to =  max(x), length= 20)
qs

props <- sapply(qs, prop)
props
plot(qs, props)

props <- sapply(qs, function(q) mean(x <=q))
plot(qs, props)
plot(ecdf(x))

#### Ejercicios de Distribucion Normal ####

library(tidyverse)
library(dslabs)
library(rafalib)
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- paste0('datos/',basename(url))
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

# make averages5
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}

# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}

mypar(1,2)
hist(averages5)
hist(averages50)


proportion <- mean(averages50 >= 23 & averages50 <= 25)

# Print the result
cat("Proportion of averages between 23 and 25:", proportion, "\n")



# Given parameters
mu <- 23.9
sigma <- 0.43

# Calculate cumulative probabilities
P_25 <- pnorm(25, mean = mu, sd = sigma)  # F(25)
P_23 <- pnorm(23, mean = mu, sd = sigma)  # F(23)

# Proportion between 23 and 25
proportion <- P_25 - P_23

# Print result
cat("Proportion of observations between 23 and 25:", proportion, "\n")


################## Central Limit Theorem  #########

library(tidyverse)
library(dslabs)
library(rafalib)
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- paste0('datos/',basename(url))
download(url, destfile=filename)
dat <- read.csv(filename) 
dat <- na.omit(dat)

x <- dat %>% 
  filter(Sex == 'M' & Diet == 'chow') %>% 
  select(Bodyweight) %>% 
  unlist()
mean(x)

popsd(x)
set.seed(1)

X <- sample(x, 25)
mean(X)


y <- dat %>% 
  filter(Sex == 'M' & Diet == 'hf') %>% 
  select(Bodyweight) %>% 
  unlist()
mean(y)

popsd(y)

set.seed(1)

Y <- sample(y, 25)
mean(Y)

### Diferencia absoluta

abs(mean(Y)-mean(X)) - abs(mean(y) - mean(x)) 


############### Poblacion Femenina ##########

x  <- dat %>% 
  filter(Sex == 'F' & Diet == 'chow') %>% 
  select(Bodyweight) %>% 
  unlist()
y <- dat %>% 
  filter(Sex == 'F' & Diet == 'hf') %>% 
  select(Bodyweight) %>% 
  unlist()

set.seed(2)

X <- sample(x, 25)
Y <- sample(y, 25)


abs(mean(Y)-mean(X)) - abs(mean(y) - mean(x)) 

#######################

x <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(2)
X <- sample(x,25)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
set.seed(2)
Y <- sample(y,25)
abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )


############ Ejercicios Central Limit Theorem ############

library(tidyverse)
library(dslabs)
library(rafalib)
library(downloader) 

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )


# Calcular el promedio (mu) y la desviación estándar (sigma) de la columna Bodyweight
mu <- mean(dat$Bodyweight)      # Promedio
sigma <- sd(dat$Bodyweight)     # Desviación estándar

# Calcular los límites de una desviación estándar
lower_limit <- mu - sigma
upper_limit <- mu + sigma

# Calcular la proporción usando pnorm()
P_upper <- pnorm(upper_limit, mean = mu, sd = sigma)
P_lower <- pnorm(lower_limit, mean = mu, sd = sigma)

# Proporción de valores dentro de una desviación estándar
proportion <- P_upper - P_lower

# Mostrar resultado
cat("Proporción de valores dentro de una desviación estándar:", proportion, "\n")

## Respuesta  ##

pnorm(2)-pnorm(-2)

####

pnorm(3)-pnorm(-3)

####

y <- dat %>% 
  filter(Sex == 'M' & Diet == 'chow') %>% 
  select(Bodyweight) %>% 
  unlist()


# Filtrar los pesos de machos en dieta de control
y <- dat$Bodyweight[dat$Sex == "M" & dat$Diet == "chow"]

# Calcular promedio (mu) y desviación estándar (sigma)
mu <- mean(y)
sigma <- sd(y)

# Calcular límites de una desviación estándar
lower_limit <- mu - sigma
upper_limit <- mu + sigma

# Proporción de valores dentro del rango
proportion <- mean(y >= lower_limit & y <= upper_limit)

# Mostrar resultado
cat("Proporción de ratones machos en dieta de control dentro de una desviación estándar:", proportion, "\n")


##### Calculando la proporcion entre dos desviaciones estandard #####

mean(y) - 2 * sd(y)
mean(y) + 2 * sd(y)

mean(y >= mean(y) - 2 * sd(y) & y <= mean(y) + 2 * sd(y))

#### Calculando la proporcion entre 3 desviaciones estandard ###

mean(y >= mean(y) -3 * sd(y) & y <= mean(y) + 3 * sd(y))

##### qqplot ####

z <- ( y - mean(y) ) / popsd(y)
qqnorm(z)
abline(0,1)


# Ahora tomaremos una muestra de tamaño 25 de la población de hombres con dieta de pienso. El promedio de esta muestra es nuestra variable aleatoria. Utilizaremos la función replicate() para observar 10.000 realizaciones de esta variable aleatoria. Fijaremos la semilla en 1, luego generaremos estos 10.000 promedios. Construyamos un histograma y un diagrama qq de estos 10.000 números contra la distribución normal.
# 
# Podemos ver que, como predice la TCL, la distribución de la variable aleatoria se aproxima muy bien a la distribución normal.


y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

mean(avgs)
popsd(avgs)


############# Ejercicio 3 /13 ##############
# Central Limit Theorem and T distribution

library(tidyverse)
library(dslabs)
library(rafalib)
library(downloader) 

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- paste0("datos/","femaleMiceWeights.csv")  
if(!file.exists(filename)) download(url,destfile=filename)
dat <- read.csv(filename)

#### Probabilidad en juego de datos

##Ejemplo
n <- 100
x <- sample(1:6, n, replace = TRUE)
mean(x == 6)

#### Ejercicio ###

set.seed(1)  # Set Seeed

n <- 30  # Numero de tiros del dato
trials <- 10000  # Numero de pruebas
p <- 0.5  # Probabilidad de que salga un 6

# Simular el lanzamiento de n dados para cada prueba y calcular proporciones de veces que salga un 6
proportions <- replicate(trials, mean(sample(1:6, n, replace = TRUE) == 6))

# Calcular scord de z
z <- (proportions - p) / sqrt(p * (1 - p) / n)

# Calcular cuando z es mayor que 2
prop_outside <- mean(abs(z) > 2)

prop_outside ### Ver el resultado

qqnorm(proportions)
qqline(proportions)

hist(proportions)

## Respuesta

ps <- c(0.5, 0.5, 0.01, 0.01)
ns <- c(5, 30, 30, 100)
library(rafalib)
mypar(4, 2)

for(i in 1:4) {
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000, {
    x <- sample(1:sides, n, replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  })
  hist(zs, nclass=7)
  qqnorm(zs)
  abline(0,1)
}

#### Q3 
library(tidyverse)

X <- filter(dat, Diet=="chow") %>% 
  select(Bodyweight) %>% 
  unlist()
Y <- filter(dat, Diet=="hf") %>% 
  select(Bodyweight) %>% 
  unlist()

mypar()

mean(X)

## Q5
0

## Q6
sd(X)


## Q7

# Given or calculated values
sigma_X <- sd(X)  # Sample standard deviation
n <- length(X)    # Sample size

# Standard error of the mean
SE <- sigma_X / sqrt(n)

# Z-score for 2 grams
z <- 2 / SE

# Probability using the standard normal distribution
probability <- 2 * (1 - pnorm(z))
probability

## Q8

# Calculate sample standard deviations
sigma_X <- sd(X)
sigma_Y <- sd(Y)

# Calculate the standard error
SE_diff <- sqrt((sigma_Y^2 / 12) + (sigma_X^2 / 12))
SE_diff

### Q9 

t_stat <- (mean(Y)-mean(X)) / SE_diff
t_stat


#### Q10

#Distribucion Normal con promedio 0 y desviacion estandar de 1

## Q11


# Example values (replace with actual t-statistic and df)
t <- t_stat  # Replace with the t-statistic from Exercise 9
df <- 22  # Degrees of freedom, if applicable

# For normal distribution
p_value_normal <- 2 * (1 - pnorm(abs(t)))

# For t-distribution
p_value_t <- 2 * (1 - pt(abs(t), df))

p_value_normal  # Use if normal
p_value_t       # Use if t-distributed


### Q 12

# Assuming X and Y are your two groups:
result <- t.test(X, Y, var.equal = TRUE)  # Use var.equal=TRUE if variances are assumed equal
p_value <- result$p.value  # Extract the p-value
p_value



# Assuming X and Y are your two numeric vectors:
result <- t.test(X, Y)  # Perform the t-test
p_value <- result$p.value  # Extract the p-value
p_value
