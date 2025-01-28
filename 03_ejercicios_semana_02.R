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
