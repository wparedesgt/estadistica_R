##### Quiz semana 02
library(tidyverse)
library(rafalib)
library(dslabs)

### Q1

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- paste0('datos/',basename(url)) 
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

# Set the seed for reproducibility
set.seed(1)


n <- 50  # Tamaño de la muestra
num_simulations <- 1000  # Numero de Simulaciones
sample_averages <- numeric(num_simulations)  # Guardamos el resultado

population_mean <- mean(x) ## Promedio de la muestra poblacional

# Creamos las 10000 simulaciones
for (i in 1:num_simulations) {
  sample_averages[i] <- mean(sample(x, n))
}

# Calculamos la poblacion de aquellos que tengan un gramo mayor que el promedio
proportion <- mean(abs(sample_averages - population_mean) > 1)

# Print the result
proportion


### Q2

library(gapminder)
data("gapminder")
head(gapminder)

set.seed(1)

paises_1952 <- gapminder %>% 
  filter(year == 1952)

exp_40 <- paises_1952 %>% 
  filter(lifeExp >= 40)
exp_60 <- paises_1952 %>% 
  filter(lifeExp <= 60)

nrow(exp_60)/nrow(exp_40)

nrow(exp_40)/nrow(exp_60)

dat1952 = gapminder[ gapminder$year == 1952, ]
x = dat1952$lifeExp
mean(x <= 60) - mean(x <= 40)
