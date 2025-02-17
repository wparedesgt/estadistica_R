### Median, MAD, Dpearman Correlation Exercises ###
library(tidyverse)
library(dslabs)
library(rafalib)

data("ChickWeight")

head(ChickWeight)

plot(ChickWeight$Time, ChickWeight$weight, col = ChickWeight$Diet)

chick <- ChickWeight %>% 
  reshape(idvar = c('Chick', 'Diet'), 
          timevar = 'Time', 
          direction = 'wide')

head(chick)

chick <- na.omit(chick)

chick$weight.4
max(chick$weight.4)
min(chick$weight.4)
mean(chick$weight.4)
median(chick$weight.4)

boxplot(chick$weight.4)


# Obtener todos los pesos del dia 4
day4_weights <-  chick$weight.4

# calcular el promedio sin outliers
mean_original <-  mean(day4_weights)

# crear un outlier de 3000 gramos
day4_with_outlier <- c(day4_weights, 3000)

# Calculando el promedio con el outlier
mean_with_outlier <- mean(day4_with_outlier)

# Calcular el ratio
ratio <- mean_with_outlier / mean_original

# Resultadps
cat("Promedio Original:", mean_original, "\n")
cat("Promedio con Atipico:", mean_with_outlier, "\n")
cat("Ratio:", ratio, "\n")


### Utilizando median en lugar del promedio (mean)

median_original <- median(day4_weights)
median_w_outliers <- median(day4_with_outlier)

ratio_median <- median_w_outliers/median_original


#### Desviacion standard 

sd_original <- sd(day4_weights)
sd_w_outliers <- sd(day4_with_outlier)
ratio_sd <- sd_w_outliers/sd_original

### Median Absolute Deviation MAD ###

mad_original <- mad(day4_weights)
mad_w_outlier <- mad(day4_with_outlier)

ratio_mad <- mad_w_outlier/mad_original


### Correlacion de Pearson y Spearman

boxplot(chick$weight.4, chick$weight.21)

day_21_weights <- chick$weight.21
day_21_with_outlier <- c(day_21_weights, 3000)

day_21_with_outlier
day4_with_outlier

pearson_cor <- cor(day4_weights, day_21_weights)
pearson_cor_w_outliers <- cor(day4_with_outlier, day_21_with_outlier)

ratio_pearson <- pearson_cor_w_outliers/pearson_cor
ratio_pearson
