library(rafalib)
library(tidyverse)

num_vec <- c(2.23,3.45,1.87,2.11,7.33,18.34,19.23)
mean(num_vec)

# Inicializar la variable de la suma
suma <- 0

# Usar un bucle for para iterar desde 1 hasta 25
for (i in 1:25) {
  suma <- suma + i^2
}

# Imprimir el resultado
print(suma)

class(cars)
str(cars)
mean(cars[,2])
which(cars$dist == 85)
x <- 1:10
y <- rnorm(10)
plot(x,y)

dat <- read.csv("datos/femaleMiceWeights.csv")
names(dat)
dat[12,2]
dat$Bodyweight[11]
length(dat$Diet)

mean(dat[which(dat$Diet == 'hf'),2])
set.seed(1)
sample(dat[13:24,2], size = 1)


####dplyr####

controls <- dat %>% 
  filter(Diet == 'chow')

controls <- select(controls, Bodyweight)
unlist(controls)

controls <- dat %>%
  filter(Diet == 'chow') %>% 
  select(Bodyweight) %>% 
  unlist()
mean(controls)

##### dlpyr Exercises #####


