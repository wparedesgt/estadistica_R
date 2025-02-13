### Scatterplot Excercises ###
library(tidyverse)
library(dslabs)
library(rafalib)
library(UsingR)

data("nym.2002")

write.csv(nym.2002, 'datos/nym.2002.csv', row.names = FALSE)

nym.2002

males <- nym.2002 %>% 
  filter(gender == 'Male')

females <- nym.2002 %>% 
  filter(gender == 'Female')

cor(males$age, males$time, method = "pearson")
cor(females$age, females$time, method = "pearson")


# Leer los datos
datos <- nym.2002

# Crear los grupos de edad (redondeando a grupos de 5)
edad_grupos <- floor(datos$age/5) * 5
edad_grupos <- paste(edad_grupos, edad_grupos + 5, sep="-")

# Usar split() para dividir los tiempos por grupos de edad
grupos_tiempo <- split(datos$time, edad_grupos)

# Crear boxplot
boxplot(grupos_tiempo,
        xlab = "Grupos de edad",
        ylab = "Tiempo de carrera",
        main = "Distribución de tiempos por grupo de edad",
        las = 2)  # las = 2 hace que las etiquetas sean más legibles

# Calcular estadísticas por grupo
estadisticas <- lapply(grupos_tiempo, function(x) {
  c(promedio = mean(x, na.rm = TRUE),
    mediana = median(x, na.rm = TRUE),
    n = length(x))
})

# Convertir a data frame para mejor visualización
resultados <- do.call(rbind, estadisticas)
print(resultados)

# Para ver un grupo específico, por ejemplo 20-25:
print(paste("Estadísticas para grupo 20-25:"))
print(grupos_tiempo[["20-25"]])


#### Log Ratios Exercises ###

time <- sort(nym.2002$time)

min(time)/median(time)
max(time)/median(time)
