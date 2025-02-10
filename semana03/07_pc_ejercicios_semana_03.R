### Ejercicios Calculos de Poder semana 03 ###

#For these exercises we will load the babies dataset from babies.txt. We will use this data to review the concepts #behind p-values and then test confidence interval concepts.

library(tidyverse)
library(dslabs)
library(rafalib)
library(downloader)

babies <- read.table('datos/babies.txt', header = TRUE)

# This is a large dataset (1,236 cases), and we will pretend that it contains the entire population in which we are interested. We will study the differences in birth weight between babies born to smoking and non-smoking mothers.
# 
# First, let's split this into two birth weight datasets: one of birth weights to non-smoking mothers and the other of birth weights to smoking mothers.

bwt.nonsmoke <- babies %>% 
  filter(smoke == 0) %>% 
  select(bwt) %>% 
  unlist()

bwt.smoke <- babies %>% 
  filter(smoke == 1) %>% 
  select(bwt) %>% 
  unlist()

#Now, we can look for the true population difference in means between smoking and non-smoking birth weights.


mean(bwt.nonsmoke) - mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

# The population difference of mean birth weights is about 8.9 ounces. The standard deviations of the nonsmoking and smoking groups are about 17.4 and 18.1 ounces, respectively.
# 
# As we did with the mouse weight data, this assessment interactively reviews inference concepts using simulations in R. We will treat the babies dataset as the full population and draw samples from it to simulate individual experiments. We will then ask whether somebody who only received the random samples would be able to draw correct conclusions about the population.
# 
# We are interested in testing whether the birth weights of babies born to non-smoking mothers are significantly different from the birth weights of babies born to smoking mothers.


# Power Calculations Exercises #1
# 
# We can explore the trade off of power and Type I error concretely using the babies data. Since we have the full population, we know what the true effect size is (about 8.93) and we can compute the power of the test for true difference between populations.
# 
# Set the seed at 1 and take a random sample of N=5 measurements from each of the smoking and nonsmoking datasets. Use the t-test function to find the p-value. (Note that you already performed this calculation in the last assessment.)
# 
# The p-value is larger than 0.05 so using the typical cut-off, we would not reject. This is a type II error. Which of the following is *not* a way to decrease this type of error?


# La respuesta correcta a la pregunta planteada es:
#   
#   "Find a population for which the null is not true."
# 
# Razón:
#   Esta opción no es una forma válida de reducir el error de tipo II en términos estadísticos. El error de tipo II ocurre cuando no se rechaza una hipótesis nula que es falsa, y las estrategias válidas para reducir este error incluyen:
#   
#   Aumentar el tamaño de la muestra: Mejora el poder estadístico.
# Usar un nivel de significancia (
#   𝛼
#   α) más alto: Incrementa la probabilidad de rechazar la hipótesis nula.
# Aumentar el tamaño del efecto detectable: Esto está relacionado con el diseño del experimento, pero no depende de cambiar la población.
# Por otro lado, "encontrar una población para la cual la hipótesis nula no sea cierta" no es una estrategia válida ni científicamente correcta.



#Power Calculations Exercises #2

# Set the seed at 1, RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding"),  then use the replicate() function to repeat the code used in the exercise above 10,000 times. What proportion of the time do we reject at the 0.05 level?


set.seed(1)
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

reject_test <- function() {
  smoke_sample <- sample(bwt.smoke, 5)
  nonsmoke_sample <- sample(bwt.nonsmoke, 5)
  t.test(nonsmoke_sample, smoke_sample)$p.value < 0.05
}

power <- mean(replicate(10000, reject_test()))
power  



# Power Calculations Exercises #3
# 
# Note that, not surprisingly, the power is lower than 10%. Repeat the exercise above for samples sizes of 30, 60, 90 and 120. Which of those four gives you power of about 80%?
# 


set.seed(1)
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

calculate_power <- function(n) {
  reject_test <- function() {
    smoke_sample <- sample(bwt.smoke, n)
    nonsmoke_sample <- sample(bwt.nonsmoke, n)
    t.test(nonsmoke_sample, smoke_sample)$p.value < 0.05
  }
  mean(replicate(10000, reject_test()))
}

sample_sizes <- c(30, 60, 90, 120)
powers <- sapply(sample_sizes, calculate_power)
powers


#The sample size of 60 gives approximately 80% power (around 0.798 or 79.8%).

# Power Calculations Exercises #4

# Repeat the problem above, but now require an alpha level of 0.01. Which of those four gives you power of about 80%?

set.seed(1)
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

calculate_power <- function(n) {
  reject_test <- function() {
    smoke_sample <- sample(bwt.smoke, n)
    nonsmoke_sample <- sample(bwt.nonsmoke, n)
    t.test(nonsmoke_sample, smoke_sample)$p.value < 0.01
  }
  mean(replicate(10000, reject_test()))
}

sample_sizes <- c(30, 60, 90, 120)
powers <- sapply(sample_sizes, calculate_power)
powers

## Respusta 90


# Power Calculations Exercises #5
# 
# Consider this statement: In a world where the alternative hypothesis is true, if you fail to reject the null hypothesis because the p value exceeds 0.05 and the power of your test is 90%, the chance that your finding is a false negative is 10%.
