###Ejercicios simulaciones de montecarlo

library(tidyverse)
library(rafalib)
library(dslabs)


# Set the seed at 1, then use rnorm() to generate a random sample of size 5, X1,...,X5, from a standard normal distribution, then compute the t-statistic t = \sqrt{5} \, \bar{X} / s with s the sample standard deviation. What value do you observe?

RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

set.seed(1)
x <- rnorm(5)  # Generate 5 standard normal random numbers
mean_x <- mean(x)
sd_x <- sd(x)
t_stat <- sqrt(5) * mean_x / sd_x
t_stat  # 0.30077



# You have just performed a Monte Carlo simulation using rnorm() , a random number generator for normally distributed data. Gosset's mathematical calculation tells us that the t-statistic defined in the previous exercises, a random variable, follows a t-distribution with N-1 degrees of freedom. Monte Carlo simulations can be used to check the theory: we generate many outcomes and compare them to the theoretical result. Set the seed to 1, then generate B = 1000 t-statistics as done in exercise 1. What proportion is larger than 2?

RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
set.seed(1)

N <- 5
B <- 1000

t_stats <- replicate(B, {
 X <- rnorm(N)
 sqrt(N) * mean(X) / sd(X)
})
mean(t_stats > 2)


1 - pt(2, df=4)

# The answer to exercise 2 is very similar to the theoretical prediction: 1-pt(2,df=4). We can check several such quantiles using the qqplot function.



# The answer to exercise 2 is very similar to the theoretical prediction: 1-pt(2,df=4). We can check several such quantiles using the qqplot function.
# 
# To obtain quantiles for the t-distribution we can generate percentiles from just above 0 to just below 1: B=100; ps = seq(1/(B+1), 1-1/(B+1),len=B), and compute the quantiles with qt(ps,df=4). Now we can use qqplot() to compare these theoretical quantiles to those obtained in the Monte Carlo simulation. Use Monte Carlo simulation developed for exercise 2 to corroborate that the t-statistic t = \sqrt{N} \, \bar{X} / s follows a t-distribution for several values of  (try Ns <- seq(5,30,5)).


RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
set.seed(1)
N <- 5
B <- 1000
ps <- seq(1/(B+1), 1-1/(B+1), len=B)
Ns <- seq(5,30,5)

# Function to generate t-statistics
sim_t_stats <- function(N, B) {
  replicate(B, {
    X <- rnorm(N)
    sqrt(N) * mean(X) / sd(X)
  })
}

# Create plot
par(mfrow=c(2,3))
for(N in Ns) {
  t_stats <- sim_t_stats(N, B)
  qqplot(qt(ps, df=N-1), t_stats,
         main=paste("N =", N),
         xlab="Theoretical Quantiles",
         ylab="Sample Quantiles")
  abline(0,1)
}



#Use Monte Carlo simulation to corroborate that the t-statistic comparing two means and obtained with normally distributed (mean 0 and sd) data follows a t-distribution. In this case we will use the t.test() function with var.equal=TRUE. With this argument the degrees of freedom will be df=2*N-2 with N the sample size. For which sample sizes does the approximation best work?

RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
set.seed(1)
B <- 1000
ps <- seq(1/(B+1), 1-1/(B+1), len=B)
Ns <- seq(5,30,5)

sim_t_stats <- function(N, B) {
  replicate(B, {
    x <- rnorm(N)
    y <- rnorm(N) 
    t.test(x, y, var.equal=TRUE)$statistic
  })
}

par(mfrow=c(2,3))
for(N in Ns) {
  t_stats <- sim_t_stats(N, B)
  qqplot(qt(ps, df=2*N-2), t_stats,
         main=paste("N =", N),
         xlab="Theoretical Quantiles",
         ylab="Sample Quantiles")
  abline(0,1)
}


#Is the following statement true or false? If instead of generating the sample with X=rnorm(15) we generate it with binary data (either positive or negative 1 with probability 0.5) X =sample(c(-1,1), 15, replace=TRUE) then the t-statistic

RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
set.seed(1)

N <- 15
B <- 1000
t_stats <- replicate(B, {
  X <- sample(c(-1,1), 15, replace=TRUE)
  sqrt(N) * mean(X) / sd(X)
})

ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), t_stats, xlim=range(t_stats))
abline(0,1)   

###Respuesta es FALSO


#Is the following statement true or false ? If instead of generating the sample with X=rnorm(N) with N=1000, we generate the data with binary data X= sample(c(-1,1), N, replace=TRUE), then the t-statistic sqrt(N)*mean(X)/sd(X) is approximated by a t-distribution with 999 degrees of freedom.

RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
set.seed(1)
N <- 1000
B <- 10000
tstats <- replicate(B, {
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
qqplot(qt(ppoints(B), df=N-1), tstats)
abline(0,1)

### Verdadero


# We can derive approximation of the distribution of the sample average or the t-statistic theoretically. However, suppose we are interested in the distribution of a statistic for which a theoretical approximation is not immediately obvious.
# 
# Consider the sample median as an example. Use a Monte Carlo to determine which of the following best approximates the median of a sample taken from normally distributed population with mean 0 and standard deviation 1.


RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
set.seed(1)
B <- 10000
N <- 30
medians <- replicate(B, {
  x <- rnorm(N)
  median(x)
})

# Compare to normal distribution with theoretical standard error
se <- 1.2533/(sqrt(N))  # 1.2533 is from asymptotic theory
z_scores <- medians/(se)

# QQ plot
qqnorm(z_scores)
abline(0,1)

RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
set.seed(1)
B <- 10000
N <- 30
medians <- replicate(B, {
  x <- rnorm(N)
  median(x)
})

# Check standard deviation
sd(medians)  # ~0.23, which is larger than 1/sqrt(30) ≈ 0.18

# QQ plot
qqnorm(medians)
abline(0,1)
