### Plots que debe de evitar##

library(tidyverse)
library(dslabs)
library(rafalib)
library(UsingR)


set.seed(12201970)
before <- runif(6, 5, 8)
after <- rnorm(6, before*1.05, 2)
li <- range(c(before, after))
ymx <- max(abs(after-before))

mypar(1,2)
plot(before, after, xlab="Before", ylab="After",
     ylim=li, xlim=li)
abline(0,1, lty=2, col=1)

plot(before, after-before, xlab="Before", ylim=c(-ymx, ymx),
     ylab="Change (After - Before)", lwd=2)
abline(h=0, lty=2, col=1)


z <- rep(c(0,1), rep(6,2))
mypar(1,2)
plot(z, c(before, after),
     xaxt="n", ylab="Response",
     xlab="", xlim=c(-0.5, 1.5))
axis(side=1, at=c(0,1), c("Before","After"))
segments(rep(0,6), before, rep(1,6), after, col=1)     

boxplot(before,after,names=c("Before","After"),ylab="Response")


url <- "https://github.com/kbroman/Talk_Graphs/raw/master/R/fig8dat.csv"
x <- read.csv(url)

##Now make alternative plot
plot(x[,1],x[,2],xlab="log Dose",ylab="Proportion survived",ylim=c(0,1),
     type="l",lwd=2,col=1)
lines(x[,1],x[,3],lwd=2,col=2)
lines(x[,1],x[,4],lwd=2,col=3)
legend(1,0.4,c("Drug A","Drug B","Drug C"),lwd=2, col=1:3)



set.seed(12201970)

x <- 1:8
ilogit <- function(x) exp(x)/(1+exp(x))
y1 <- 0.9 - x/80 + rnorm(length(x), 0, 0.02)
y2 <- 0.9 - x/40 + rnorm(length(x), 0, 0.02)
y3 <- 0.85 - x/30 + rnorm(length(x), 0, 0.02)
y <- cbind(y1, y2, y3)

z1 <- 0.95 - x/40 + rnorm(length(x), 0, 0.02)
z2 <- ilogit(-0.4*(x-4.5) + rnorm(length(x), 0, 0.04))
z3 <- ilogit(-0.5*(x-4.5) + rnorm(length(x), 0, 0.04))
z1[6:8] <- z1[6:8] - 0.18*3
z <- cbind(z1, z2, z3)
ym <- apply(y, 1, mean)
zm <- apply(z, 1, mean)


plot(x, y1, ylim=c(0,1), type="n", xlab="Dose", ylab="Response") 
for(i in 1:3) lines(x, y[,i], col=1, lwd=1, lty=2)
for(i in 1:3) lines(x, z[,i], col=2, lwd=1, lty=2)
lines(x, ym, col=1, lwd=2)
lines(x, zm, col=2, lwd=2)
legend("bottomleft", lwd=2, col=c(1, 2), c("Control", "Treated"))


heights <- cbind(rnorm(8,73,3),rnorm(8,73,3),rnorm(8,80,3),
                 rnorm(8,78,3),rnorm(8,78,3))
colnames(heights)<-c("SG","PG","C","PF","SF")
rownames(heights)<- paste("team",1:8)
heights


round(heights,1)

#### Ejemplo 

data("divorce_margarine")
plot(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine)
cor(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine)


