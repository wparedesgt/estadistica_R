#### Robust Summaries ###

library(tidyverse)
library(dslabs)
library(rafalib)

set.seed(1)
x=c(rnorm(100,0,1)) ##real distribution
x[23] <- 100 ##mistake made in 23th measurement
boxplot(x)

cat("The average is",mean(x),"and the SD is",sd(x))

median(x)

mad(x)


 set.seed(1)
x=rnorm(100,0,1) ##real distribution
x[23] <- 100 ##mistake made in 23th measurement
y=rnorm(100,0,1) ##real distribution
y[23] <- 84 ##similar mistake made in 23th measurement
library(rafalib)
mypar()
plot(x,y,main=paste0("correlation=",round(cor(x,y),3)),
     pch=21,bg=1,xlim=c(-3,100),ylim=c(-3,100))
abline(0,1)


mypar(1,2)
plot(x,y,main=paste0("correlation=",round(cor(x,y),3)),pch=21,bg=1,xlim=c(-3,100),ylim=c(-3,100))
plot(rank(x),rank(y), main=paste0("correlation=",round(cor(x,y,method="spearman"),3)),
     pch=21,bg=1,xlim=c(-3,100),ylim=c(-3,100))
abline(0,1)


x <- 2^(rnorm(100))
y <- 2^(rnorm(100)) 
ratios <- x / y 


mypar(1,2)
hist(ratios)

logratios <- log2(ratios)
hist(logratios)



x=2^seq(1,5)
y=c(rev(1/x),1,x)
Names=c(paste0("1/",rev(x)),1,x)
mypar(1,2)
plot(seq(along=y),y,xlab="",ylab="",type="n",xaxt="n")
text(seq(along=y),y,Names,cex=1.2)
abline(h=1)
plot(seq(along=y),y,xlab="",ylab="",type="n",log="y",xaxt="n")
text(seq(along=y),y,Names,cex=1.2)
abline(h=1)
