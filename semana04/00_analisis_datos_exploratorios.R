### Analisis de datos exploratorio ###

library(tidyverse)
library(rafalib)
library(dslabs)


data(father.son,package="UsingR") ##available from CRAN
x <- father.son$fheight
y <- father.son$sheight


plot(x,y, xlab = "Father's height in inches", 
     ylab = "Son's height in inches", 
     main = paste("correlation =", signif(cor(x,y), 2)))


split(y, round(x))
boxplot(split(y, round(x)))
mean(y[round(x) == 72])


x <- (x-mean(x))/sd(x)
y <- (y-mean(y))/sd(y)


means <- tapply(y, round(x*4)/4, mean)
fatherheights <- as.numeric(names(means))

plot(fatherheights, means, 
     ylab = "average of  strata of son  heights", 
     ylim = range(fatherheights))

abline(0, cor(x,y))


### No correlacionados

set.seed(1)
a <- rnorm(100)
a[1] <- 25
b <- rnorm(100)
b[1] <- 26

plot(a, b, 
     main = paste("Correlation =", signif(cor(a,b), 2)))


cor(a,b, method = "spearman")


ps <- ( seq(0,99) + 0.5 )/100 
qs <- quantile(x, ps)
normalqs <- qnorm(ps, mean(x), popsd(x))
plot(normalqs,qs,xlab="Normal percentiles",ylab="Height percentiles")
abline(0,1) ##identity line


qqnorm(x)
qqline(x) 

n <-1000
x <- rnorm(n)
qqnorm(x)
qqline(x)

dfs <- c(3,6,12,30)
mypar(2,2)
for(df in dfs){
  x <- rt(1000,df)
  qqnorm(x,xlab="t quantiles",main=paste0("d.f=",df),ylim=c(-6,6))
  qqline(x)
}

data(exec.pay,package="UsingR")
mypar(1,2)
hist(exec.pay) 
qqnorm(exec.pay)
qqline(exec.pay)

boxplot(exec.pay, ylab="10,000s of dollars", ylim=c(0,400))

data(father.son,package="UsingR")
x=father.son$fheight
y=father.son$sheight
plot(x,y, xlab="Father's height in inches", 
     ylab="Son's height in inches", 
     main=paste("correlation =",signif(cor(x,y),2)))

groups <- split(y,round(x)) 
boxplot(groups)
print(mean(y[ round(x) == 72]))

groups <- split(y,round(x)) 
mypar(2,2)
for(i in c(5,8,11,14)){
  qqnorm(groups[[i]],main=paste0("X=",names(groups)[i]," strata"),
         ylim=range(y),xlim=c(-2.5,2.5))
  qqline(groups[[i]])
}

x=( x-mean(x) )/sd(x)
y=( y-mean(y) )/sd(y)
means=tapply(y, round(x*4)/4, mean)
fatherheights=as.numeric(names(means))
mypar(1,1)
plot(fatherheights, means, ylab="average of strata of son heights", ylim=range(fatherheights))
abline(0, cor(x,y))

