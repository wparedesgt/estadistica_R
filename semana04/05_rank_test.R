#### Prueba de Rangos #### 

library(tidyverse)
library(dslabs)
library(rafalib)


set.seed(779) ##779 picked for illustration purposes
N=25
x<- rnorm(N,0,1)
y<- rnorm(N,0,1)


x[1] <- 5
x[2] <- 7
cat("t-test pval:",t.test(x,y)$p.value)
cat("Wilcox test pval:",wilcox.test(x,y)$p.value)


mypar(1,2)

stripchart(list(x,y),vertical=TRUE,ylim=c(-7,7),ylab="Observations",pch=21,bg=1)
abline(h=0)

xrank<-rank(c(x,y))[seq(along=x)]
yrank<-rank(c(x,y))[-seq(along=x)]

stripchart(list(xrank,yrank),vertical=TRUE,ylab="Ranks",pch=21,bg=1,cex=1.25)

ws <- sapply(x,function(z) rank(c(z,y))[1]-1)
text( rep(1.05,length(ws)), xrank, ws, cex=0.8)
W <-sum(ws) 


n1<-length(x);n2<-length(y)
Z <- (mean(ws)-n2/2)/ sqrt(n2*(n1+n2+1)/12/n1)
Z

