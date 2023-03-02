rm(list=ls())

( x<-c(38, 30, 24, 27, 20, 31, 22, 32) )

## Distribution under H0:
?Normal
# https://en.wikipedia.org/wiki/Normal_distribution
mu<-mean(x)
sigma<-sd(x)
F0<-function(t)pnorm(t,mu,sigma) # CDF
y<-sapply(x,F0)
plot(x,y,type="l")

## Table for computing the KS test statistic:
( n<-length(x) )
x<-sort(x) # Do not forget!!
y<-sapply(x,F0)
a1<-1:n
a2<-a1/n
a3<-a2-y
a4<-y-(a1-1)/n
A<-cbind(a1,x,y,a2,a3,a4)
colnames(A)<-c("j","X(j)","F0(X(j))","j/n",
               "j/n-F0(X(j))","F0(X(j))-(j-1)/n")
rownames(A)<-a1
View(A)

## KS statistic:
( Dn_plus<-max(c(0,a3)) )
( Dn_minus<-max(c(0,a4)) )
( Dn<-max(Dn_plus,Dn_minus) )

## P-value:
# Session > Set WD > To source fi location
source("01_generate_lillie_table.R")
lillie_table(n) # Equivalent to looking at the PDF table
Dn # We see that in this example: p-value >> .2
# install.packages("nortest")
nortest::lillie.test(x) # p-value ~= 0.95
