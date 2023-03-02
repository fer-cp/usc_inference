rm(list=ls())

( x<-c(3.8,.1,1,.25,1.8,3.9,.2,3.5,3,.3) )

## Distribution under H0:
?Uniform
# https://en.wikipedia.org/wiki/Continuous_uniform_distribution
F0<-function(t)if(t<0)0 else if (t>4)1 else t/4  # CDF
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
source("01_generate_ks_table.R")
ks_table(n) # Equivalent to looking at the PDF table
Dn # We see that in this example: .1 < p-value < .2 (close to .2)
ks.test(x,"punif",min=0,max=4) # p-value ~= 0.19
