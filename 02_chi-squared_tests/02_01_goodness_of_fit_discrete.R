rm(list=ls())

N<-c(10,20,15,5)
( n<-sum(N) )
( k<-length(N) )
( e<-rep(n/k,k) )
( D<-sum((N-e)^2/e) )
1-pchisq(D,k-1)

# ?chisq.test
chisq.test(N)
