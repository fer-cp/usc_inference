rm(list=ls())


N<-c(22,53,58,39,20,8)
( k<-length(N) )
( n<-sum(N) )
( lambda<-sum((0:4)*N[-6],5*5,6*2,7*1)/n )
p<-c(dpois(0:4,lambda),1-ppois(4,lambda))
e<-n*p
all(e>=5)
( D<-sum((N-e)^2/e) )
1-pchisq(D,k-1-1)
