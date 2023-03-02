rm(list=ls())

( N<-matrix(c(12,50,60,10,58,10),ncol=3,nrow=2) )
( n<-sum(N) )
( Nj<-rowSums(N) )
( ni<-colSums(N) )
( e<-Nj%o%ni/n )
( D<-sum((N-e)^2/e) )

1-pchisq(D,(nrow(N)-1)*(ncol(N)-1))
chisq.test(N)