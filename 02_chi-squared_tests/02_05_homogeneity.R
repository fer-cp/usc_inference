rm(list=ls())

( N<-10*matrix(c(4,6,3,3,10,4,2,2,2,1,2,1),ncol=4,nrow=3) )
( n<-sum(N) )
( ni<-rowSums(N) )
( Nj<-colSums(N) )
( e<-ni%o%Nj/n )
( D<-sum((N-e)^2/e) )

1-pchisq(D,(nrow(N)-1)*(ncol(N)-1))
chisq.test(N)
