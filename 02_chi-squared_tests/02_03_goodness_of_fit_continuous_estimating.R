rm(list=ls())

x<-c(1.2, 2.3, -1.4, -.4, -.6, 3.2, 3.9, -2.5,
     .8, -.1, 1.3, .2, 3.8, 4.1, -2.6, 2.4,
     -4.1, -2.6, .6, -.3, 1.5, 1.9, -2.7, -2.4,
     -3.7, .7, -.2, .5, -1.2, 1.7)
( n<-length(x) )
( k<-floor(sqrt(n)) ) # "Not round", nor "ceiling"
pr<-(0:k)/k
ties<-qnorm(pr,mean(x),sd(x))
( N<-table(c(cut(x,breaks=ties))) )

( e<-rep(n/k,k) )
all(e>=5)
( D<-sum((N-e)^2/e) )
1-pchisq(D,k-1-2)

# ?chisq.test
chisq.test(N)$statistic # Incorrect df's
