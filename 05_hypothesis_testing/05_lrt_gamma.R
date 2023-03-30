rm(list=ls())

phi<-function(x,alpha=.05){
  n<-length(x)
  c<-qchisq(alpha,4*n)/(2*n)
  return(mean(x)<c)
}

beta_phi<-function(theta,n=10,alpha=.05,R=1E5){
  dig<-1-floor(log10(1/sqrt(R)))
  r<-replicate( R, phi(rgamma(n,shape=2,rate=1/theta), alpha) )
  return( round(mean(r), dig) )
}

set.seed(1)
beta_phi(1) # 0.05
beta_phi(.5) # 0.92
beta_phi(.01) # 1
beta_phi(1.1) # 0.02
beta_phi(2) # 0
beta_phi(300) # 0

x1<-c(0.37, 3.44, 1.68, 3.49, 1.50, 3.75, 3.04, 3.65, 0.50, 1.39)
phi(x1) # FALSE (do not reject)
mean(x1) # < 2

x2<-c(0.64, 0.33, 2.76, 1.22, 1.24, 1.64, 0.29, 0.61, 0.89, 0.23)
phi(x2) # TRUE (reject)
mean(x2) # << 2
