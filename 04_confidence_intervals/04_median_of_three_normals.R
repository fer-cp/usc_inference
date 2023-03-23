rm(list=ls())

#### Monte-Carlo method for checking the formula for G ####
set.seed(1)
R<-1E5
x<-replicate(R, median(rnorm(3)))
summary(x)
G<-function(z)pnorm(z)^2*(3-2*pnorm(z))
G_hat<-function(z)mean(x<z)

v<-(-10:10)/10
A<-cbind(v,sapply(v,G),sapply(v,G_hat))
colnames(A)<-c("z","G(z)","G_hat(z)")
A[,2:3]<-round(A[,2:3],4)
A
max(abs(apply(A[,2:3],1,diff)))<.1^floor(log10(R)/2) # TRUE


#### Confidence interval ####
## Quantile "a" via Newton-Raphson's method:
alpha<-.05
alpha1<-alpha/2
alpha2<-alpha/2

G<-function(z)pnorm(z)^2*(3-2*pnorm(z))
g<-function(z)6*dnorm(z)*pnorm(z)*(1-pnorm(z))

qmedian<-function(p){
  y<-0
  niter<-0
  while(abs(G(y)-p)>1E-3){
    y<-y-(G(y)-p)/g(y)
    niter<-niter+1
    if(niter>100)break
  }
  return(y)
}

( a<-qmedian(alpha1) )
( b<-qmedian(1-alpha2) )
all.equal(abs(a),b) # TRUE
plot(g,xlim=c(-3,3)) # Symmetric

set.seed(1)
R<-1E5
theta<-3.2
x<-replicate(R, abs(median(rnorm(3,mean=theta))-theta)<abs(b) )
round(mean(x),floor(log10(R)/2)) # 0.95
