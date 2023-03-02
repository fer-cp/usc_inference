ks_table<-function(n, R=1E5,
                   alphas=c(.2,.1,.05,.02,.01,.001)){

# set.seed(123456)

Ds<-rep(NA,R)
for (r in 1:R){
  x <- runif(n)
  Ds[r] <- ks.test(x, "punif")$statistic
  if(r%%1000==0) cat(100*r/R, "% complete.\n")
}
digits<-ceiling(1+.5*log10(R))
return( round(quantile(Ds,1-alphas), digits) )

}

# ks_table(n=4)
