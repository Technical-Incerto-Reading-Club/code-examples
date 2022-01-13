library(QRM)
x<-danish$FIRE.LOSSES
plot(x)
hist(x)
MEplot(x, main = "Mean-Excess Plot", xlab = "Threshold", ylab = "Mean Excess")

#calculate hill estimator of the Pareto shape parameter
y<-rep(0,60)
for(i in 2:60){
  y[i]<-hill(x,k=i)
}

#this is the quantile function of transformed data
quantilewithMax<-function(p,alpha,sigma,H,L){
  gamma<-(alpha*sigma*(1-p)^(-1/alpha))/H
  q<-exp(-gamma)*(L*exp(alpha*sigma/H)+H*exp(gamma)-H*exp(alpha*sigma/H))
  return(q)
}

#Test danish fire data with different assumed maximums


H2<-max(x)*2
H5<-max(x)*5
H10<-max(x)*10


#Calculate the VaR at 99th percentile for various thresholds
z<-rep(0,60)
n99th<-rep(0,60)
n99thhill<-rep(0,60)

for(i in 2:60){
  zz<-fit.GPD(x,threshold=i)
  z[i]<-fit.GPD(x,threshold=i)$par.ests[[1]]
  n99th[i]<-qGPD(0.99,xi=zz$par.ests[[1]],beta=zz$par.ests[[2]])
  n99thhill[i]<-qGPD(0.99,xi=1/y[i],beta=zz$par.ests[[2]])
}

#Plot estimates of Pareto shape parameter using Hill and MLE on GPD shape
plot(y[2:60],type="l",ylim=c(0,3.5),ylab="Pareto Sharpe Parameter Estimate",xlab="Threshold",col="blue", main="Hill Estimate of Pareto Shape Parameter")
lines(1/z[2:50],col="red")

#Data conversion function

HConvert<-function(x,H,L){
  z<-L - H * log((H-x)/(H-L))
  return(z)
}

#Reverse of the conversion function
HReverse<-function(z,H,L){
  x <- H - (H-L) *exp((L-z)/H)
  return(x)
}

#Quantile function on transformed data in original parameter space
quantilewithMax<-function(p,alpha,sigma,H,L){
  gamma<-alpha*sigma*(1-p)^(-1/alpha)/H
  q<-exp(-gamma)*(L*exp(alpha*sigma/H)+H*exp(gamma)-H*exp(alpha*sigma/H))
  return(q)
}

#plot the 99th percentile at different thresholds with different maximums
xH2<-HConvert(x,H2,0)
xH5<-HConvert(x,H5,0)
xH10<-HConvert(x,H10,0)

n99thH22<-rep(0,60)
n99thH52<-rep(0,60)
n99thH102<-rep(0,60)

n99thH2<-rep(0,60)
n99thH5<-rep(0,60)
n99thH10<-rep(0,60)

for(i in 2:60){
  zzH2<-fit.GPD(xH2,threshold=i)
  zzH5<-fit.GPD(xH5,threshold=i)
  zzH10<-fit.GPD(xH10,threshold=i)
  
  n99thH2[i]<-qGPD(0.99,xi=zzH2$par.ests[[1]],beta=zzH2$par.ests[[2]])
  n99thH5[i]<-qGPD(0.99,xi=zzH5$par.ests[[1]],beta=zzH5$par.ests[[2]])
  n99thH10[i]<-qGPD(0.99,xi=zzH10$par.ests[[1]],beta=zzH10$par.ests[[2]])
  
  n99thH22[i]<-quantilewithMax(0.99,1/zzH2$par.ests[[1]],zzH2$par.ests[[2]], H2,0)
  n99thH52[i]<-quantilewithMax(0.99,1/zzH5$par.ests[[1]],zzH5$par.ests[[2]], H5,0)
  n99thH102[i]<-quantilewithMax(0.99,1/zzH10$par.ests[[1]],zzH10$par.ests[[2]], H10,0)
  
}

#test<-fit.GPD(x,threshold=20)
#qGPD(0.99,xi=test$par.ests[[1]],beta=test$par.ests[[2]])

plot(n99th[2:40],type="l",ylim=c(0,1000),main="Danish Fire Data - 99th Percentile Various Thresholds/Assumed Max",xlab="Threshold", ylab="99th percentile loss (DKK millions)")
plot(n99thhill[2:40],type="l",main="Danish Fire Data - 99th Percentile Various Thresholds/Assumed Max",xlab="Threshold", ylab="99th percentile loss (DKK millions)")
lines(HReverse(n99thH2[2:40],H2,0),col="red")

lines(HReverse(n99thH5[2:40],H5,0),col="blue")
lines(HReverse(n99thH10[2:40],H10,0),col="green")
legend(25, 250, c("No Max","Max = 10 * Empirical Max","Max = 5 * Empirical Max","Max = 2 * Empirical Max"), col = c("black","green","blue","red"),
       text.col = "green4", lty = c(1, 1,1,1),merge = TRUE, bg = "gray90", cex=0.7)

lines(n99thhill[2:40],col="purple")

lines(n99thH22[2:40],col="red")
lines(n99thH52[2:40],col="blue")
lines(n99thH102[2:40],col="green")

#confirmed the same running updated quantile function and reversing transform




