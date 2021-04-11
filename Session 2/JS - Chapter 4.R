#chapter4

library(moments)

#Page 67

#4.1.1 a variance preserving heuristic
rnorma<-function(a,n){
  c<-rbinom(n,1,0.5)
  b<-rnorm(n,0,sqrt(1+a))*c+rnorm(n,0,sqrt(1-a))*(1-c)
  return(b)
}

#Calculate variance and kurtosis for various values of a
#Standard Normal has variance 1 and Kurtosis 3
a<-0.0001
var(rnorma(a,10000))
kurtosis(rnorma(a,10000))
#for very small value of a, kurtosis is close to 3


a<-0.999999
var(rnorma(a,10000))
kurtosis(rnorma(a,10000))

# for large value of a, kurtosis is close to 6
#variance is close to 1 in all cases

#Section 4.1.2 Page 68
#Fattening tails with skewed variance

rnormaskew<-function(a,p,n){
  b<-(-a*(p/(1-p)))
  c<-rbinom(n,1,p)
  z<-rnorm(n,0,sqrt(1+a))*c+rnorm(n,0,sqrt(1+b))*(1-c)
  return(z)
}

kurtosis(rnormaskew(998,0.001,100000))
hist(rnormaskew(998,0.001,3000))
plot(rnormaskew(998,0.001,3000),type="l")
#Kurtosis close to 3000

#Normal Distribution with Variance that is Gamma distributed
#Otherwise known as the Normal Inverse Gamma
library(GeneralizedHyperbolic)
?rnig
plot(rnig(1000,0,1,1,0.9),type="l")
lines(rnorm(1000,0,1),col="red")


