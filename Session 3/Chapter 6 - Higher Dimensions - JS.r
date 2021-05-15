#Section 6.4

x<-seq(-10,10,1)
y<-x^2
z<-x
a<-rep(1,21)



plot(x,y,type="l",ylim=c(-10,100))
lines(x,z,col="red")
lines(x,a,col="blue")

cor(y,x)
cor(x,z)


library(infotheo)
mutinformation(x,a)
mutinformation(x,z)
mutinformation(x,y)


#Section 6.5
RandMatrixNormal <-function(n){
mats<-matrix(rnorm(n^2,0,1),ncol=n)
return((mats+t(mats))/2)
}

RandMatrixStudent <-function(n,df){
  mats<-matrix(rt(n^2,df,0),ncol=n)
  return((mats+t(mats))/2)
}

Normal<-eigen(RandMatrixNormal(1000))
Student3<-eigen(RandMatrixStudent(1000,3))
Cauchy<-eigen(RandMatrixStudent(1000,1))

hist(Normal$values)
hist(Student3$values)
hist(Cauchy$values)




#Section 6.7

#Y= aX+b +e


sampleCauchy<-function(n,a,b){
  
  X<-seq(1,n,1)
  e<-rcauchy(n,0,1)
  Y<-a*X+b+e
  return(Y)
}

xx<-sampleCauchy(50,2,15)

plot(xx)


