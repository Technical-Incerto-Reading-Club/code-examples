#### Purpose: Demonstrate extreme value theory on UK Equity Data
#### Author: James Sharpe
#### License: MIT
#### Global Technical Incerto Reading Club

library(QRM)
library(data.table)
library(ggplot2)
library(dplyr)

ret_dat = fread("C:/Users/Setup/Documents/Book club/1. EVT/EVT presentation/boe_millenium_equity.csv")
ret_dat[, annualized_return := shift(Index,12, type = "lead")/Index - 1]

ret_dat %>% ggplot(aes(x = annualized_return))+geom_density()

ret_dat %>% ggplot(aes(x = paste0(Year, Month), y = annualized_return))+
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8))+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

UK = ret_dat[!is.na(annualized_return)]$annualized_return

UK %>% QRM::hillPlot()

UKGPD <- fit.GPD(-UK,0.4,type="pwm") #MLE by default
UKGPD$par.ests
RiskMeasures(UKGPD,0.995)

#######various thresholds


i<-0.01;y<-0;x<-0;z<-0;w<-0;jj<-0;j<-0;p<-0;q<-0
ypwm<-0;xpwm<-0;zpwm<-0;wpwm<-0;a<-0;b<-0;c<-0;d<-0
for (j in 1:2400){
  
  GPD <- fit.GPD(-UK,i)#,method="pwm") #MLE by default
  GPDpwm <- fit.GPD(-UK,i, type="pwm") #MLE by default
  
  i<-i+0.0001
  y[j]<-RiskMeasures(GPD,0.995)[2]
  x[j]<-GPD$n.exceed
  
  xpwm[j]<-GPDpwm$n.exceed
  ypwm[j]<- RiskMeasures(GPDpwm,0.995)[2]
  p[j]<-GPD$par.ses[1]
  q[j]<-GPD$ll.max/GPD$n.exceed
  z[j]<-GPD$par.ests[1]
  w[j]<-GPD$par.ests[2]
  
  jj[j]<-j
}

par(mfrow=c(2,2))
plot(jj/100,x,type="l",xlab="Threshold%",ylab="Number of Points",main="Number of calibration points")
plot(jj/100,y,type="l",xlab="Threshold%",ylab="99.5th stress",main="99.5th percentile Maximum Likelihood")
plot(jj/100,z,type="l",xlab="Threshold%",ylab="Shape parameter",main="Shape parameter")
plot(jj/100,p,type="l",xlab="Threshold%",ylab="Standard Error",main="Standard Error of the Shape parameter")
plot(jj/100,q,type="l",xlab="Threshold%",ylab="Averaged Log Likelihood",main="Maximum Log Likelihood Averaged")

par(mfrow=c(1,2))
plot(jj/100,xpwm,type="l",xlab="Threshold%",ylab="Number of Points",main="Number of calibration points")
plot(jj/100,ypwm,type="l",xlab="Threshold%",ylab="99.5th stress",main="99.5th percentile Probability Weighted Moments")

par(mfrow=c(1,3))
plot(jj/100,x,type="l",xlab="Threshold%",ylab="Number of Points",main="Number of calibration points")
plot(jj/100,y,type="l",xlab="Threshold%",ylab="99.5th stress",main="99.5th percentile Maximum Likelihood")
plot(jj/100,ypwm,type="l",xlab="Threshold%",ylab="99.5th stress",main="99.5th percentile Probability Weighted Moments")
