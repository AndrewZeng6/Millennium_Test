library("dplyr")
library("plyr")
library("lubridate")
setwd("C:/Users/AndrewZeng/Desktop/2016/Interview_Prepare/Millennium")

returns_raw=read.csv("input\\return.csv")
returns_raw$Date=as.POSIXct(returns_raw$Date)#Convert to date time 
returns=returns_raw[returns_raw$Date<as.POSIXct("2016-01-01"),]#only choose dates before 2016-01-01

a=0.05
weights=matrix(c(0.15,0.15,0.15,0.2,0.05,0.2,0.1),ncol=1)
N=252#annualize
eps=0.00001
C=cov(returns[,2:dim(returns)[2]])
mu_daily=colMeans(returns[,2:dim(returns)[2]])
returns$Portfolio=as.matrix(returns[,2:dim(returns)[2]]) %*% weights#Portfolio return



#Part 1: historical
VaR1=-quantile(returns$Portfolio,a)*sqrt(N)
CVaR1=sum(sapply(seq(eps,a,by=eps),function(x){-quantile(returns$Portfolio,x)*sqrt(N)})*eps)/a


#Part 2: parametric
mu=mean(returns$Portfolio)*N
sigma=sqrt(t(weights)%*%C%*%weights)*sqrt(N) #annual vol
VaR2=-mu+sigma*qnorm(1-a,mean=0,sd=1) #Assume normal distribution and iid
CVaR2=sum(sapply(seq(eps,a,by=eps),
                 function(x){-mu+sigma*qnorm(1-x,mean=0,sd=1)})*eps)/a


#Part 3:dynamic optimization
optimalWeights=data.frame(matrix(nrow=12,
                                 ncol=dim(returns_raw)[2]),row.names = seq(1,12))
colnames(optimalWeights)=c(colnames(returns_raw)[-1],"optimal value")
mu_monthly=matrix(mu_daily*21,ncol=1)
cov_monthly=C*21
#calculate the optimal weight dynamically
coeff1=1#1+mu*w
coeff2=1#1+2mu*w+w*cov*w
z=qnorm(0.95)
ones=matrix(1.0,nrow=dim(optimalWeights)[2]-1,ncol=1)
#x0=ones/dim(optimalWeights)[2]#initial guess
x0=matrix(c(1,0,0,0,0,0,0),ncol=1)
for(n in seq(12,1,-1)){
  print(n)
  obj<-function(x){
    temp=t(mu_monthly)%*%x
    -(coeff1*(1+temp)-z*sqrt((1+2*temp+t(x)%*%cov_monthly%*%x)*coeff2-(coeff1*(1+temp))^2))
  }
  res=constrOptim(theta=x0,f=obj,grad=NULL,ui=rbind(t(ones),-t(ones)),ci=rbind(0.9999,-1.0001))
  w=res$par
  print(res$value)
  optimalWeights[n,-dim(optimalWeights)[2]]=w
  optimalWeights[n,dim(optimalWeights)[2]]=-res$value
  coeff1=coeff1*(1+t(mu_monthly)%*%w)
  coeff2=coeff2*(1+2*t(mu_monthly)%*%w+t(w)%*%cov_monthly%*%w)
}
write.csv(optimalWeights,"output\\optimalWeights.csv")





