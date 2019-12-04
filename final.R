setwd("/Users/cooper/Documents/Study/STT844/FINAL/")
library(itsmr)
data=read.csv("UKGDP.csv")
test=data[1:178,2]
data=data[1:171,]
colnames(data)=c("Year","GDP")
plot(data,type="l")
data=data[,2]
acf(data,40,ylim=c(-1,1))
pacf(data,40,ylim=c(-1,1))

data_log=log(data)
plot(data_log,type = "l")

data_logdiff1=ts(diff(data_log,1))
plot(data_logdiff1)

acf(data_logdiff1,40,ylim=c(-1,1))
pacf(data_logdiff1,40,ylim=c(-1,1))
autofit(data_logdiff1,p=0:5,q=0:5)

M=c("log","trend",1)
e=Resid(data,M)
fit.ml=arma(e,p=4,q=1)
fit.ml
ee.ml=Resid(data,M,fit.ml)
test(ee.ml)

fit.hannan=hannan(e,p=4,q=1)
fit.hannan
ee.hannan=Resid(data,M,fit.hannan)
test(ee.hannan)

forecast(data,M,fit.ml,h=7,opt=1)
ml=forecast(data,M,fit.ml,h=7,opt=0)
mlforecast=c(data,ml$pred)
plot(mlforecast,type = "l",col="red",ylim = c(0,25000))
points(172:178,ml$l,col="red",type="l")
points(172:178,ml$u,col="red",type="l")
lines(test,col="blue")

forecast(data,M,fit.hannan,h=7,opt=1)
hannan=forecast(data,M,fit.hannan,h=7,opt=0)
hannanforecast=c(data,hannan$pred)
plot(hannanforecast,type = "l",col="red",ylim = c(0,25000))
points(172:178,hannan$l,col="red",type="l")
points(172:178,hannan$u,col="red",type="l")
lines(test,col="blue")

forecast(data,M,autofit(data),h=7)
