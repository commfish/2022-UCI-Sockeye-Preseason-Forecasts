library(forecast)
library(smooth)
#Kasilof sockeye R2_2 forecast 
#input data
d=read.table("KA_BRTB_ADJ.csv", sep = ",", header=T)
is.na(d) <- d=="." #change missing value . to NA
d<-as.data.frame(lapply(d, as.numeric)) #change all varialbes as numeric
myvars <- c("YEAR","ESCP","R2_1", "R2_2")
d=d[myvars] #select the variables
#plot(log(R1_3) ~ log(R1_2), data=d,col="lightblue", pch=19, cex=2)
#text(log(R1_3) ~ log(R1_2), data=d, labels=YEAR,cex=0.6, font=5)

#fit the same data set
fish=d
#make parameter vlaues equal to one from SAS. Compare to Mark's SAS
options(contrasts=c("contr.SAS","contr.poly")) 
fit.sibling <- lm(log(R2_2) ~ log(R2_1+1 ), data=fish) 
#The following result may differ to its SAS program due to auto.arima select different model
fitar1<-auto.arima(log(fish$R2_2), xreg=log(fish$R2_1+1))
fitar2<-auto.arima(fish$R2_2, xreg=fish$R2_1)
fitar3<-auto.arima(log(fish$R2_2), xreg=log(fish$ESCP))
#pacf(log(na.omit(fish$R2_2))) #check ar process
#fitar4<-auto.arima(log(fish$R2_2)) #auto select a model
#arima(log(fish$R2_2), order=c(0,1,0)) #differencing model
#arima(log(fish$R2_2), order=c(1,0,0)) #auotregressive model
#the results showed diffierecing model fit better than AR(1)

AIC(fit.sibling)
summary(fit.sibling)$adj.r.squared

#using log(R2_2)~log(R2_1) (silbing model)   
myvars <- c("YEAR","R2_1", "R2_2")
fish=d[myvars] #select the variables
options(contrasts=c("contr.SAS","contr.poly"))
#fish=fish[which(fish$R1_2 != "NA"),]
year=fish$YEAR
n=12 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit.log <- lm(log(R2_2) ~ log(R2_1+1), data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit.log, new)
  pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_2")
obs=obs[myvars]
sibling=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#write.csv(sibling,'R22_2020.csv') #write a file for 2020 forecast

#using log(R2_2)~log(ESCP)   
myvars <- c("YEAR","ESCP", "R2_2")
fish=d[myvars] #select the variables
options(contrasts=c("contr.SAS","contr.poly"))
#fish=fish[which(fish$R1_2 != "NA"),]
year=fish$YEAR
n=12 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit.log <- lm(log(R2_2) ~ log(ESCP), data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit.log, new)
  pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_2")
obs=obs[myvars]
esc=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#write.csv(sibling,'R22_2020.csv') #write a file for 2020 forecast


#Forecast using regression on log(R2_1) with ARIMA error
myvars <- c("YEAR","R2_1","R2_2")
fish<-d[myvars]
year=fish$YEAR
n=12
year<-tail(year,n)
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit.ar<-auto.arima(log(fish.sub$R2_2), xreg=log(fish.sub$R2_1+1))
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-forecast(fit.ar, newxreg=log(new$R2_1)+1)
  pred<-exp(lnpred$pred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_2")
obs=obs[myvars]
sibling.ar1=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#sibling.ar <- na.omit(sibling.ar) 

#Forecast using regression on R2_1 with ARIMA error
myvars <- c("YEAR","R2_1","R2_2")
fish<-d[myvars]
year=fish$YEAR
n=12
year<-tail(year,n)
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit.ar<-auto.arima(fish.sub$R2_2, xreg=fish.sub$R2_1+1)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-forecast(fit.ar, newxreg=new$R2_1+1)
  pred<-lnpred$pred
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_2")
obs=obs[myvars]
sibling.ar2=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#sibling.ar <- na.omit(sibling.ar) 

#Forecast using regression on log(ESCP) with ARIMA error
myvars <- c("YEAR","ESCP","R2_2")
fish<-d[myvars]
year=fish$YEAR
n=12
year<-tail(year,n)
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit.ar<-auto.arima(log(fish.sub$R2_2), xreg=log(fish.sub$ESCP))
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-forecast(fit.ar, newxreg=log(new$ESCP))
  pred<-exp(lnpred$pred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_2")
obs=obs[myvars]
esc.ar=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#sibling.ar <- na.omit(sibling.ar) 

#forecast using exponential smoothing
myvars <- c("YEAR", "R2_2")
fish=d[myvars] #select the variables
year=fish$YEAR
n=12 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "forecast.point")
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  run=fish.sub$R2_2
  #exponential smoothing from library("smooth")
  mod.es=es(run, holdout = F)
  f.es=forecast(mod.es, h=1, level =0.9) #one-year ahead forecast
  forecast.point=as.vector(f.es$forecast)
  lower90=as.vector(f.es$lower)
  upper90=as.vector(f.es$upper)
  f.yr=year[i]  #forecast one-year ahead
  d.es=cbind(f.yr, forecast.point)
  d.es=as.data.frame(d.es)
  m=rbind(m,d.es)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_2")
obs=obs[myvars]
exsmooth=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#exsmooth=na.omit(exsmooth)

#simple moving avearage from library("smooth")
myvars <- c("YEAR", "R2_2")
fish=d[myvars] #select the variables
year=fish$YEAR
n=12 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "forecast.point")
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  run=fish.sub$R2_2
  mod.sma= sma(run)
  f.sma=forecast(mod.sma, h=1, level =0.9) #one-year ahead forecast
  forecast.point=as.vector(f.sma$forecast)
  lower90=as.vector(f.sma$lower)
  upper90=as.vector(f.sma$upper)
  f.yr=year[i]  #forecast one-year ahead
  d.sma=cbind(f.yr, forecast.point)
  d.sma=as.data.frame(d.sma)
  m=rbind(m,d.sma)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_2")
obs=obs[myvars]
ma=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#ma=na.omit(ma)

#comb=merge(fry, sibling, by = "YEAR", all=T)
#write.csv(comb,'comb.csv')

accuracy(sibling$pred, sibling$R2_2)
accuracy(sibling.ar1$pred, sibling.ar1$R2_2)
accuracy(sibling.ar2$pred, sibling.ar2$R2_2)
accuracy(esc$pred, esc$R2_2)
accuracy(esc.ar$pred, esc.ar$R2_2)
accuracy(exsmooth$forecast.point, exsmooth$R2_2)
accuracy(ma$forecast.point, ma$R2_2)

x<-ma
write.table(x, "clipboard", sep="\t", col.names = F) #copy to excel: use "ctrl+v" in excel

