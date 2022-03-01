library(forecast)
library(smooth)
#Kasilof sockeye R2_3 forecast 
#input data
d=read.table("KA_BRTB_ADJ.csv", sep = ",", header=T)
is.na(d) <- d=="." #change missing value . to NA
#fish <- na.omit(fish) #remove rows with missing values(uncompleted returns)
d<-as.data.frame(lapply(d, as.numeric)) #change all varialbes as numeric
myvars <- c("YEAR","SM2","R2_2", "R2_3")
d=d[myvars] #select the variables
#plot(log(R2_3) ~ log(R2_2), data=d,col="lightblue", pch=19, cex=2)
#text(log(R2_3) ~ log(R2_2), data=d, labels=YEAR,cex=0.6, font=5)

#fit the same data set
fish=d
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
fit.sibling <- lm(log(R2_3) ~ log(R2_2 ), data=fish) 
fit.ar<-auto.arima(log(fish$R2_3), xreg=log(fish$R2_2))
fit.sibling2 <- lm(R2_3 ~ R2_2 , data=fish)
fit.ar2<-auto.arima(fish$R2_3, xreg=fish$R2_2)
fit.sm <- lm(R2_3 ~ SM2, data=fish) #smolt model

AIC(fit.sibling)
AIC(fit.ar)
AIC(fit.sibling2)
AIC(fit.ar2)
AIC(fit.sm)
summary(fit.sibling2)$adj.r.squared

#R2_3 using log(R2_3)~log(R2_2) (silbing model)   
#the best model for 2020 is sibling model:log(R1_3)~log(R1_2)
myvars <- c("YEAR","R2_2", "R2_3")
fish=d[myvars] #select the variables
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
#fish=fish[which(fish$R1_2 != "NA"),]
year=fish$YEAR
n=13 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit.log <- lm(log(R2_3) ~ log(R2_2), data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit.log, new)
  pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_3")
obs=obs[myvars]
sibling=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#sibling <- na.omit(sibling)
#write.csv(sibling,'R23_2020.csv') #write a file for 2020 forecast

#Forecast using regression on log(R2_2) with ARIMA error
myvars <- c("YEAR","R2_2","R2_3")
fish<-d[myvars]
year=fish$YEAR
n=13
year<-tail(year,n)
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit.ar<-auto.arima(log(fish.sub$R2_3), xreg=log(fish.sub$R2_2))
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-forecast(fit.ar, newxreg=log(new$R2_2))
  pred<-exp(lnpred$pred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_3")
obs=obs[myvars]
sibling.ar=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#sibling.ar <- na.omit(sibling.ar) 

#R2_3 using R2_3~R2_2 (silbing model without log transformation)   
myvars <- c("YEAR","R2_2", "R2_3")
fish=d[myvars] #select the variables
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
#fish=fish[which(fish$R1_2 != "NA"),]
year=fish$YEAR
n=13 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit<- lm(R2_3 ~ R2_2, data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  pred<-predict(fit, new)
  #pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_3")
obs=obs[myvars]
sibling2=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#sibling <- na.omit(sibling)
#write.csv(sibling,'R23_2020.csv') #write a file for 2020 forecast

#Forecast using regression on R2_2 with ARIMA error
myvars <- c("YEAR","R2_2","R2_3")
fish<-d[myvars]
year=fish$YEAR
n=13
year<-tail(year,n)
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  #i=1
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit.ar<-auto.arima(fish.sub$R2_3, xreg=fish.sub$R2_2)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-forecast(fit.ar, newxreg=new$R2_2)
  pred<-lnpred$pred
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_3")
obs=obs[myvars]
sibling.ar2=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#sibling.ar <- na.omit(sibling.ar) 

#R2_3 forecast using smolt model  
#the best model for 2020 is sibling model:log(R1_2)~log(SM1)
myvars <- c("YEAR","SM2", "R2_3")
fish=d[myvars] #select the variables
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
#fish=fish[which(fish$R1_2 != "NA"),]
year=fish$YEAR
n=13 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit <- lm(log(R2_3) ~ log(SM2), data=fish.sub) 
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit, new)
  pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_3")
obs=obs[myvars]
smolt=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#write.csv(smolt,'R13_2020.csv') #write a file for 2020 forecast

#forecast using exponential smoothing
myvars <- c("YEAR", "R2_3")
fish=d[myvars] #select the variables
year=fish$YEAR
n=13 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "forecast.point")
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  run=fish.sub$R2_3
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
myvars <- c("YEAR", "R2_3")
obs=obs[myvars]
exsmooth=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#exsmooth=na.omit(exsmooth)

#simple moving avearage from library("smooth")
myvars <- c("YEAR", "R2_3")
fish=d[myvars] #select the variables
year=fish$YEAR
n=13 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "forecast.point")
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  run=fish.sub$R2_3
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
myvars <- c("YEAR", "R2_3")
obs=obs[myvars]
ma=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#ma=na.omit(ma)

#comb=merge(fry, sibling, by = "YEAR", all=T)
#write.csv(comb,'comb.csv')

accuracy(sibling$pred, sibling$R2_3)
accuracy( sibling.ar$pred, sibling.ar$R2_3)
accuracy(sibling2$pred, sibling2$R2_3)
accuracy(sibling.ar2$pred, sibling.ar2$R2_3)
accuracy(smolt$pred, smolt$R2_3)
accuracy(exsmooth$forecast.point, exsmooth$R2_3)
accuracy(ma$forecast.point, ma$R2_3)

x<-ma
write.table(x, "clipboard", sep="\t", col.names = F) #copy to excel: use "ctrl+v" in excel

