library(forecast)
library(smooth)
#Kenai sockeye R2_2 forecast 
#Input data
d=read.table("KE_BRTB_ADJ.csv", sep = ",", header=T)
#names(d)[1] <- "YEAR" #rename first column's name
myvars <- c("YEAR","ESCP","R2_1","R2_2")
d=d[myvars] #select the variables
is.na(d) <- d=="." #change missing value . to NA
d<-as.data.frame(lapply(d, as.numeric)) #change all varialbes as numeric
#fish <- na.omit(d) #remove rows with missing values(uncompleted returns)
#plot(log(R2_2) ~ log(ESCP), data=fish,col="lightblue", pch=19, cex=2)
#text(log(R2_2) ~ log(ESCP), data=fish, labels=YEAR,cex=0.6, font=5)

#fit the same dataset to compare AIC etc
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
fish <- na.omit(d) #remove rows with missing values(uncompleted returns)
fish <- subset(d, d$R2_1 != 0) #remove R2_1=0
fit22esc <- lm(log(R2_2) ~ log(ESCP), data=fish)
fit22sibling <- lm(log(R2_2) ~ log(R2_1), data=fish) #sibling model
fit22ar<-auto.arima(log(fish$R2_2), xreg=log(fish$R2_1))
AIC(fit22esc)
AIC(fit22sibling)
AIC(fit22ar)
summary(fit22esc)$adj.r.squared
summary(fit22sibling)$adj.r.squared

#Kenai sockeye R2_2 forecast using escapment: LNR2_2=LNESCP  
myvars <- c("YEAR","ESCP","R2_2")
fish<-d[myvars]
#fish <- na.omit(d)
year=fish$YEAR
n=12 #ten-year comparison with run data and two-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit22.esc <- lm(log(R2_2) ~ log(ESCP), data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit22.esc, new)
  pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_2")
obs=obs[myvars]
lnesc=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#lnesc <- na.omit(lnesc) 
#accuracy(lnesc$R2_2, lnesc$pred)

#Kenai sockeye R2_2 forecast using sibling model:log(R2_2)=log(R2_1)
#the best model for 2020 is sibling model log(R2_2)~log(R2_1)
myvars <- c("YEAR","R2_1","R2_2")
fish<-d[myvars]
fish <- subset(fish, fish$R2_1 != 0) #remove R2_1=0
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
year=fish$YEAR
n=11 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit22.sibling <- lm(log(R2_2) ~ log(R2_1), data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit22.sibling, new)
  pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_2")
obs=obs[myvars]
sibling=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#lnesc <- na.omit(sibling) 
#accuracy(sibling$R2_2, sibling$pred)
#write.csv(sibling, "R22_2020.csv")

#Forecast using regression on R2_1 abundance with ARIMA error
#fish <- na.omit(d)
myvars <- c("YEAR","R2_1","R2_2")
fish<-d[myvars]
fish <- subset(fish, fish$R2_1 != 0)
year=fish$YEAR
n=11
year<-tail(year,n)
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit22ar<-auto.arima(log(fish.sub$R2_2), xreg=log(fish.sub$R2_1))
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-forecast(fit22ar, newxreg=log(new$R2_1))
  pred<-exp(lnpred$pred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_2")
obs=obs[myvars]
sibling.ar=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#sibling.ar <- na.omit(sibling.ar) 

#forecast using exponential smoothing
myvars <- c("YEAR", "R2_2")
fish=d[myvars] #select the variables
year=fish$YEAR
n=12
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

#simple moving avearage from library(smooth)
myvars <- c("YEAR", "R2_2")
fish=d[myvars] #select the variables
year=fish$YEAR
n=12 #how many years to forecast
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
accuracy(lnesc$pred, lnesc$R2_2)
accuracy(sibling$pred, sibling$R2_2)
accuracy(sibling.ar$pred, sibling.ar$R2_2)
accuracy(exsmooth$forecast.point, exsmooth$R2_2)
accuracy(ma$forecast.point, ma$R2_2)

x<-ma
write.table(x, "clipboard", sep="\t", col.names = F) #copy to excel: use "ctrl+v" in excel

