library(forecast)
library(smooth)
#Kenai sockeye R2_3 forecast 
#input data
d=read.table("KE_BRTB_ADJ.csv", sep = ",", header=T)
#names(d)[1] <- "YEAR" #rename first colume's name
myvars <- c("YEAR","ESCP", "FABND1","R2_2", "R2_3")
d=d[myvars] #select the variables
is.na(d) <- d=="." #change missing value . to NA
d<-as.data.frame(lapply(d, as.numeric)) #change all varialbes as numeric
#plot(R2_3 ~ R2_2, data=d,col="lightblue", pch=1, cex=2)
#text(R2_3 ~ R2_2, data=d, labels=YEAR,cex=0.6, font=5)
d.omit4 <- subset(d, YEAR!="1983"&YEAR!="1987"&YEAR!="2000"&YEAR!="2005")
#d.omit4 <- d  #if I want to know what if without omit4, run it.
rownames(d.omit4) <- NULL #reset rownames integer

#model fit
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
fit23sibling <- lm(R2_3 ~ R2_2, data=d)
fit23sibling.log <- lm(log(R2_3) ~ log(R2_2), data=d)
fit23sibling.omit4 <- lm(R2_3 ~ R2_2, data=d.omit4)
fit23sibling.omit4.log <- lm(log(R2_3) ~ log(R2_2), data=d.omit4)
#Regression on fry(age1) with ARIMA error
  #fish <- na.omit(d.omit4)
  fish <- d.omit4
  fish <- subset(fish, fish$R2_2 != "NA")
  fish <- subset(fish, fish$FABND1!=0)
  rownames(fish) <- NULL #reset rownames integer
  fit23ar<-auto.arima(log(fish$R2_3), xreg=log(fish$FABND1))#In SAS for R13, it's log(FABND0)
  #model selection criteria
AIC(fit23sibling)
AIC(fit23sibling.log)
AIC(fit23sibling.omit4)
AIC(fit23sibling.omit4.log)
AIC(fit23ar)
summary(fit23sibling)$adj.r.squared
summary(fit23sibling.log)$adj.r.squared
summary(fit23sibling.omit4)$adj.r.squared
summary(fit23sibling.omit4.log)$adj.r.squared

#Forecast using regression on fry(age1) abundance with ARIMA error
#best model for 2020 forecast
fish <- d.omit4
fish <- subset(fish, fish$FABND1 != 0)
fish <- subset(fish, fish$R2_2 != "NA")
year=fish$YEAR
n=11
year<-tail(year,n)
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit23ar<-auto.arima(log(fish.sub$R2_3), xreg=log(fish.sub$FABND1))
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-forecast(fit23ar, newxreg=log(new$FABND1))
  pred<-exp(lnpred$pred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_3")
obs=obs[myvars]
fry=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#fry <- na.omit(fry) 
#write.csv(fry, "R23_2020.csv")

#Kenai sockeye R2_3 forecast using R2_2 (silbing model log transformation)  
#log(R2_3) ~ log(R2_2)
myvars <- c("YEAR","R2_2", "R2_3")
fish=d.omit4[myvars] #select the variables
fish <- subset(fish, fish$R2_2 != "NA")
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
year=fish$YEAR
n=11
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit13.log <- lm(log(R2_3) ~ log(R2_2), data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit13.log, new)
  pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_3")
obs=obs[myvars]
sibling.log=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#sibling <- na.omit(sibling) 
#accuracy(sibling.log$R2_3, sibling.log$pred)

#Kenai sockeye R2_3 forecast using R2_2 (no log transformation) 
#R2_3 ~ R2_2
myvars <- c("YEAR","R2_2", "R2_3")
fish=d.omit4[myvars] #select the variables
#fish <- subset(fish, fish$R2_2 != "NA")
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
year=fish$YEAR
n=13
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit13 <- lm(R2_3 ~ R2_2, data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  pred<-predict(fit13, new)
  #pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R2_3")
obs=obs[myvars]
sibling=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#sibling <- na.omit(sibling) 
#accuracy(sibling$R2_3, sibling$pred)

#forecast using exponential smoothing
myvars <- c("YEAR", "R2_3")
fish=d.omit4[myvars] #select the variables
#fish <- na.omit(fish) #remove rows with missing values(uncompleted returns)
year=fish$YEAR
n=13
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "forecast.point")
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  run=fish.sub$R2_3
  #exponential smoothing from library(smooth)
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
fish=d.omit4[myvars] #select the variables
#fish <- na.omit(fish) #remove rows with missing values(uncompleted returns)
year=fish$YEAR
n=13
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

accuracy(sibling$pred,sibling$R2_3) #calculate forecast errors
accuracy( sibling.log$pred, sibling.log$R2_3) #calculate forecast errors
accuracy(fry$pred, fry$R2_3)
accuracy(exsmooth$forecast.point, exsmooth$R2_3)
accuracy(ma$forecast.point, ma$R2_3)

x<-ma
write.table(x, "clipboard", sep="\t", col.names = F) #copy to excel: use "ctrl+v" in excel



