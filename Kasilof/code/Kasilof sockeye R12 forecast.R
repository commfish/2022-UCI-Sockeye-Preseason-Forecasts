library(forecast)
library(smooth)
#Kasilof sockeye R1_2 forecast 
#input data
d=read.table("KA_BRTB_ADJ.csv", sep = ",", header=T)
#names(d)[1] <- "YEAR" #rename first colume's name
is.na(d) <- d=="." #change missing value . to NA
#fish <- na.omit(fish) #remove rows with missing values(uncompleted returns)
d<-as.data.frame(lapply(d, as.numeric)) #change all varialbes as numeric
myvars <- c("YEAR","ESCP","SM1","R1_1", "R1_2")
d=d[myvars] #select the variables
#plot(log(R1_2) ~ log(ESCP), data=d,col="lightblue", pch=19, cex=2)
#text(log(R1_2) ~ log(ESCP), data=d, labels=YEAR,cex=0.6, font=5)

#fit the same data set
fish=d
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
fit12esc <- lm(log(R1_2) ~ log(ESCP), data=fish)
fit12ar<-auto.arima(log(fish$R1_2), xreg=log(fish$ESCP))
fit.sibling.log <- lm(log(R1_2) ~ log(R1_1 + 1), data=fish) 
fit.sm <- lm(log(R1_2) ~ log(SM1), data=fish) #smolt model

AIC(fit12esc)
AIC(fit.sibling.log)
summary(fit12esc)$adj.r.squared

#R1_2 using log(R1_1) (silbing model with log transformation)   
myvars <- c("YEAR","R1_1", "R1_2")
fish=d[myvars] #select the variables
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
#fish=fish[which(fish$R1_2 != "NA"),]
year=fish$YEAR
n=11 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit12.log <- lm(log(R1_2) ~ log(R1_1 + 1), data=fish.sub) # R1_1 + 1 to avoid log(0)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit12.log, new)
  pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R1_2")
obs=obs[myvars]
sibling=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#sibling <- na.omit(sibling)
#write.csv(sibling,'R12_2020.csv') #write a file for 2020 forecast


#R1_2 forecast using escapment: LNR1_2=LNESCP   
myvars <- c("YEAR","ESCP","R1_2")
fish<-d[myvars]
year=fish$YEAR
n=11#ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit12.esc <- lm(log(R1_2) ~ log(ESCP), data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit12.esc, new)
  pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R1_2")
obs=obs[myvars]
lnesc=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#lnesc <- na.omit(lnesc) 
#accuracy(lnesc$R1_2, lnesc$pred)
#write.csv(lnesc,'R12_2020.csv') #write a file for 2020 forecast

#Forecast using regression on escapment with ARIMA error
#fish <- na.omit(d)
myvars <- c("YEAR","ESCP","R1_2")
fish<-d[myvars]
#fish <- subset(fish, fish$R2_1 != 0)
year=fish$YEAR
n=11
year<-tail(year,n)
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit22ar<-auto.arima(log(fish.sub$R1_2), xreg=log(fish.sub$ESCP))
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-forecast(fit22ar, newxreg=log(new$ESCP))
  pred<-exp(lnpred$pred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R1_2")
obs=obs[myvars]
escp.ar=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#sibling.ar <- na.omit(sibling.ar) 

#R1_2 forecast using smolt model  
#the best model for 2020 is sibling model:log(R1_2)~log(SM1)
myvars <- c("YEAR","SM1", "R1_2")
fish=d[myvars] #select the variables
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
#fish=fish[which(fish$R1_2 != "NA"),]
year=fish$YEAR
n=11 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit12 <- lm(log(R1_2) ~ log(SM1), data=fish.sub) 
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit12, new)
  pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R1_2")
obs=obs[myvars]
smolt=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#write.csv(smolt,'R12_2020.csv') #write a file for 2020 forecast

#forecast using exponential smoothing
myvars <- c("YEAR", "R1_2")
fish=d[myvars] #select the variables
year=fish$YEAR
n=11 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "forecast.point")
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  run=fish.sub$R1_2
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
#obs=fish[which(fish$YEAR > 2002),]
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R1_2")
obs=obs[myvars]
exsmooth=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#exsmooth=na.omit(exsmooth)

#simple moving avearage from library("smooth")
myvars <- c("YEAR", "R1_2")
fish=d[myvars] #select the variables
year=fish$YEAR
n=11 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "forecast.point")
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  run=fish.sub$R1_2
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
myvars <- c("YEAR", "R1_2")
obs=obs[myvars]
ma=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#ma=na.omit(ma)

#comb=merge(fry, sibling, by = "YEAR", all=T)
#write.csv(comb,'comb.csv')

accuracy( sibling$pred, sibling$R1_2,)
accuracy( lnesc$pred, lnesc$R1_2)
accuracy( escp.ar$pred, escp.ar$R1_2)
accuracy(smolt$pred, smolt$R1_2)
accuracy(exsmooth$forecast.point, exsmooth$R1_2)
accuracy(ma$forecast.point, ma$R1_2)

x<-ma
write.table(x, "clipboard", sep="\t", col.names = F) #copy to excel: use "ctrl+v" in excel
