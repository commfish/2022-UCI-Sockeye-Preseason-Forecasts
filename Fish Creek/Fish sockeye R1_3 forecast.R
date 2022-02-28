library(forecast)
library(smooth)
#Fish Creek sockeye forecast 
#input data
d=read.table("FISH_BRTB2021.csv", sep = ",", header=T)
names(d)[1] <- "YEAR"
is.na(d) <- d=="." #change missing value . to NA
d<-as.data.frame(lapply(d, as.numeric)) #change all varialbes as numeric

#using log(R1_3)~log(R1_2) (silbing model) 
plot(log(R1_3) ~ log(R1_2), data=d,col="lightblue", pch=19, cex=2)
text(log(R1_3) ~ log(R1_2), data=d, labels=YEAR,cex=0.6, font=5)
fit.sibling <- lm(log(R1_3) ~ log(R1_2), data=d) 
fit.sibling2 <- lm((R1_3) ~ (R1_2), data=d) 
AIC(fit.sibling)
AIC(fit.sibling2)

myvars <- c("YEAR","R1_2", "R1_3")
fish=d[myvars] #select the variables
options(contrasts=c("contr.SAS","contr.poly"))
#fish=fish[which(fish$R1_3 != "NA"),]
year=fish$YEAR
n=13 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit <- lm(log(R1_3) ~ log(R1_2), data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit, new)
  pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R1_3")
obs=obs[myvars]
sibling=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#write.csv(sibling,'R22_2020.csv') #write a file for 2020 forecast

#using (R1_3)~(R1_2) (silbing model without log transformation)   
myvars <- c("YEAR","R1_2", "R1_3")
fish=d[myvars] #select the variables
options(contrasts=c("contr.SAS","contr.poly"))
#fish=fish[which(fish$R1_3 != "NA"),]
year=fish$YEAR
n=13 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit <- lm((R1_3) ~ (R1_2), data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit, new)
  pred<-(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R1_3")
obs=obs[myvars]
sibling2=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#write.csv(sibling,'R22_2020.csv') #write a file for 2020 forecast

#forecast using exponential smoothing
myvars <- c("YEAR", "R1_3")
fish=d[myvars] #select the variables
year=fish$YEAR
n=13 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "forecast.point")
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  run=fish.sub$R1_3
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
myvars <- c("YEAR", "R1_3")
obs=obs[myvars]
exsmooth=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#exsmooth=na.omit(exsmooth)

#simple moving avearage from library("smooth")
myvars <- c("YEAR", "R1_3")
fish=d[myvars] #select the variables
year=fish$YEAR
n=13 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "forecast.point")
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  run=fish.sub$R1_3
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
myvars <- c("YEAR", "R1_3")
obs=obs[myvars]
ma=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)

accuracy(sibling$pred, sibling$R1_3)
accuracy(sibling2$pred, sibling2$R1_3)
accuracy(exsmooth$forecast.point, exsmooth$R1_3)
accuracy(ma$forecast.point, ma$R1_3)

x<-ma
write.table(x, "clipboard", sep="\t", col.names = F) #copy to excel: use "ctrl+v" in excel

