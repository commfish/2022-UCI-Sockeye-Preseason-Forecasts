library(forecast)
library(smooth)
#Kenai sockeye R1_3 forecast 
#input data
d=read.table("KE_BRTB_ADJ.csv", sep = ",", header=T) #they are same as SAS data
#names(d)[1] <- "YEAR" #rename first colume's name
myvars <- c("YEAR","FABND0","FSWT0","R1_2", "R1_3")
d=d[myvars] #select the variables
is.na(d) <- d=="." #change missing value . to NA
#d <- na.omit(d) 
#remove rows with missing values(uncompleted returns)
#for model comparisons below, must use the same dataset, so removing all missing values
#if check with Mark's SAS code's fry and sibling models, comment it out
d<-as.data.frame(lapply(d, as.numeric)) #change all varialbes as numeric
d$CFSWT[d$FSWT0<0.9]<-1 #create variable CFSWT (=1 if FSWT0<0.9)
d$CFSWT[d$FSWT0>=0.9] <- 2 #CFSWT=2 if FSWT0<0.9
d$CFSWT <- factor(d$CFSWT)#take CFSWT as a factor or "class" in SAS
#str(fish$CFSWT)
#levels(fish$CFSWT)

#fit the same dataset to compare models
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
fish=na.omit(d) 
fit13fry <- lm(log(R1_3) ~ log(FABND0), data=fish)
fit13frywt <- lm(log(R1_3) ~ log(FABND0)+CFSWT, data=fish)
fit13sibling <- lm(log(R1_3) ~ log(R1_2), data=fish)
#Regression with ARIMA error
fit13ar<-auto.arima(log(fish$R1_3), xreg=log(fish$FABND0))
#It turns out Regression with ARIMA(0,0,0) errors which is same as model "fit13fry"
AIC(fit13fry)
AIC(fit13frywt)
AIC(fit13sibling)
AIC(fit13ar) #it's same as fit13fry because of input variable fry. no ARIMA term 
AIC(es(fish$R1_3)) #exponential smoothing
AIC(sma(fish$R1_3)) #simple moving average
#anova(fit13fry, fit13sibling)
summary(fit13fry)$adj.r.squared
summary(fit13frywt)$adj.r.squared
summary(fit13sibling)$adj.r.squared
#par(mfrow = c(2,2)) # optional 4 graphs/page
#plot(fit13fry)
#par(mfrow = c(1, 1))

#Kenai sockeye R1_3 forecast using fry(age0) abundance and weight
myvars <- c("YEAR","FABND0","CFSWT", "R1_3")
fish=d[myvars] #select the variables
year=fish$YEAR
n=12 #ten-year comparison with run data and two-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
      fish.sub=fish[which(fish$YEAR < year[i]),]
      #fish.sub=fish[which(fish$YEAR < year[i]),]
      fit13.log <- lm(log(R1_3) ~ log(FABND0)+CFSWT, data=fish.sub)
      #f.yr=i+1  #forecast one-year ahead
      f.yr=year[i]  #forecast one-year ahead
      new<-fish[which(fish$YEAR==f.yr),]
      lnpred<-predict(fit13.log, new)
      pred<-exp(lnpred)
      d.f=cbind(f.yr, pred)
       m=rbind(m, d.f)
}
m <- na.omit(m) 
#obs=fish[which(fish$YEAR > 2002),]
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R1_3")
obs=obs[myvars]
fry=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#fry <- na.omit(fry)

#Kenai sockeye R1_3 forecast using R1_2 (silbing model)   
#the best model for 2020 is sibling model:log(R1_3)~log(R1_2)
myvars <- c("YEAR","R1_2", "R1_3")
fish=d[myvars] #select the variables
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
#fish=fish[which(fish$R1_2 != "NA"),]
year=fish$YEAR
n=12 #ten-year comparison with run data and two-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit13.log <- lm(log(R1_3) ~ log(R1_2), data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit13.log, new)
  pred<-exp(lnpred)
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R1_3")
obs=obs[myvars]
sibling=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#sibling <- na.omit(sibling)
#write.csv(sibling,'R13_2020.csv') #write a file for 2020 forecast

#forecast using exponential smoothing
myvars <- c("YEAR", "R1_3")
fish=d[myvars] #select the variables
year=fish$YEAR
n=12 #ten-year comparison with run data and two-year ahead forecast
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
  f.yr=year[i] #forecast one-year ahead
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
n=12 #ten-year comparison with run data and two-year ahead forecast
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
#ma=na.omit(ma)

#comb=merge(fry, sibling, by = "YEAR", all=T)
#write.csv(comb,'comb.csv')

accuracy(sibling$pred, sibling$R1_3) #calculate forecast errors
accuracy(fry$pred, fry$R1_3)
accuracy(exsmooth$forecast.point, exsmooth$R1_3)
accuracy(ma$forecast.point, ma$R1_3)

x<-ma
write.table(x, "clipboard", sep="\t", col.names = F) #copy to excel: use "ctrl+v" in excel


