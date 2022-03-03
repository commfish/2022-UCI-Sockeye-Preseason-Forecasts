library(forecast)
library(smooth)
#Kenai sockeye R1_2 forecast 
#input data
d=read.table("KE_BRTB_ADJ.csv", sep = ",", header=T)
myvars <- c("YEAR","ESCP","R1_2")
d=d[myvars] #select the variables
is.na(d) <- d=="." #change missing value . to NA
#fish <- na.omit(fish) #remove rows with missing values(uncompleted returns)
d<-as.data.frame(lapply(d, as.numeric)) #change all varialbes as numeric
d$R12S<-d$R1_2/d$ESCP
#Brood interaction Model: MODEL LNRS=INTAC;
mylag <- function(x,k) c(rep(NA,k),head(x,-k)) #lag function
nrs <- d$ESCP # equivalent to datalines
one <- data.frame(
  x = nrs,
  lag1 = mylag(nrs,1),
  lag2 = mylag(nrs,2),
  year = 999  # R automatically loops, so no extra command needed
)
d$lag1 <- one$lag1
d$INTAC=d$ESCP*d$lag1
#plot(log(R1_2) ~ log(ESCP), data=d,col="lightblue", pch=19, cex=2)
#text(log(R1_2) ~ log(ESCP), data=d, labels=YEAR,cex=0.6, font=5)

#fit the same data set
fish=na.omit(d)
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
fit12esc <- lm(log(R1_2) ~ log(ESCP), data=fish)
fit12rs <- lm(log(R12S) ~ ESCP, data=fish) #Ricker form
fit12intac <- lm(log(R12S) ~ INTAC, data=fish) #Brood interaction Model
AIC(fit12esc)
AIC(fit12rs)
AIC(fit12intac)
summary(fit12esc)$adj.r.squared
summary(fit12rs)$adj.r.squared
summary(fit12intac)$adj.r.squared

#Kenai sockeye R1_2 forecast using escapment: LNR1_2=LNESCP   
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

#Kenai sockeye R1_2 forecast using Ricker model: LNRS=ESCP  
fish=d
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
year=fish$YEAR
n=11 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit12.rick <- lm(log(R12S) ~ ESCP, data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit12.rick, new)
  #pred<-exp(lnpred)*fish$ESCP[i] #calculate predicted R1_2 by R/S * S
  pred<-exp(lnpred)*new$ESCP #calculate predicted R1_2 by R/S * S
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R1_2")
obs=obs[myvars]
rick=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#lnesc <- na.omit(rick) 
#accuracy(rick$R1_2, rick$pred)

#Kenai sockeye R1_2 forecast using Brood interaction Model: LNRS=ESCP *ESCP(-1) (LNRS=INTAC ) 
fish=d
options(contrasts=c("contr.SAS","contr.poly")) #make parameter vlaues equal to one from SAS. Compare to Mark's SAS
year=fish$YEAR
n=11 #ten-year comparison with run data and one-year ahead forecast
year<-tail(year,n) #get the last 10 years for comparison
m=matrix(NA, nrow=1, ncol=2)
colnames(m)=c("f.yr", "pred") 
m=as.data.frame(m)
for(i in 1:n ) {
  fish.sub=fish[which(fish$YEAR < year[i]),]
  fit12.intac <- lm(log(R12S) ~ INTAC, data=fish.sub)
  f.yr=year[i]  #forecast one-year ahead
  new<-fish[which(fish$YEAR==f.yr),]
  lnpred<-predict(fit12.intac, new)
  #pred<-exp(lnpred)*$ESCP[i] #calculate predicted R1_2 by R/S * S
  pred<-exp(lnpred)*new$ESCP #calculate predicted R1_2 by R/S * S
  d.f=cbind(f.yr, pred)
  m=rbind(m,d.f)
}
m <- na.omit(m) 
obs=fish[which(fish$YEAR >= year[1]),]
myvars <- c("YEAR", "R1_2")
obs=obs[myvars]
intac=merge(obs, m, by.x="YEAR", by.y="f.yr", all=T)
#lnesc <- na.omit(rick) 
#accuracy(intac$R1_2, intac$pred)

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

accuracy(lnesc$pred, lnesc$R1_2)
accuracy( rick$pred, rick$R1_2)
accuracy( intac$pred, intac$R1_2)
accuracy( exsmooth$forecast.point, exsmooth$R1_2)
accuracy( ma$forecast.point, ma$R1_2)

x<-ma
write.table(x, "clipboard", sep="\t", col.names = F) #copy to excel: use "ctrl+v" in excel

