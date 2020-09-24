########################## reconstruction input statistics for bottom trawlers in China #############
#### The code in this project was used to reconstruct fishing capacity for China's bottom trawlers and other motorized catchers ##
#### A detailed description of the methodology are available from "Reconstructing fishing capacity and landings of China's bottom trawl fisheries (1950 - 2018)", by Xiong Zhang & Amanda C.J. Vincent, published as a working paper by Institute for the Oceans and Fisheries, UBC.
#### install packages
install.packages('forecast', dependencies = TRUE)
library(forecast)
install.packages("aTSA")
library(aTSA)
install.packages('fANCOVA')
library(fANCOVA)
setwd("C:/Users/xiong/Desktop/MS_history of BTF China") ### set up directory for yourself in your PC.

################### 1st period: bottom trawlers from 1950 - 1978 ###################
# data input
df0<-read.csv("CN_LMC2.csv",header=TRUE,sep=',') ## This is the origional data about China's large marine catchers, which contain bottom trawlers
str(df0)
df0$N_LMClw<-df0$N_LMC
df0$N_LMCup<-df0$N_LMC
df0$H_LMClw<-df0$H_LMC
df0$H_LMCup<-df0$H_LMC
df<-df0[1:34,]
str(df)
df2<- df[seq(dim(df)[1],1),]
str(df2)
library(forecast)
################################ for N_YL (# of Yulun vessels) and H_YL (Horsepower of Yulun vessels); see explanation in the methodology.
df3<-df0[seq(dim(df)[1],1),]
df3
### arima
ari0<-auto.arima(df3$N_YL,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0)
YL<-forecast(ari0,h=1)
YL
### loess
los<-loess.as(df3$year[1:33],df3$H_YL[1:33],criterion = c("aicc", "gcv")[1],control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
s2<-summary(los)$p$span
d2<-summary(los)$p$degree
los2<-loess(df3$H_YL~df3$year,degree=d2,span=s2,control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
fit2<-predict(los2,df3,se=T)$fit
fit2_se<-predict(los2,df3,se=T)$se.fit
ss.dist <- sum(scale(df3$H_YL[1:33], scale=FALSE)^2) # change N_YL to H_YL for create model for the latter
ss.resid <- sum(resid(los2)^2)
r.sq2<- 1-ss.resid/ss.dist
fit2
fit2_se

## for RN_LMC (Ration between large marine catchers and Yulun by number)
ari<-auto.arima(df2$RN_LMC,trace = T)
summary(ari)
forecast(ari,h=28)
df$RN_LMC_arlower<-df$RN_LMC
df$RN_LMC_arupper<-df$RN_LMC
df
for(i in 1:34){
  if(is.na(df$RN_LMC[i])){
    df$RN_LMC[i]<-0.9181803
    df$RN_LMC_arlower[i]<-0.9084178
    df$RN_LMC_arupper[i]<-0.9279428
  }
}
## for RHpV_LMC (Ration between large marine catchers and Yulun by mean horsepower)
ari<-auto.arima(df2$RHpV_LMC,trace = T)
summary(ari)
pred2<-predict(ari,df2)
forecast(ari,h=28)
df$RHpV_LMC_arlower<-df$RHpV_LMC
df$RHpV_LMC_arupper<-df$RHpV_LMC
df
for(i in 1:34){
  if(is.na(df$RHpV_LMC[i])){
    df$RHpV_LMC[i]<-0.9505
    df$RHpV_LMC_arlower[i]<-0.9359
    df$RHpV_LMC_arupper[i]<-0.9652
  }
}
str(df)
## calculate H_LMC (horsepower of large marine catchers) from RHpV_LMC
df$HpV_LMC_mean<-df$HpV_LMC
df$HpV_LMC_lower<-df$HpV_LMC
df$HpV_LMC_upper<-df$HpV_LMC
df$HpV_LMC_mean
df$HpV_LMC_lower
df$RHpV_LMC-df$RHpV_LMC_arlower
for(i in 1:34){
  if(is.na(df$HpV_LMC_mean[i])){
    se<-(df$RHpV_LMC[i]-df$RHpV_LMC_arlower[i])/2
    df$HpV_LMC_mean[i]<-df$HpV_YL[i]*df$RHpV_LMC[i]
    df$HpV_LMC_lower[i]<-df$HpV_LMC_mean[i]*(1-2*se)
    df$HpV_LMC_upper[i]<-df$HpV_LMC_mean[i]*(1+2*se)
  }
}
df
######### Fig S3: plot arima and prediction on total number of LMC
par(mfrow=c(1,2),mar=c(4,5,1,1))
# Fig S3a
df$N_LMC_lower<-df$N_YL*df$RN_LMC_arlower
df$N_LMC_upper<-df$N_YL*df$RN_LMC_arupper
df$N_LMC_mean<-df$N_YL*df$RN_LMC
df$N_LMC_lower[1]<-df$N_LMC_mean[1]*(1-2*sqrt((0.005/0.918)^2+(32/199)^2))
df$N_LMC_upper[1]<-df$N_LMC_mean[1]*(1+2*sqrt((0.005/0.918)^2+(32/199)^2))
df
# calculate for trawlers
# N_tr (# of trawlers) including lower and upper bounds of the 95% CI
df$N_tr<-df$N_LMC_mean
df$N_tr[15:20]<-df$N_LMC_mean[15:20]-17
df$N_tr[21]<-df$N_LMC_mean[21]-26
df$N_tr[22:34]<-df$N_LMC_mean[22:34]-46
df$N_tr
df$N_tr_lw<-df$N_LMC_lower
df$N_tr_lw[15:20]<-df$N_LMC_lower[15:20]-17
df$N_tr_lw[21]<-df$N_LMC_lower[21]-26
df$N_tr_lw[22:34]<-df$N_LMC_lower[22:34]-46
df$N_tr_lw
df$N_tr_up<-df$N_LMC_upper
df$N_tr_up[15:20]<-df$N_LMC_upper[15:20]-17
df$N_tr_up[21]<-df$N_LMC_upper[21]-26
df$N_tr_up[22:34]<-df$N_LMC_upper[22:34]-46
df$N_tr_up
# H_tr (Horsepower of trawlers) including lower and upper bounds of the 95% CI
df$H_tr<-df$N_tr*df$HpV_LMC_mean
df$H_tr
se_tr<-0
se_HpV<-0
for(i in 1:34){
  se_tr[i]<-(df$N_tr[i]-df$N_tr_lw[i])/2
  se_HpV[i]<-(df$HpV_LMC_mean[i]-df$HpV_LMC_lower[i])/2
  df$H_tr_se[i]<-df$H_tr*sqrt((se_tr[i]/df$N_tr[i])^2+(se_HpV[i]/df$HpV_LMC_mean[i])^2)
}
df$H_tr_se
df<-df[1:29,]
df
par(mfrow=c(1,2),mar=c(4,4,1,1))
max<-max(df$N_tr_up)/100;max
min<-min(df$N_tr_lw)/100;min
plot(df$N_tr/100~df$year,pch=1,type="l",xaxt="n",ylim=c(min,max),xlab="",ylab="",las=1)
lines(df$N_tr_up/100~df$year,lty="dashed",col="red")
lines(df$N_tr_lw/100~df$year,lty="dashed",col="red")
Axis(side=1,at=seq(1950,1983,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of Chinese bottom trawlers (x100)",side=2,col="black",line=2.5,las=0) 
mtext("a)",side=3,adj=0,cex=1)
legend("topleft",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
### convert hp to kW by 0.7457
max<-0.7457*max(df$H_tr,df$H_tr+2*df$H_tr_se)/100000
min<-0.7457*min(df$H_tr,df$H_tr-2*df$H_tr_se)/100000
plot(0.7457*df$H_tr/100000~df$year,pch=1,type="l",xaxt="n",xlab="",xlim=c(1950,1980),ylim=c(min,max),ylab="",las=1)
lines((0.7457*df$H_tr-2*df$H_tr_se)/100000~df$year,lty="dashed",col="red")
lines(0.7457*(df$H_tr+2*df$H_tr_se)/100000~df$year,lty="dashed",col="red")
Axis(side=1,at=seq(1950,1986,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total horsepower of Chinese bottom trawlers (x100,000 kW)",side=2,col="black",line=2.5,las=0)
mtext("b)",side=3,adj=0,cex=1)
legend("topleft",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend

par(fig=c(0.04,0.22,0.46,0.86),new=T)
plot(df$N_tr[1:3]/100~df$year[1:3],pch=1,type="l",ylim=c(1,3),xaxt="n",xlab="",ylab="",las=1,cex.axis=0.8)
lines(df$N_tr_lw[1:3]/100~df$year[1:3],lty="dashed",col="red")
lines(df$N_tr_up[1:3]/100~df$year[1:3],lty="dashed",col="red")
Axis(side=1,at=seq(1950,1953,by=1),cex.axis=0.8)

par(fig=c(0.3,0.48,0.07,0.4),new=T)
plot(df$N_tr[16:18]/100~df$year[16:18],pch=1,type="l",ylim=c(8,8.5),xaxt="n",xlab="",ylab="",las=1,cex.axis=0.8)
lines(df$N_tr_up[16:18]/100~df$year[16:18],lty="dashed",col="red")
lines(df$N_tr_lw[16:18]/100~df$year[16:18],lty="dashed",col="red")
Axis(side=1,at=seq(1965,1970,by=1),cex.axis=0.8)

par(fig=c(0.55,0.73,0.46,0.86),new=T)
plot(0.7457*df$H_tr[1:3]/100000~df$year[1:3],pch=1,type="l",ylim=c(0.7457*0.1,0.7457*0.2237),xaxt="n",xlab="",ylab="",las=1,cex.axis=0.8)
lines(0.7457*(df$H_tr-2*df$H_tr_se)[1:3]/100000~df$year[1:3],lty="dashed",col="red")
lines(0.7457*(df$H_tr+2*df$H_tr_se)[1:3]/100000~df$year[1:3],lty="dashed",col="red")
Axis(side=1,at=seq(1950,1953,by=1),cex.axis=0.8)

par(fig=c(0.8,0.98,0.07,0.4),new=T)
plot(0.7457*df$H_tr[16:18]/100000~df$year[16:18],pch=1,type="l",ylim=c(0.7457*1.23,0.7457*1.256),xaxt="n",xlab="",ylab="",las=1,cex.axis=0.8)
lines(0.7457*(df$H_tr-2*df$H_tr_se)[16:18]/100000~df$year[16:18],lty="dashed",col="red")
lines(0.7457*(df$H_tr+2*df$H_tr_se)[16:18]/100000~df$year[16:18],lty="dashed",col="red")
Axis(side=1,at=seq(1965,1970,by=1),cex.axis=0.8)

#### output estimates

write.csv(df,"CN_tr_1978.csv")
#########################################

####################### 2nd Period: bottom trawlers from 1980 - 2002 #######################################
df<-read.csv("CN_recon.csv",header=TRUE,sep=',') ## This dataset contains the collected available data for fishing capacity of various of trawlers and motorized catchers from local provinces (i.e. Zhejiang, Fujian, and Guangdong)
str(df)
head(df)
df$O_year<-df$year-1979
df$O_year
####### for shrimp trawlers in Zhejiang
# gam for full dataset
library(mgcv)
gam1<-gam(N_st_ZJ~s(O_year),data=df, select=TRUE, method='GCV.Cp');BIC(gam1)
summary(gam1)
plot(gam1,shade = T)
pred<-predict(gam1,df[1:35,],se=T)
pred
df$N_st_ZJ
# lm for subdataset
gam2<-gam(N_st_ZJ~s(O_year,k=2),data=df[1:7,]);BIC(gam2)
summary(gam2)
pred2<-predict(gam2,df[1:7,],se.fit=T)
pred2
# interpolation
df$N_st_ZJ
df$N_st_ZJ[2]<-round(pred2$fit[2])
df$N_st_ZJse<-0
df$N_st_ZJse[2]<-round(pred2$se.fit[2])
df$N_st_ZJ[c(8:14,17:21,26:28,30:32,34)]<-round(pred$fit[c(8:14,17:21,26:28,30:32,34)])
df$N_st_ZJse[c(8:14,17:21,26:28,30:32,34)]<-round(pred$se.fit[c(8:14,17:21,26:28,30:32,34)])
head(df)
df$N_st_ZJ<-round(df$N_st_ZJ)
df$N_st_ZJ
######## Fig S4
###### plot  estimation 
# plot gam for full dataset
max<-max(pred$fit+2*pred$se.fit)/1000
min<-min(pred$fit-2*pred$se.fit)/1000
plot(df$N_st_ZJ/1000~df$year,pch=1,type="p",xaxt="n",ylim=c(min,max),xlab="",ylab="",las=1)
lines(pred$fit/1000~df$year[1:35],lty="solid",col="black")
lines((pred$fit-2*pred$se.fit)/1000~df$year[1:35],lty="dashed",col="red")
lines((pred$fit+2*pred$se.fit)/1000~df$year[1:35],lty="dashed",col="red")
Axis(side=1,at=seq(1980,2017,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of shrimp trawlers in Zhejiang (x1000)",side=2,col="black",line=2.5,las=0) 
mtext("a)",side=3,adj=0,cex=1)
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
# plot lm for subdataset
max<-max(na.omit(df$N_st_ZJ[1:7]),pred2$fit+2*pred2$se.fit)/1000
min<-min(na.omit(df$N_st_ZJ[1:7]),pred2$fit-2*pred2$se.fit)/1000
plot(df$N_st_ZJ[1:7]/1000~df$year[1:7],pch=1,type="p",xaxt="n",ylim=c(min,max),xlab="",ylab="",las=1)
lines(pred2$fit/1000~df$year[1:7],lty="solid",col="black")
lines((pred2$fit-2*pred2$se.fit)/1000~df$year[1:7],lty="dashed",col="red")
lines((pred2$fit+2*pred2$se.fit)/1000~df$year[1:7],lty="dashed",col="red")
Axis(side=1,at=seq(1980,1986,by=1))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of shrimp trawlers in Zhejiang (x1000)",side=2,col="black",line=2.5,las=0) 
mtext("b)",side=3,adj=0,cex=1)
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend

###### for single trawlers in Zhejiang
df$N_single_tr_ZJ<-round(df$N_single_tr_ZJ)
df$N_single_tr_ZJ
df$RN_st_single_tr_ZJ<-df$N_single_tr_ZJ/df$N_st_ZJ
plot(df$RN_st_single_tr_ZJ~df$year)
# arima model
df2<-df[17:35,]
df2$N_single_tr_ZJ
library(forecast)
ari<-auto.arima(df2$N_single_tr_ZJ,trace = T)
summary(ari)
for6<-forecast(ari,h=6)
for6
df$N_sg_ZJ<-df$N_single_tr_ZJ
df$N_sg_ZJ[30:35]<-2109
df$N_sg_ZJlw<-df$N_single_tr_ZJ
df$N_sg_ZJlw[30:35]<-1953
df$N_sg_ZJup<-df$N_single_tr_ZJ
df$N_sg_ZJup[30:35]<-2265
df4<-df[1:29,]
df4<-as.data.frame(do.call(cbind, lapply(df4, rev)))
df4$N_single_tr_ZJ
ari2<-auto.arima(df4$N_single_tr_ZJ,trace = T)
summary(ari2)
bck10<-forecast(ari2,h=16)
bck10
df$N_sg_ZJ[1:16]<-2109
df$N_sg_ZJlw[1:16]<-1953
df$N_sg_ZJup[1:16]<-2265
# loess model
df3<-df
fit3<-loess.as(df3$O_year[17:29],df3$N_single_tr_ZJ[17:29],criterion = c("aicc", "gcv")[1],control = loess.control(surface = "direct"))
summary(fit3)
fit4<-loess(df3$N_single_tr_ZJ~df3$O_year,degree=1,span=0.848,control = loess.control(surface = "direct"))
summary(fit4)
pred<-predict(fit4,df3,se=T)
pred
df$N_sg_ZJ[c(1:16,30:35)]<-pred$fit[c(1:16,30:35)]
df$N_sg_ZJse[c(1:16,30:35)]<-pred$se.fit[c(1:16,30:35)]
df$N_sg_ZJlw[c(1:16,30:35)]<-pred$fit[c(1:16,30:35)]-2*pred$se.fit[c(1:16,30:35)]
df$N_sg_ZJup[c(1:16,30:35)]<-pred$fit[c(1:16,30:35)]+2*pred$se.fit[c(1:16,30:35)]

######### Fig S5: plot arima and loess prediction
max<-max(na.omit(df3$N_single_tr_ZJ),pred$fit+2*pred$se.fit)/1000
min<-min(na.omit(df3$N_single_tr_ZJ),pred$fit-2*pred$se.fit)/1000
plot(df3$N_single_tr_ZJ/1000~df3$year,pch=1,type="p",xaxt="n",ylim=c(min,max),xlab="",ylab="",las=1)
lines(df$N_sg_ZJ[1:35]/1000~df$year[1:35],lty="solid",col="black")
lines(df$N_sg_ZJup[1:35]/1000~df$year[1:35],lty="dashed",col="red")
lines(df$N_sg_ZJlw[1:35]/1000~df$year[1:35],lty="dashed",col="red")
Axis(side=1,at=seq(1980,2015,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of single trawlers in Zhejiang (x1000)",side=2,col="black",line=2.5,las=0) 
mtext("a)",side=3,adj=0,cex=1)
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend

max<-max(na.omit(df3$N_single_tr_ZJ),pred$fit+2*pred$se.fit)/1000
min<-min(na.omit(df3$N_single_tr_ZJ),pred$fit-2*pred$se.fit)/1000
plot(df3$N_single_tr_ZJ[1:35]/1000~df3$year[1:35],pch=1,type="p",xaxt="n",ylim=c(min,max),xlab="",ylab="",las=1)
lines(pred$fit[1:35]/1000~df3$year[1:35],lty="solid",col="black")
lines((pred$fit[1:35]-2*pred$se.fit[1:35])/1000~df3$year[1:35],lty="dashed",col="red")
lines((pred$fit[1:35]+2*pred$se.fit[1:35])/1000~df3$year[1:35],lty="dashed",col="red")
Axis(side=1,at=seq(1980,2015,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of single trawlers in Zhejiang (x1000)",side=2,col="black",line=2.5,las=0) 
mtext("b)",side=3,adj=0,cex=1)
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend

##### estimate trawlers from single and shrimp trawlers
str(df)
df$N_sg_ZJ<-round(df$N_sg_ZJ)
df$N_com_ZJ<-df$N_sg_ZJ+df$N_st_ZJ
df$N_com_ZJ
df$N_com_ZJse<-sqrt((2*df$N_sg_ZJse)^2+(2*df$N_st_ZJse)^2)/2
df$N_com_ZJse
df5<-df[1:35,]
plot(df5$N_com_ZJ~df5$year)
plot(df5$N_tr~df5$N_com_ZJ)
plot(df$N_tr/df$N_com_ZJ~df$year)
write.csv(df,"CN_recon2.csv")
########
df<-read.csv("CN_recon2.csv",header=TRUE,sep=',')
str(df)
attach(df)
v<-RN_tr_com_ZJ
ve<-RN_tr_com_Zjse
df_bt<-data.frame(O_year,v,ve)
str(df_bt)
par(mfrow=c(1,1))

########################
# gam model with bootstrap on ratio dataset
fit2<-data.frame(matrix(NA, nrow = 1000, ncol = 38))
fit2_se<-data.frame(matrix(NA, nrow = 1000, ncol = 38))
r.sq2<-0
library(mgcv)
for(j in 1:1000){
  Rtr<-0
  mv<-0
  sdv<-0
  df_bt[1,2]
  for(i in 1:38){
    mv<-df_bt[i,2]
    if(!is.na(mv)){
      sdv<-df_bt[i,3]*sqrt(34)
      Rtr[i]<-rnorm(1,mv,sdv)
    }
    else {
      Rtr[i]<-NA
    }
  }
  gam1<-gam(Rtr~s(O_year),data=df, select=TRUE, method='GCV.Cp')
  fit2[j,]<-predict(gam1,df,se=T)$fit
  fit2_se[j,]<-predict(gam1,df,se=T)$se.fit
  r.sq2[j]<-summary(gam1)$r.sq
}
head(fit2)
hist(t(fit2[,3]))
head(fit2_se)
r2<-0
# calcuate the mean and se
for(k in 1:38){
  r2[k]<-mean(fit2[,k])
}
r2
r2se<-0
summary(fit2_se[,2])
sqrt(sum((2*fit2_se[,2])^2))/1000
hist(fit2_se[,2])
for(k in 1:38){
  r2se[k]<-sqrt(sum((fit2_se[,k])^2))/1000
}
r2se
mean(r.sq2)

median(r.sq2)
sd(r.sq2)/sqrt(1000)

#### Fig S6 plot for ratio of subgroup trawlers and model explanation power in Zhejiang
max<-max(na.omit(r2+2*r2se), na.omit(df2[,31]));max
min<-min(na.omit(r2-2*r2se), na.omit(df2[,31]));min
plot(df2$[,31]~df2$year,pch=1,type="p",col="black",ylim=c(min,max),xaxt="n",xlab="",ylab="",las=1)
lines(r2~df2$year,lty="solid",col="black")
lines(r2+2*r2se~df2$year,lty="dashed",col="red")
lines(r2+2*r2se~df2$year,lty="dashed",col="red")
Axis(side=1,at=seq(1980,2014,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Ratio of subgroup trawlers (shrimp & single trawlers)\nto all trawlers in Zhejiang",side=2,col="black",line=2.5,las=0) 
mtext("a)",side=3,adj=0,cex=1)
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
hist(r.sq2,breaks=30,main="",xlab="Adjusted R-squared (i.e, model explanation power)")
abline(v=median(r.sq2),lty="dashed",col="red")
abline(v=quantile(r.sq2,probs=0.05),lty="dashed",col="blue")
abline(v=quantile(r.sq2,probs=0.95),lty="dashed",col="blue")
mtext("b)",side=3,adj=0,cex=1)
# create new data
str(df)
df2<-df
df2[,31]
df2[,33]
for(i in 1:38){
  if(is.na(df2[i,31])){
    df2[i,31]<-r2[i]
    df2[i,33]<-r2se[i]
  }
}
str(df2)
df2[,c(29,30,31,33)]
str(df2)


## calculate N_tr_ZJ
df2$N_tr_ZJ_1<-df2[,31]*df2$N_com_ZJ
df2$N_tr_ZJ_1
df2$N_tr_ZJse_1<-df2$N_tr_ZJ_1*sqrt((df2[,33]/df2[,31])^2+(df2[,29]/df2[,30])^2)
df2$N_tr_ZJse_1
write.csv(df2,"CN_recon3.csv")
# Fig S7: plot prediction based on subgroup trawlers in Zhejiang
par(mfrow=c(1,1),mar=c(4,5,1,1))
max<-max(na.omit(df2$N_tr_ZJ_1+2*df2$N_tr_ZJse_1), na.omit(df2$N_tr_ZJ))/1000;max
min<-min(na.omit(df2$N_tr_ZJ_1-2*df2$N_tr_ZJse_1), na.omit(df2$N_tr_ZJ))/1000;min
plot(df2$N_tr_ZJ/1000~df2$year,pch=1,type="p",col="black",ylim=c(min,max),xaxt="n",xlab="",ylab="",las=1)
par(new=T)
plot(df2$N_tr_ZJ_1[1:35]/1000~df2$year[1:35],pch=1,type="l",col="black",ylim=c(min,max),xaxt="n",yaxt="n",xlab="",ylab="",las=1)
lines((df2$N_tr_ZJ_1+2*df2$N_tr_ZJse_1)[1:35]/1000~df2$year[1:35],lty="dashed",col="red")
lines((df2$N_tr_ZJ_1-2*df2$N_tr_ZJse_1)[1:35]/1000~df2$year[1:35],lty="dashed",col="red")
Axis(side=1,at=seq(1980,2014,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of trawlers in Zhejiang (x1000)",side=2,col="black",line=2.5,las=0) 
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend


######### estimate for catchers in Zhejiang
gam2<-gam(N_c_ZJ~s(O_year),data=df, select=TRUE, method='GCV.Cp')
summary(gam2)
fit<-predict(gam2,df,se=T)$fit
fit_se<-predict(gam2,df,se=T)$se.fit
r.sq<-summary(gam2)$r.sq
df4

#  Fig S8 for catchers
# Fig S8a
par(mfrow=c(1,2),mar=c(4,5,1,1))
max<-max(na.omit(fit+2*fit_se), na.omit(df$N_c_ZJ))/10000;max
min<-min(na.omit(fit-2*fit_se), na.omit(df$N_c_ZJ))/10000;min
plot(df$N_c_ZJ/10000~df$year,pch=1,type="p",col="black",ylim=c(min,max),xaxt="n",xlab="",ylab="",las=1)
lines(fit/10000~year,df,pch=1,type="l",col="black",ylim=c(min,max),xaxt="n",yaxt="n",xlab="",ylab="",las=1)
lines((fit+2*fit_se)/10000~year,df,lty="dashed",col="red")
lines((fit-2*fit_se)/10000~year,df,lty="dashed",col="red")
Axis(side=1,at=seq(1980,2017,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of catchers in Zhejiang (x10,000)",side=2,col="black",line=3,las=0) 
mtext("a)",side=3,adj=0,cex=1)
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
# get values
df2$N_c_ZJse<-0
for(i in 1:38){
  if(is.na(df2$N_c_ZJ[i])){
    df2$N_c_ZJ[i]<-fit[i]
    df2$N_c_ZJse[i]<-fit_se[i]
  }
  else {
    df2$N_c_ZJse[i]<-0
  }
}
df2$N_c_ZJ
df2$N_c_ZJse
# estimate trawlers from catchers
df4$RN_tr_c_ZJ_arse<-(df4$RN_tr_c_ZJ_ar-df4$RN_tr_c_ZJ_arlw)/2
write.csv(df4,"CN_recon4.csv")
## gam for ratio of trawlers to catchers
df4<-read.csv("CN_recon4.csv",header=TRUE,sep=',')
library(mgcv)
gam1<-gam(RN_tr_c_ZJ~s(O_year),data=df4, select=TRUE, method='GCV.Cp');BIC(gam1)
summary(gam1)
plot(gam1,shade = T)
pred<-predict(gam1,df,se=T)
pred
# get values
df4$RN_tr_c_ZJ_gam<-df4$RN_tr_c_ZJ
df4$RN_tr_c_ZJ_gamse<-0
for(i in 1:38){
  if(is.na(df4$RN_tr_c_ZJ_gam[i])){
    df4$RN_tr_c_ZJ_gam[i]<-pred$fit[i]
    df4$RN_tr_c_ZJ_gamse[i]<-pred$se.fit[i]
  }
}
df4$RN_tr_c_ZJ_gam
df4$RN_tr_c_ZJ_gamse
## Fig S8b
max<-max(df4$RN_tr_c_ZJ_gam+2*df4$RN_tr_c_ZJ_gamse, na.omit(df4$RN_tr_c_ZJ));max
min<-min(df4$RN_tr_c_ZJ_gam-2*df4$RN_tr_c_ZJ_gamse, na.omit(df4$RN_tr_c_ZJ));min
plot(df4$RN_tr_c_ZJ~df4$year,pch=1,type="p",col="black",ylim=c(min,max+0.02),xaxt="n",xlab="",ylab="",las=1)
lines(df4$RN_tr_c_ZJ_gam~df4$year,col="black")
lines((df4$RN_tr_c_ZJ_gam+2*df4$RN_tr_c_ZJ_gamse)[1:24]~df4$year[1:24],lty="dashed",col="red")
lines((df4$RN_tr_c_ZJ_gam-2*df4$RN_tr_c_ZJ_gamse)[1:24]~df4$year[1:24],lty="dashed",col="red")
Axis(side=1,at=seq(1980,2017,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Ratio of trawlers to catchers in Zhejiang\n(by total number of vessels)",side=2,col="black",line=3,las=0) 
mtext("b)",side=3,adj=0,cex=1)
legend("topright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend


## calculate N_tr_ZJ
df4$N_tr_ZJ_2<-df4$RN_tr_c_ZJ_gam*df4$N_c_ZJ
df4$N_tr_ZJse_2<-df4$N_tr_ZJ_2*sqrt((df4$RN_tr_c_ZJ_gamse/df4$RN_tr_c_ZJ_gam)^2+(df4$N_c_ZJse/df4$N_c_ZJ)^2)
df4$N_tr_ZJ_2
df4$N_tr_ZJse_2
write.csv(df2,"CN_recon5.csv")
## Fig S9: plot ratio of trawler to catchers in Zhejiang
par(mfrow=c(1,1))
max<-max(df4$N_tr_ZJ_2+2*df4$N_tr_ZJse_2, na.omit(df4$N_tr_ZJ))/1000;max
min<-min(df4$N_tr_ZJ_2-2*df4$N_tr_ZJse_2, na.omit(df4$N_tr_ZJ))/1000;min
plot(df4$N_tr_ZJ/1000~df4$year,pch=1,type="p",col="black",ylim=c(min,max+0.02),xaxt="n",xlab="",ylab="",las=1)
lines(df4$N_tr_ZJ_2/1000~year,df4,col="black")
lines((df4$N_tr_ZJ_2+2*df4$N_tr_ZJse_2)[1:24]/1000~df4$year[1:24],df4,lty="dashed",col="red")
lines((df4$N_tr_ZJ_2-2*df4$N_tr_ZJse_2)[1:24]/1000~df4$year[1:24],df4,lty="dashed",col="red")
Axis(side=1,at=seq(1980,2017,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of trawlers in Zhejiang (x1000)",side=2,col="black",line=3,las=0) 
legend("topright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend

### total horsepower of trawlers in Zhejiang
## gam for ratio of trawlers to catchers
df4<-read.csv("CN_recon.csv",header=TRUE,sep=',')
str(df4)
library(mgcv)
gam1<-gam(H_tr_ZJ~s(year),data=df4, select=TRUE, method='GCV.Cp');BIC(gam1)
summary(gam1)
plot(gam1,shade = T)
pred<-predict(gam1,df,se=T)
pred
# for plot
for(i in 1:38){
df4$H_tr_ZJ_gam[i]<-pred$fit[i]
df4$H_tr_ZJ_gamse[i]<-pred$se.fit[i]
}
# get values
df4$H_tr_ZJ_gam<-df4$H_tr_ZJ
df4$H_tr_ZJ_gamse<-0
for(i in 1:38){
  if(is.na(df4$H_tr_ZJ_gam[i])){
    df4$H_tr_ZJ_gam[i]<-pred$fit[i]
    df4$H_tr_ZJ_gamse[i]<-pred$se.fit[i]
  }
  }
df4$H_tr_ZJ_gam
df4$H_tr_ZJ_gamse
str(df4)
df<-df4[,21:22]
write.csv(df,"CN_H_tr_ZJ.csv")
####
## Fig S10: plot estimated horsepower of trawlers in Zhejiang
par(mfrow=c(1,1),mar=c(4,4,1,1))
max<-max(df4$H_tr_ZJ_gam+2*df4$H_tr_ZJ_gamse, na.omit(df4$H_tr_ZJ))/1000000;max
min<-min(df4$H_tr_ZJ_gam-2*df4$H_tr_ZJ_gamse, na.omit(df4$H_tr_ZJ))/1000000;min
plot(df4$H_tr_ZJ/1000000~df4$year,pch=1,type="p",col="black",ylim=c(min,max+0.02),xaxt="n",xlab="",ylab="",las=1)
lines(df4$H_tr_ZJ_gam/1000000~year,df4,col="black")
lines((df4$H_tr_ZJ_gam+2*df4$H_tr_ZJ_gamse)/1000000~df4$year,df4,lty="dashed",col="red")
lines((df4$H_tr_ZJ_gam-2*df4$H_tr_ZJ_gamse)/1000000~df4$year,df4,lty="dashed",col="red")
Axis(side=1,at=seq(1980,2017,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total horsepower of trawlers in Zhejiang (million kW)",side=2,col="black",line=3,las=0) 
legend("topright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend

####### for trawlers in Fujian
# gam for full dataset
df<-read.csv("CN_recon.csv",header=TRUE,sep=',')
str(df)
library(mgcv)
gam1<-gam(N_tr_FJ~s(year),data=df, select=TRUE, method='GCV.Cp');BIC(gam1)
summary(gam1)

pred<-predict(gam1,df[1:38,],se=T)
pred
# for plot
for(i in 1:38){
  df$N_tr_FJ_gam[i]<-pred$fit[i]
  df$N_tr_FJ_gamse[i]<-pred$se.fit[i]
}
df$N_tr_FJ_gam
df$N_tr_FJ_gamse
# for output data and only estimate for missing values
df$N_tr_FJ_gam<-df$N_tr_FJ
df$N_tr_FJ_gamse<-0
for(i in 1:38){
  if(is.na(df$N_tr_FJ_gam[i])){
    df$N_tr_FJ_gam[i]<-pred$fit[i]
    df$N_tr_FJ_gamse[i]<-pred$se.fit[i]
  }
}
df$N_tr_FJ_gam
df$N_tr_FJ_gamse
str(df)
df0<-df[,21:22]
write.csv(df0,"CN_N_tr_FJ.csv")

####
## Fig S11a: plot estimated number of trawlers in Fujian
par(mfrow=c(1,2),mar=c(4,4,1,1))
max<-max(df$N_tr_FJ_gam+2*df$N_tr_FJ_gamse, na.omit(df$N_tr_FJ))/1000;max
min<-min(df$N_tr_FJ_gam-2*df$N_tr_FJ_gamse, na.omit(df$N_tr_FJ))/1000;min
plot(df$N_tr_FJ/1000~df$year,pch=1,type="p",col="black",ylim=c(min,max+0.02),xaxt="n",xlab="",ylab="",las=1)
lines(df$N_tr_FJ_gam/1000~year,df,col="black")
lines((df$N_tr_FJ_gam+2*df$N_tr_FJ_gamse)/1000~df$year,df,lty="dashed",col="red")
lines((df$N_tr_FJ_gam-2*df$N_tr_FJ_gamse)/1000~df$year,df,lty="dashed",col="red")
Axis(side=1,at=seq(1980,2017,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of trawlers in Fujian (x 1000)",side=2,col="black",line=3,las=0) 
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
mtext("a)",side=3,adj=0,cex=1)
####### for horsepower of trawlers in Fujian
# gam for full dataset
str(df)
gam1<-gam(H_tr_FJ~s(year),data=df, select=TRUE, method='GCV.Cp');BIC(gam1)
summary(gam1)
pred<-predict(gam1,df[1:38,],se=T)
pred
# for plot
for(i in 1:38){
  df$H_tr_FJ_gam[i]<-pred$fit[i]
  df$H_tr_FJ_gamse[i]<-pred$se.fit[i]
}
df$H_tr_FJ_gam
df$H_tr_FJ_gamse
# for output data and only estimate for missing values
df$H_tr_FJ_gam<-df$H_tr_FJ
df$H_tr_FJ_gamse<-0
for(i in 1:38){
  if(is.na(df$H_tr_FJ_gam[i])){
    df$H_tr_FJ_gam[i]<-pred$fit[i]
    df$H_tr_FJ_gamse[i]<-pred$se.fit[i]
  }
}
df$H_tr_FJ_gam
df$H_tr_FJ_gamse
str(df)
df<-df[,21:22]
write.csv(df,"CN_H_tr_FJ.csv")
## Fig S11b: plot estimated horsepower of trawlers in Fujian
max<-max(df$H_tr_FJ_gam+2*df$H_tr_FJ_gamse, na.omit(df$H_tr_FJ))/100000;max
min<-min(df$H_tr_FJ_gam-2*df$H_tr_FJ_gamse, na.omit(df$H_tr_FJ))/100000;min
plot(df$H_tr_FJ/100000~df$year,pch=1,type="p",col="black",ylim=c(min,max+0.02),xaxt="n",xlab="",ylab="",las=1)
lines(df$H_tr_FJ_gam/100000~year,df,col="black")
lines((df$H_tr_FJ_gam+2*df$H_tr_FJ_gamse)/100000~df$year,df,lty="dashed",col="red")
lines((df$H_tr_FJ_gam-2*df$H_tr_FJ_gamse)/100000~df$year,df,lty="dashed",col="red")
Axis(side=1,at=seq(1980,2017,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total horsepower of trawlers in Fujian (x 100,000 kW)",side=2,col="black",line=3,las=0) 
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
mtext("b)",side=3,adj=0,cex=1)
###
## extrapolate to 1980
############# loess model with bootstrap
library(fANCOVA)
library(car)
# for number
df<-read.csv("CN_N_tr_FJ.csv",header=TRUE,sep=',')
df0<-read.csv("CN_H_tr_FJ.csv",header=TRUE,sep=',')
str(df)
str(df0)
df1<-cbind(df,df0[,2:3])
str(df1)
fit<-data.frame(matrix(NA, nrow = 38, ncol = 1000))
fit_se<-data.frame(matrix(NA, nrow = 38, ncol = 1000))
r.sq<-0
## bootstrap method
df1[1,2:5]<-NA
str(df1)
# for N_tr
df1[,6]<-0
for(j in 1:1000){
  s<-0
  d<-0
  df1[1,6]<-NA
  for(i in 2:37){
    df1[i,6]<-rnorm(1,df1[i,2],df1[i,3]*sqrt(30))
  }
  los<-loess.as(df1[2:38,1],df1[2:38,6],criterion = c("aicc", "gcv")[1],control = loess.control(surface = "direct"))
  s<-summary(los)$p$span
  d<-summary(los)$p$degree
  los1<-loess(df1[,6]~df1[,1],degree=d,span=s,control = loess.control(surface = "direct"))
  fit[,j]<-predict(los1,df1,se=T)$fit
  fit_se[,j]<-predict(los1,df1,se=T)$se.fit
  ss.dist <- sum(scale(df1[2:38,2], scale=FALSE)^2)
  ss.resid <- sum(resid(los1)^2)
  r.sq[j]<- 1-ss.resid/ss.dist
}
fit[,1]
fit_se[,1]
hist(as.numeric(fit[1,]))
hist(as.numeric(fit_se[1,]))
# calcuate the mean and se
r<-0
for(k in 1:37){
  r[k]<-mean(t(fit[k,]))
}
r
sum(fit_se[1,])
rse<-0
for(k in 1:37){
  rse[k]<-sqrt(sum((fit_se[k,])^2))/1000
}
rse
mean(r.sq)
median(r.sq)
sd(r.sq2)/sqrt(1000)
df$N_LMC[1]<-r[1]
df$N_LMClw[1]<-r[1]-2*rse[1]
df$N_LMCup[1]<-r[1]+2*rse[1]
head(df)

####### estimate trawlers for Guangdong ##################################
####### for trawlers in Guangdong
# gam for full dataset
df<-read.csv("CN_recon.csv",header=TRUE,sep=',')
str(df)
library(mgcv)
gam1<-gam(N_tr_GD~s(year),data=df, select=TRUE, method='GCV.Cp');BIC(gam1)
summary(gam1)

pred<-predict(gam1,df[1:38,],se=T)
pred
# for plot
for(i in 1:38){
  df$N_tr_GD_gam[i]<-pred$fit[i]
  df$N_tr_GD_gamse[i]<-pred$se.fit[i]
}
df$N_tr_GD_gam
df$N_tr_GD_gamse
# for output data and only estimate for missing values
df$N_tr_GD_gam<-df$N_tr_GD
df$N_tr_GD_gamse<-0
for(i in 1:38){
  if(is.na(df$N_tr_GD_gam[i])){
    df$N_tr_GD_gam[i]<-pred$fit[i]
    df$N_tr_GD_gamse[i]<-pred$se.fit[i]
  }
}
df$N_tr_GD_gam
df$N_tr_GD_gamse
str(df)
df0<-df[,22:23]
write.csv(df0,"CN_N_tr_GD.csv")
############## using ratio of trawlers to catchers insteand
df<-read.csv("CN_recon.csv",header=TRUE,sep=',')
str(df)
library(mgcv)
gam1<-gam(RN_tr_GD~s(year),data=df, select=TRUE, method='GCV.Cp');BIC(gam1)
summary(gam1)

pred<-predict(gam1,df[1:38,],se=T)
pred
# for plot
df$RN_tr_GD_gam<-df$RN_tr_GD
df$RN_tr_GD_gamse<-0
for(i in 1:38){
  df$RN_tr_GD_gam[i]<-pred$fit[i]
  df$RN_tr_GD_gamse[i]<-pred$se.fit[i]
}
df$RN_tr_GD_gam
df$RN_tr_GD_gamse
# for output data and only estimate for missing values
df$RN_tr_GD_gam<-df$RN_tr_GD
df$RN_tr_GD_gamse<-0
for(i in 1:38){
  if(is.na(df$RN_tr_GD_gam[i])){
    df$RN_tr_GD_gam[i]<-pred$fit[i]
    df$RN_tr_GD_gamse[i]<-pred$se.fit[i]
  }
}
df$RN_tr_GD_gam
df$RN_tr_GD_gamse
str(df)
df0<-df[,22:23]
write.csv(df0,"CN_RN_tr_GD.csv")
df00<-df$N_c_GD*df0
df00
write.csv(df00,"CN_N_tr_GD_ratiobased.csv")
####

####### for horsepower of trawlers in Guangdong
# gam for ratio in terms of total horsepower
df<-read.csv("CN_recon.csv",header=TRUE,sep=',')
str(df)
gam2<-gam(H_tr_GD~s(year),data=df, select=TRUE, method='GCV.Cp');BIC(gam1)
summary(gam2)
pred<-predict(gam2,df[1:38,],se=T)
pred
# gam for ratio in terms of mean horsepower
# for plot
df$H_tr_GD_gam<-df$H_tr_GD
df$H_tr_GD_gamse<-0
for(i in 1:38){
  df$H_tr_GD_gam[i]<-pred$fit[i]
  df$H_tr_GD_gamse[i]<-pred$se.fit[i]
}
df$H_tr_GD_gam
df$H_tr_GD_gamse
# for output data and only estimate for missing values
df$H_tr_GD_gam<-df$RHpV_tr_GD
df$H_tr_GD_gamse<-0
for(i in 1:38){
  if(is.na(df$H_tr_GD_gam[i])){
    df$H_tr_GD_gam[i]<-pred$fit[i]
    df$H_tr_GD_gamse[i]<-pred$se.fit[i]
  }
}
df$H_tr_GD_gam
df$H_tr_GD_gamse
str(df)
df<-df[,23:24]
str(df)
write.csv(df,"CN_H_tr_GD.csv")

##
## Fig S12 estimates of total number and horsepower of trawlers in Guangdong based on GAMs
par(mfrow=c(1,2),mar=c(4,5,1,1))
max<-max(df$N_tr_GD_gam+2*df$N_tr_GD_gamse)/1000;max
min<-min(df$N_tr_GD_gam-2*df$N_tr_GD_gamse)/1000;min
plot(df$N_tr_GD/1000~df$year,pch=1,type="p",col="black",ylim=c(min,max+0.02),xaxt="n",xlab="",ylab="",las=1)
lines(df$N_tr_GD_gam/1000~year,df,col="black")
lines((df$N_tr_GD_gam+2*df$N_tr_GD_gamse)/1000~df$year,df,lty="dashed",col="red")
lines((df$N_tr_GD_gam-2*df$N_tr_GD_gamse)/1000~df$year,df,lty="dashed",col="red")
Axis(side=1,at=seq(1980,2017,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of trawlers in Guangdong (x 1000)",side=2,col="black",line=3,las=0) 
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
mtext("a)",side=3,adj=0,cex=1)

max<-max(df$H_tr_GD_gam+2*df$H_tr_GD_gamse)/100000;max
min<-min(df$H_tr_GD_gam-2*df$H_tr_GD_gamse)/100000;min
plot(df$H_tr_GD/100000~df$year,pch=1,type="p",col="black",ylim=c(min,max+0.02),xaxt="n",xlab="",ylab="",las=1)
lines(df$H_tr_GD_gam/100000~year,df,col="black")
lines((df$H_tr_GD_gam+2*df$H_tr_GD_gamse)/100000~df$year,df,lty="dashed",col="red")
lines((df$H_tr_GD_gam-2*df$H_tr_GD_gamse)/100000~df$year,df,lty="dashed",col="red")
Axis(side=1,at=seq(1980,2017,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total horsepower of trawlers in Guangdong (x 100,000 kW)",side=2,col="black",line=3,las=0) 
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
mtext("b)",side=3,adj=0,cex=1)

## Fig S13: plot estimated number of trawlers in Guangdong
par(mfrow=c(1,2),mar=c(4,5,1,1))
max<-max(df$RN_tr_GD_gam+2*df$RN_tr_GD_gamse);max
min<-min(df$RN_tr_GD_gam-2*df$RN_tr_GD_gamse);min
plot(df$RN_tr_GD~df$year,pch=1,type="p",col="black",ylim=c(min,max+0.02),xaxt="n",xlab="",ylab="",las=1)
lines(df$RN_tr_GD_gam~year,df,col="black")
lines((df$RN_tr_GD_gam+2*df$RN_tr_GD_gamse)~df$year,df,lty="dashed",col="red")
lines((df$RN_tr_GD_gam-2*df$RN_tr_GD_gamse)~df$year,df,lty="dashed",col="red")
Axis(side=1,at=seq(1980,2017,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Ratio between trawlers and all catchers in Guangdong",side=2,col="black",line=3,las=0) 
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
mtext("a)",side=3,adj=0,cex=1)
max<-max(df$N_c_GD*(df$RN_tr_GD_gam+2*df$RN_tr_GD_gamse))/1000;max
min<-min(df$N_c_GD*(df$RN_tr_GD_gam-2*df$RN_tr_GD_gamse))/1000;min
plot(df$N_c_GD*df$RN_tr_GD/1000~df$year,pch=1,type="p",col="black",ylim=c(min,max+0.02),xaxt="n",xlab="",ylab="",las=1)
lines(df$N_c_GD*df$RN_tr_GD_gam/1000~year,df,col="black")
lines(df$N_c_GD*(df$RN_tr_GD_gam+2*df$RN_tr_GD_gamse)/1000~df$year,df,lty="dashed",col="red")
lines(df$N_c_GD*(df$RN_tr_GD_gam-2*df$RN_tr_GD_gamse)/1000~df$year,df,lty="dashed",col="red")
Axis(side=1,at=seq(1980,2017,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of trawlers in Guangdong (x 1000)",side=2,col="black",line=3,las=0) 
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
mtext("b)",side=3,adj=0,cex=1)

#################################################################
######### subgroup trawlers to trawlers: scaling up to the national level based on reconstruction in the above three provinces
df<-read.csv("CN_tr_1980_2017.csv",header=TRUE,sep=',') ### This dataset contains the compiled reconstruction for the three provinces from 1980 to 2017
str(df)
head(df)
df3<-df[seq(dim(df)[1],1),]
df3
df3$RN_tr<-df3$N_tr/df3$N_tr_sum
df3$RN_tr
df3$RHpV_tr<-df3$HpV_tr/df3$HpV_tr_sum
df3$RHpV_tr
### arima for RN_tr (Ratio between total trawlers and the trawlers from the three provinces reconstructed above, here by number from 2003 to 2017)
library(forecast)
ari0<-auto.arima(df3$RN_tr,trace = T) 
summary(ari0)
ari0$fitted
forecast(ari0,h=23)
df3$RN_tr_mean<-ari0$fitted
df3$RN_tr_se<-0
df3
for(i in 1:38){
  if(is.na(df3$RN_tr_mean[i])){
    df3$RN_tr_mean[i]<-2.083182
    df3$RN_tr_se[i]<-0.11098
  }
}
df3
# N_tr (Total number of Chinese trawlers) based on RN_tr
for(i in 1:38){
  df3$N_tr_mean[i]<-df3$RN_tr_mean[i]*df3$N_tr_sum[i]
  if(df3$RN_tr_se[i]==0){
    df3$N_tr_se[i]<-df3$RN_tr_mean[i]*df3$N_tr_sum_se[i]
  } 
  else {
    df3$N_tr_se[i]<-df3$N_tr_mean[i]*sqrt((df3$RN_tr_se[i]/df3$RN_tr_mean[i])^2+(df3$N_tr_sum_se[i]/df3$N_tr_sum[i])^2)
  }
}
df4<-df3[seq(dim(df3)[1],1),]
df4
df$N_tr_mean<-df4$N_tr_mean
df$N_tr_se<-df4$N_tr_se
### arima for H_tr (Total Horsepower of Chinese trawlers)
library(forecast)
ari0<-auto.arima(df3$RH_tr,trace = T) # 
summary(ari0)
Box.test(ari0$residuals) ## null: the autocorrelations are all zerios. when p < 0.05, reject it.
ar_f<-forecast(ari0,h=23)
ar_f$mean
ar_f$lower[,2]

## for plot
df3$RH_tr_ar_m[1:15]<-ari0$fitted[1:15]
df3$RH_tr_ar_se<-0
df3$RH_tr_ar_m[16:38]<-ar_f$mean
df3$RH_tr_ar_se[16:38]<-(ar_f$mean-ar_f$lower[,2])/2
str(df3)
for(i in 1:38){
  df3$H_tr_mean[i]<-df3$RH_tr_ar_m[i]*df3$H_tr_sum[i]
  if(df3$RH_tr_ar_se[i]==0){
    df3$H_tr_se[i]<-df3$RH_tr_ar_m[i]*df3$H_tr_sum_se[i]
  } 
  else {
    df3$H_tr_se[i]<-df3$H_tr_mean[i]*sqrt((df3$RH_tr_ar_se[i]/df3$RH_tr_ar_m[i])^2+(df3$H_tr_sum_se[i]/df3$H_tr_sum[i])^2)
  }
}

df4<-df3[seq(dim(df3)[1],1),]
str(df4)
df$H_tr_mean<-df4$H_tr_mean
df$H_tr_se<-df4$H_tr_se
df$RH_tr_mean<-df4$RH_tr_ar_m
df$RH_tr_se<-df4$RH_tr_ar_se
## for output
write.csv(df,"CN_trawlers_1980-2017_arima.csv")

### loess for H_tr
library(fANCOVA)
str(df)
los<-loess.as(df$year[24:38],df$RH_tr[24:38],criterion = c("aicc", "gcv")[1],control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
s2<-summary(los)$p$span
d2<-summary(los)$p$degree
los2<-loess(df$RH_tr~df$year,degree=d2,span=s2,control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
fit2<-predict(los2,df,se=T)$fit
fit2_se<-predict(los2,df,se=T)$se.fit
ss.dist <- sum(scale(df$RH_tr[24:38], scale=FALSE)^2) # change N_YL to H_YL for create model for the latter
ss.resid <- sum(resid(los2)^2)
r.sq2<- 1-ss.resid/ss.dist
fit2
fit2_se
r.sq2
## for plot
df$RH_tr_mean<-fit2
df$RH_tr_se<-fit2_se
str(df)
for(i in 1:38){
  df$H_tr_ls_mean[i]<-df$RH_tr_mean[i]*df$H_tr_sum[i]
  df$H_tr_ls_se[i]<-df$H_tr_ls_mean[i]*sqrt((df$RH_tr_se[i]/df$RH_tr_mean[i])^2+(df$H_tr_sum_se[i]/df$H_tr_sum[i])^2)
}
df
## for output
write.csv(df,"CN_trawlers_1980-2017_arima_loess.csv")

### Fig S14 plot
str(df)
par(mfrow=c(1,2),mar=c(4,4,1,1))
max<-max(df$N_tr_mean+2*df$N_tr_se, na.omit(df$N_tr))/10000;max
min<-min(df$N_tr_mean-2*df$N_tr_se, na.omit(df$N_tr))/10000;min
plot(df$N_tr/10000~df$year,pch=1,type="p",col="black",ylim=c(min,max+0.02),xaxt="n",xlab="",ylab="",las=1)
lines(df$N_tr_mean/10000~year,df,col="black")
lines((df$N_tr_mean+2*df$N_tr_se)/10000~df$year,df,lty="dashed",col="red")
lines((df$N_tr_mean-2*df$N_tr_se)/10000~df$year,df,lty="dashed",col="red")
Axis(side=1,at=seq(1980,2017,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of trawlers in China (x 10,000)",side=2,col="black",line=3,las=0) 
legend("bottomright",legend=c("Mean","95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
mtext("a)",side=3,adj=0,cex=1)

max<-max(df$H_tr_mean+2*df$H_tr_se, df$H_tr_ls_mean+2*df$H_tr_ls_se)/1000000;max
min<-min(df$H_tr_mean-2*df$H_tr_se, df$H_tr_ls_mean-2*df$H_tr_ls_se)/1000000;min
plot(df$H_tr/1000000~df$year,pch=1,type="p",col="black",ylim=c(min,max+0.02),xaxt="n",xlab="",ylab="",las=1)
lines(df$H_tr_mean/1000000~year,df,col="black")
lines((df$H_tr_mean+2*df$H_tr_se)/1000000~df$year,df,lty="dashed",col="red")
lines((df$H_tr_mean-2*df$H_tr_se)/1000000~df$year,df,lty="dashed",col="red")
lines(df$H_tr_ls_mean/1000000~year,df,col="blue")
lines((df$H_tr_ls_mean+2*df$H_tr_ls_se)/1000000~df$year,df,lty="dashed",col="green")
lines((df$H_tr_ls_mean-2*df$H_tr_ls_se)/1000000~df$year,df,lty="dashed",col="green")

Axis(side=1,at=seq(1980,2017,by=5))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total horsepower of trawlers in China (million kW)",side=2,col="black",line=3,las=0) 
legend("bottomright",legend=c("Mean based on ARIMA","95% CI based on ARIMA","Mean based on LOESS","95% CI based on LOESS"),
       text.col=c("black","red","blue","green"),lty=c("solid","dashed","solid","dashed"),col=c("black","red","blue","green"))## Add Legend
mtext("b)",side=3,adj=0,cex=1)

#### Fig S15 validation plot
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")

df<-read.csv("CN_tr_val.csv",header=TRUE,sep=',')
str(df)
head(df)
par(mfrow=c(1,2),mar=c(4,4,1,1))
max<-max(df$N_tr_mean+2*df$N_tr_se, na.omit(df$N_pv))/10000;max
min<-min(df$N_tr_mean-2*df$N_tr_se, na.omit(df$N_tr_ECS))/10000;min
plot(df$N_tr_mean/10000~df$year,pch=1,type="o",col="black",ylim=c(min,max+5),xaxt="n",xlab="",ylab="",las=1)
lines((df$N_tr_mean+2*df$N_tr_se)/10000~df$year,df,lty="dashed",col="red")
lines((df$N_tr_mean-2*df$N_tr_se)/10000~df$year,df,lty="dashed",col="red")
par(new=T)
plot(df$N_pv/10000~df$year,pch=24,type="p",col="red",ylim=c(min,max+5),xaxt="n",yaxt="n",xlab="",ylab="",las=1)
par(new=T)
plot(df$N_tr_SCS/10000~df$year,pch=16,type="p",col="blue",ylim=c(min,max+5),xaxt="n",yaxt="n",xlab="",ylab="",las=1)
par(new=T)
plot(df$N_tr_ECS/10000~df$year,pch=16,type="p",col="green",ylim=c(min,max+5),xaxt="n",yaxt="n",xlab="",ylab="",las=1)
Axis(side=1,at=seq(1980,2002,by=2))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of trawlers (x 10,000)",side=2,col="black",line=3,las=0) 
legend("topleft",legend=c("China's production vessels","China's trawlers (mean)","China's trawlers (95% CI)","Trawlers in the East China Sea","Trawlers in the South China Sea"),
       text.col=c("red","black","red","green","blue"),pch=c(24,1,NA,16,16),lty=c(NA,"solid","dashed",NA,NA),col=c("red","black","red","green","blue"))## Add Legend
mtext("a)",side=3,adj=0,cex=1)

max<-max(df$H_tr_mean+2*df$H_tr_se, na.omit(df$H_pv))/1000000;max
min<-min(df$H_tr_mean-2*df$H_tr_se, na.omit(df$H_tr_ECS))/1000000;min
plot(df$H_tr_mean/1000000~df$year,pch=1,type="l",col="black",ylim=c(min,max+2),xaxt="n",xlab="",ylab="",las=1)
lines((df$H_tr_mean+2*df$H_tr_se)/1000000~df$year,df,lty="dashed",col="red")
lines((df$H_tr_mean-2*df$H_tr_se)/1000000~df$year,df,lty="dashed",col="red")
par(new=T)
plot(df$H_tr_ls_mean/1000000~year,df,pch=1,type="o",col="blue",ylim=c(min,max+2),xaxt="n",yaxt="n",xlab="",ylab="")
lines((df$H_tr_ls_mean+2*df$H_tr_ls_se)/1000000~df$year,df,lty="dashed",col="green")
lines((df$H_tr_ls_mean-2*df$H_tr_ls_se)/1000000~df$year,df,lty="dashed",col="green")
par(new=T)
plot(df$H_pv/1000000~df$year,pch=24,type="p",col="red",ylim=c(min,max+2),xaxt="n",yaxt="n",xlab="",ylab="",las=1)
par(new=T)
plot(df$H_tr_SCS/1000000~df$year,pch=16,type="p",col="blue",ylim=c(min,max+2),xaxt="n",yaxt="n",xlab="",ylab="",las=1)
par(new=T)
plot(df$H_tr_ECS/1000000~df$year,pch=16,type="p",col="green",ylim=c(min,max+2),xaxt="n",yaxt="n",xlab="",ylab="",las=1)

Axis(side=1,at=seq(1980,2002,by=2))
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total horsepower of trawlers (million kW)",side=2,col="black",line=3,las=0) 
legend("topleft",legend=c("China's production vessels","China's trawlers (mean) based on ARIMA","China's trawlers (95% CI) based on ARIMA","China's trawlers (mean) based on LOESS","China's trawlers (95% CI) based on LOESS","Trawlers in the East China Sea","Trawlers in the South China Sea"),
       text.col=c("red","black","red","blue","green","green","blue"),pch=c(24,NA,NA,1,NA,16,16),lty=c(NA,"solid","dashed","solid","dashed",NA,NA),col=c("red","black","red","blue","green","green","blue"))## Add Legend
mtext("b)",side=3,adj=0,cex=1)

#### Please fill gap in 1979 mannually using the mean of 1980 and 1978 in excel, before running the below code.
########################################################################################
## plot the trajectory from 1950 - 2017
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")

df<-read.csv("CN_tr_full.csv",header=TRUE,sep=',') ## This dataset contains the compiled fishing capacity data of Chinese trawlers based on above analyses.
str(df)
head(df)
par(mfrow=c(1,1),mar=c(4,4,1,1))
## Fig S16 total number of trawlers from 1950 - 2017
max<-max(df$N_tr+2*df$N_tr_se)/10000;max
min<-min(df$N_tr-2*df$N_tr_se)/10000;min
plot(df$N_tr[54:78]/10000~df$year[54:78],pch=1,type="p",col="black",xlim=c(1950,2020),ylim=c(min,max),xaxt="n",xlab="",ylab="",las=1)
lines(df$N_tr/10000~df$year,df,lty="solid",col="black")
lines((df$N_tr+2*df$N_tr_se)[1:54]/10000~df$year[1:54],df,lty="dashed",col="red")
lines((df$N_tr-2*df$N_tr_se)[1:54]/10000~df$year[1:54],df,lty="dashed",col="red")
Labels<-c("Economic reform 1978",
          "South Tour Speech 1992\n(Increasing reform speed)",
          "Double Control 1997")
abline(v=c(1978,1992,1997),lty="dotted",col="grey50")
text(x=c(1978,1992,1997),y=c(5,4,8),labels = Labels,cex=0.8,pos=4,col=c("grey50","grey50","grey50"))

Axis(side=1,at=seq(1950,2017,by=10))
ticks = seq(1950,2017,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of trawlers (x 10,000)",side=2,col="black",line=3,las=0) 
legend("topleft",legend=c("Mean", "95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend

## Fig S17 total horsepower of trawlers from 1950 - 2017

max<-max(df$H_tr+2*df$H_tr_se)/1000000;max
min<-min(df$H_tr-2*df$H_tr_se)/1000000;min
plot(df$H_tr[54:78]/1000000~df$year[54:78],pch=1,type="p",col="black",xlim=c(1950,2020),ylim=c(min,max),xaxt="n",xlab="",ylab="",las=1)
lines(df$H_tr/1000000~df$year,df,lty="solid",col="black")
lines((df$H_tr+2*df$H_tr_se)[1:54]/1000000~df$year[1:54],df,lty="dashed",col="red")
lines((df$H_tr-2*df$H_tr_se)[1:54]/1000000~df$year[1:54],df,lty="dashed",col="red")
Labels<-c("Economic reform 1978",
          "South Tour Speech 1992\n(Increasing reform speed)",
          "Double Control 1997")
abline(v=c(1978,1992,1997),lty="dotted",col="grey50")
text(x=c(1978,1992,1997),y=c(6,4,8.3),labels = Labels,cex=0.8,pos=4,col=c("grey50","grey50","grey50"))

Axis(side=1,at=seq(1950,2017,by=10))
ticks = seq(1950,2017,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total horsepower of trawlers (million kW)",side=2,col="black",line=3,las=0) 
legend("topleft",legend=c("Mean", "95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
######################################################################################################################################################

####### From here, we start to estimate fishing capacity for distant-water catchers
################### estimation for distant water vessels ################################
###
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
df<-read.csv("CN_dv.csv",header=TRUE,sep=',')
str(df)
head(df)
### arima for RN_dvc (Ratio between distant-water catchers and distant-water vessels, by number)
df3<-df
library(forecast)
df3
ari0<-auto.arima(df3$RN_dvc[1:18],trace = T) # 
summary(ari0)
arim3<-forecast(ari0,h=16)
arim3
df$RN_dvc_ar_m<-0
df$RN_dvc_ar_m[1:18]<-ari0$fitted[1:18]
df$RN_dvc_ar_m[19:34]<-arim3$mean
df$RN_dvc_ar_se<-0
df$RN_dvc_ar_se[19:34]<-(arim3$mean-arim3$lower[,2])/2
df


### arima for RH_dvc (Ratio between distant-water catchers and distant-water vessels, by horsepower)
library(forecast)
df3
ari0<-auto.arima(df3$RH_dvc[1:18],trace = T) # 
summary(ari0)
ari0$fitted
arim3<-forecast(ari0,h=16)
arim3
df3$RH_dvc_ar_m<-0
df3$RH_dvc_ar_m[1:18]<-ari0$fitted[1:18]
df3$RH_dvc_ar_m[19:34]<-arim3$mean
df3$RH_dvc_ar_se<-0
df3$RH_dvc_ar_se[19:34]<-(arim3$mean-arim3$lower[,2])/2
df3
df3

## for output
str(df)
write.csv(df3,"CN_dvc.csv")
## Fig x plot RN_dvc estimates
par(mfrow=c(1,2),mar=c(4,4,1,1))
max<-max(df3$RN_dvc_ar_m+2*df3$RN_dvc_ar_se);max
min<-min(df3$RN_dvc_ar_m-2*df3$RN_dvc_ar_se);min
plot(df3$RN_dvc~df3$year,pch=1,type="p",col="black",xlim=c(1985,2018),ylim=c(min,1),xaxt="n",xlab="",ylab="",las=1)
lines(df3$RN_dvc_ar_m~df$year,df,lty="solid",col="black")
lines((df3$RN_dvc_ar_m+2*df3$RN_dvc_ar_se)~df$year,df,lty="dashed",col="red")
lines((df3$RN_dvc_ar_m-2*df3$RN_dvc_ar_se)~df$year,df,lty="dashed",col="red")
Axis(side=1,at=seq(1985,2018,by=5))
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Ratio between DWCs and DWVs in China based on ARIMA",side=2,col="black",line=3,las=0) 
legend("topright",legend=c("Mean", "95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
mtext("a)",side=3,adj=0,cex=1)
####
max<-max(df3$RH_dvc_ar_m+2*df3$RH_dvc_ar_se);max
min<-min(df3$RH_dvc_ar_m-2*df3$RH_dvc_ar_se);min
plot(df3$RH_dvc~df3$year,pch=1,type="p",col="black",xlim=c(1985,2018),ylim=c(min,max+0.04),xaxt="n",xlab="",ylab="",las=1)
lines(df3$RH_dvc_ar_m~df3$year,df,lty="solid",col="black")
lines((df3$RH_dvc_ar_m+2*df3$RH_dvc_ar_se)~df$year,df,lty="dashed",col="red")
lines((df3$RH_dvc_ar_m-2*df3$RH_dvc_ar_se)~df$year,df,lty="dashed",col="red")
abline(h=1,lty="dashed",col="grey50")
Axis(side=1,at=seq(1985,2018,by=10))
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Ratio between DWCs and DWVs in China based on ARIMA",side=2,col="black",line=3,las=0) 
legend("topright",legend=c("Mean", "95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
mtext("b)",side=3,adj=0,cex=1)

#### loess for RN_dvc
library(fANCOVA)
los<-loess.as(df$year[1:18],df$RN_dvc[1:18],criterion = c("aicc", "gcv")[1],control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
s2<-summary(los)$p$span
d2<-summary(los)$p$degree
los2<-loess(df$RN_dvc~df$year,degree=d2,span=s2,control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
fit2<-predict(los2,df,se=T)$fit
fit2_se<-predict(los2,df,se=T)$se.fit
ss.dist <- sum(scale(df$RN_dvc[1:18], scale=FALSE)^2) # change N_YL to H_YL for create model for the latter
ss.resid <- sum(resid(los2)^2)
r.sq2<- 1-ss.resid/ss.dist
fit2
fit2_se
r.sq2
## for plot
df$RN_dvc_mean<-fit2
df$RN_dvc_se<-fit2_se

### loess for RH_dvc
library(fANCOVA)
los<-loess.as(df$year[1:18],df$RH_dvc[1:18],criterion = c("aicc", "gcv")[1],control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
s2<-summary(los)$p$span
d2<-summary(los)$p$degree
los2<-loess(df$RH_dvc~df$year,degree=d2,span=s2,control = loess.control(surface = "direct")) # change N_YL to H_YL for create model for the latter
fit2<-predict(los2,df,se=T)$fit
fit2_se<-predict(los2,df,se=T)$se.fit
ss.dist <- sum(scale(df$RH_dvc[1:18], scale=FALSE)^2) # change N_YL to H_YL for create model for the latter
ss.resid <- sum(resid(los2)^2)
r.sq2<- 1-ss.resid/ss.dist
fit2
fit2_se
r.sq2
## for plot
df$RH_dvc_mean<-fit2
df$RH_dvc_se<-fit2_se
df
## Fig x plot RN_dvc estimates
par(mfrow=c(1,2),mar=c(4,4,1,1))
max<-max(df$RN_dvc_mean+2*df$RN_dvc_se,na.omit(df$RN_dvc));max
min<-min(df$RN_dvc_mean-2*df$RN_dvc_se);min
plot(df$RN_dvc~df3$year,pch=1,type="p",col="black",xlim=c(1985,2018),ylim=c(min,1.05),xaxt="n",xlab="",ylab="",las=1)
lines(df$RN_dvc_mean~df$year,df,lty="solid",col="black")
lines((df$RN_dvc_mean+2*df$RN_dvc_se)~df$year,df,lty="dashed",col="red")
lines((df$RN_dvc_mean-2*df$RN_dvc_se)~df$year,df,lty="dashed",col="red")
Axis(side=1,at=seq(1985,2018,by=10))
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Ratio between DWCs and DWVs in China based on LOESS",side=2,col="black",line=3,las=0) 
legend("topleft",legend=c("Mean", "95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
mtext("a)",side=3,adj=0,cex=1)
####
max<-max(df$RH_dvc_mean+2*df$RH_dvc_se,na.omit(df$RH_dvc));max
min<-min(df$RH_dvc_mean-2*df$RH_dvc_se);min
plot(df$RH_dvc~df3$year,pch=1,type="p",col="black",xlim=c(1985,2018),ylim=c(min,1.1),xaxt="n",xlab="",ylab="",las=1)
lines(df$RH_dvc_mean~df$year,df,lty="solid",col="black")
lines((df$RH_dvc_mean+2*df$RH_dvc_se)~df$year,df,lty="dashed",col="red")
lines((df$RH_dvc_mean-2*df$RH_dvc_se)~df$year,df,lty="dashed",col="red")
abline(h=1,lty="dashed",col="grey50")
abline(v=2015,lty="dashed",col="grey50")
Axis(side=1,at=seq(1985,2018,by=10))
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Ratio between DWCs and DWVs in China based on LOESS",side=2,col="black",line=3,las=0) 
legend("topleft",legend=c("Mean", "95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
mtext("b)",side=3,adj=0,cex=1)
## calcuate mean and se directly for RH_dvc
RN_mean<-mean(na.omit(df$RN_dvc))
RN_se<-sd(na.omit(df$RN_dvc))/sqrt(18)
RN_mean
RN_se
RH_mean<-mean(na.omit(df$RH_dvc))
RH_se<-sd(na.omit(df$RH_dvc))/sqrt(18)
df$N_dvc_m<-df$N_dvc
df$N_dvc_se<-0
df$H_dvc_m<-df$H_dvc
df$H_dvc_se<-0
df
for(i in 1:34){
  if(is.na(df$N_dvc_m[i])){
    df$N_dvc_m[i]<-df$N_dv[i]*RN_mean
    df$N_dvc_se[i]<-df$N_dv[i]*RN_se
    df$H_dvc_m[i]<-df$H_dv[i]*RH_mean
    df$H_dvc_se[i]<-df$H_dv[i]*RH_se
  }
}
df

## for output
str(df)
write.csv(df,"CN_dvc_estimated2.csv")
df3<-df
## Fig S18 plot N_dvc and H_dvc estimates
par(mfrow=c(1,2),mar=c(4,4,1,1))
max<-max(df3$N_dvc_m+2*df3$N_dvc_se,df3$N_dv)/1000;max
min<-min(df3$N_dvc_m-2*df3$N_dvc_se)/1000;min
plot(df3$N_dvc/1000~df3$year,pch=1,type="p",col="black",xlim=c(1985,2018),ylim=c(min,max),xaxt="n",xlab="",ylab="",las=1)
lines(df3$N_dvc_m/1000~df$year,df,lty="solid",col="black")
lines((df3$N_dvc_m+2*df3$N_dvc_se)[18:34]/1000~df$year[18:34],df,lty="dashed",col="red")
lines((df3$N_dvc_m-2*df3$N_dvc_se)[18:34]/1000~df$year[18:34],df,lty="dashed",col="red")
lines(df3$N_dv/1000~df$year,df,lty="solid",col="blue")
Axis(side=1,at=seq(1985,2018,by=5))
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of vessels (x 1000)",side=2,col="black",line=3,las=0) 
legend("topleft",legend=c("Distant-water vessels (reported)","Distant-water catchers (Mean, estimated)", "Distant-water catchers (95% CI, estimated)"),
       text.col=c("blue","black","red"),lty=c("solid","solid","dashed"),col=c("blue","black","red"))## Add Legend
mtext("a)",side=3,adj=0,cex=1)
####
max<-max(df3$H_dvc_m+2*df3$H_dvc_se,df$H_dv)/1000000;max
min<-min(df3$H_dvc_m-2*df3$H_dvc_se)/1000000;min
plot(df3$H_dvc/1000000~df3$year,pch=1,type="p",col="black",xlim=c(1985,2018),ylim=c(min,max+0.04),xaxt="n",xlab="",ylab="",las=1)
lines(df3$H_dvc_m/1000000~df3$year,df,lty="solid",col="black")
lines((df3$H_dvc_m+2*df3$H_dvc_se)[18:34]/1000000~df$year[18:34],df,lty="dashed",col="red")
lines((df3$H_dvc_m-2*df3$H_dvc_se)[18:34]/1000000~df$year[18:34],df,lty="dashed",col="red")
lines(df3$H_dv/1000000~df$year,df,lty="solid",col="blue")
Axis(side=1,at=seq(1985,2018,by=10))
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total horsepower of vessels (million kW)",side=2,col="black",line=3,las=0) 
legend("topleft",legend=c("Distant-water vessels (reported)","Distant-water catchers (Mean, estimated)", "Distant-water catchers (95% CI, estimated)"),
       text.col=c("blue","black","red"),lty=c("solid","solid","dashed"),col=c("blue","black","red"))## Add Legend
mtext("b)",side=3,adj=0,cex=1)
#################################################################################################################################
#############################################################################################################################

########## From here, we begin to estimate fishing capacity for distant-water bottom trawlers
#######################################################################################
############### estimate distant-water bottom trawlers based on catchers ###############
## gam for ratio of trawlers to catchers
df4<-read.csv("CN_dbt_estimated.csv",header=TRUE,sep=',')
str(df4)
library(mgcv)
# estimate N_dbt based on its own data and gam
gam1<-gam(N_dbt~s(year,k=3),data=df4, select=TRUE,method='GCV.Cp');BIC(gam1)
summary(gam1)
plot(gam1,shade = T)
pred<-predict(gam1,df4,se=T)
pred
# for plot
df4$N_dbt_gam<-df4$N_dbt
df4$N_dbt_gamse<-0
for(i in 1:34){
  if(is.na(df4$N_dbt_gam[i])){
    df4$N_dbt_gam[i]<-pred$fit[i]
    df4$N_dbt_gamse[i]<-pred$se.fit[i]
  }
}

# estimate N_dbt based on the ratio between N_dbt and N_dvc
gam1<-gam(RN_dbt~s(year,k=3),data=df4, select=TRUE,method='GCV.Cp');BIC(gam1)
summary(gam1) # R-sq = .78; s(year) = 1.822, p < 0.01
plot(gam1,shade = T)
pred<-predict(gam1,df4,se=T)
pred
# for plot
df4$RN_dbt_gam<-df4$RN_dbt
df4$RN_dbt_gamse<-0
for(i in 1:34){
  if(is.na(df4$RN_dbt_gam[i])){
    df4$RN_dbt_gam[i]<-pred$fit[i]
    df4$RN_dbt_gamse[i]<-pred$se.fit[i]
  }
}
par(mfrow=c(1,1),mar=c(4,4,1,1))
max<-max(df4$RN_dbt_gam+2*df4$RN_dbt_gamse, na.omit(df4$RN_dbt));max
min<-min(df4$RN_dbt_gam-2*df4$RN_dbt_gamse, na.omit(df4$RN_dbt));min
plot(df4$RN_dbt~df4$year,pch=1,type="p",col="black",ylim=c(min,max),xaxt="n",xlab="",ylab="",las=1)
lines(df4$RN_dbt_gam~df4$year,col="black")
lines((df4$RN_dbt_gam+2*df4$RN_dbt_gamse)~df4$year,lty="dashed",col="red")
lines((df4$RN_dbt_gam-2*df4$RN_dbt_gamse)~df4$year,lty="dashed",col="red")
Axis(side=1,at=seq(1985,2018,by=5))
ticks = seq(1985,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
# for output
write.csv(df4,"CN_dbt_estimated.csv")
##

## gam for RH_ZN_tw, i.e.,ratio between trawlers and catchers by horsepower (based on trawlers and all catchers of a major Chinese DWF company, the China National Fisheries Co. Ltd. (2003) and its successor the China National Agricultural Development Group Co. Ltd. (2004 to 2017))
df4<-read.csv("CN_dbt_estimated.csv",header=TRUE,sep=',')
str(df4)
library(mgcv)
gam1<-gam(RH_ZN_tw~s(year),data=df4, select=TRUE,method='GCV.Cp');BIC(gam1)
summary(gam1)
plot(gam1,shade = T)
pred<-predict(gam1,df,se=T)
pred
# for plot
df4$RH_ZN_tw_gam[1:34]<-pred$fit
df4$RH_ZN_tw_gamse[1:34]<-pred$se.fit

##
## Fig S20
df<-df4
df
par(mfrow=c(1,1),mar=c(4,4,1,1))
max<-max(df$RH_ZN_tw_gam+2*df$RH_ZN_tw_gamse, na.omit(df$RH_ZN_tw));max
min<-min(df$RH_ZN_tw_gam-2*df$RH_ZN_tw_gamse, na.omit(df$RH_ZN_tw));min
plot(df$RH_ZN_tw~df$year,pch=1,type="p",col="black",ylim=c(min,max),xaxt="n",xlab="",ylab="",las=1)
lines(df$RH_ZN_tw_gam~df$year,col="black")
lines((df$RH_ZN_tw_gam+2*df$RH_ZN_tw_gamse)~df$year,lty="dashed",col="red")
lines((df$RH_ZN_tw_gam-2*df$RH_ZN_tw_gamse)~df$year,lty="dashed",col="red")
Axis(side=1,at=seq(1985,2018,by=5))
ticks = seq(1985,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Ratio between DWBTs and DWCs (by horsepower)",side=2,col="black",line=3,las=0) 
legend("topright",legend=c("Mean", "95% confidence intervals"),
       text.col=c("black","red"),lty=c("solid","dashed"),col=c("black","red"))## Add Legend
# for output
df4$RH_ZN_tw_gam<-df4$RH_ZN_tw
df4$RH_ZN_tw_gamse<-0
for(i in 1:34){
  if(is.na(df4$RH_ZN_tw_gam[i])){
    df4$RH_ZN_tw_gam[i]<-pred$fit[i]
    df4$RH_ZN_tw_gamse[i]<-pred$se.fit[i]
  }
}
df4
# for output
write.csv(df4,"CRH_ZN_tw_estimated2.csv")
## calculate total horsepower of DWBTs
df<-df4
df$H_dbt_m<-df$H_dbt
df$H_dbt_se<-0
df
for(i in 1:18){
  if(is.na(df$H_dbt_m[i])){
    df$H_dbt_m[i]<-df$RH_ZN_tw_gam[i]*df$H_dvc[i]
    df$H_dbt_se[i]<-df$H_dvc[i]*df$RH_ZN_tw_gamse[i]
  }
}
df
for(i in 19:34){
    df$H_dbt_m[i]<-df$RH_ZN_tw_gam[i]*df$H_dvc[i]
    df$H_dbt_se[i]<-df$RH_ZN_tw_gam[i]*df$H_dvc_se[i]
}
df
# for output
write.csv(df,"CRH_ZN_tw_estimated3.csv")

## Fig S19a
str(df)
df4<-df
par(mfrow=c(1,2),mar=c(4,4,1,1))
max<-max(df4$N_dbt_gam+2*df4$N_dbt_gamse, na.omit(df4$N_dvc))/1000;max
min<-min(df4$N_dbt_gam-2*df4$N_dbt_gamse, na.omit(df4$N_dbt))/1000;min
plot(df4$N_dbt/1000~df4$year,pch=1,type="p",col="black",ylim=c(0,max),xaxt="n",xlab="",ylab="",las=1)
lines(df4$N_dbt_gam/1000~df4$year,col="black")
lines((df4$N_dbt_gam+2*df4$N_dbt_gamse)/1000~df4$year,lty="dashed",col="red")
lines((df4$N_dbt_gam-2*df4$N_dbt_gamse)/1000~df4$year,lty="dashed",col="red")
lines(df4$N_dvc/1000~df4$year,df,lty="solid",col="blue")
Axis(side=1,at=seq(1985,2018,by=5))
ticks = seq(1985,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total number of vessels (x 1000)",side=2,col="black",line=3,las=0) 
mtext("a)",side=3,adj=0,cex=1)
legend("topleft",legend=c("Distant-water catchers (Mean, estimated)","Distant-water bottom trawlers (Mean, estimated)", "Distant-water bottom trawlers (95% CI, estimated)"),
       text.col=c("blue","black","red"),lty=c("solid","solid","dashed"),col=c("blue","black","red"))## Add Legend

## Fig S19b
str(df)
df
max<-max(df$H_dbt_m+2*df$H_dbt_se, na.omit(df$H_dvc))/1000000;max
min<-min(df$H_dbt_m-2*df$H_dbt_se, na.omit(df$H_dvc))/1000000;min
plot(df$H_dbt/1000000~df$year,pch=1,type="p",col="black",ylim=c(0,max+0.2),xaxt="n",xlab="",ylab="",las=1)
lines(df$H_dbt_m/1000000~df$year,col="black")
lines((df$H_dbt_m+2*df$H_dbt_se)/1000000~df$year,lty="dashed",col="red")
lines((df$H_dbt_m-2*df$H_dbt_se)/1000000~df$year,lty="dashed",col="red")
lines(df$H_dvc/1000000~df$year,df,lty="solid",col="blue")
Axis(side=1,at=seq(1985,2018,by=5))
ticks = seq(1985,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
mtext("Total horsepower of vessels (million kW)",side=2,col="black",line=3,las=0) 
mtext("b)",side=3,adj=0,cex=1)
legend("topleft",legend=c("Distant-water catchers (Mean, estimated)","Distant-water bottom trawlers (Mean, estimated)", "Distant-water bottom trawlers (95% CI, estimated)"),
       text.col=c("blue","black","red"),lty=c("solid","solid","dashed"),col=c("blue","black","red"))## Add Legend
