############# Figures and maps created in the main text and supplementary information
setwd("C:/Users/xiong/Desktop/BTF_China")
### Plot figures

####### Fig. 1
## read data
sh<-read.csv("Figure_dataset2.csv",header=TRUE,sep=',')
str(sh)
tiff("Figure/Fig.1 within & beyond C4S.tif",width = 12, height = 8.8, units = 'in', res = 300)
par(mar=c(2,3.5,1,3.5),mfrow=c(2,2),oma=c(0,0,0,0),las=1)
max2<-max(na.omit(sh$H_dobt2+2*sh$H_dobt2_se))/1000000
max2
min2<-min(na.omit(sh$H_dobt2))/1000000
plot(sh$H_dobt2/1000000~sh$year,type='l',lwd=2,pch=2,col="black",ylab="",xlab="",xlim=c(1950,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-1,14), rev(rep(4200,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-1,16), rev(rep(4200,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-1,19), rev(rep(4200,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:69],rev(sh$year[47:69])),
        y=c(rep(-1,23), rev(rep(4200,23))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$H_dobt2-2*sh$H_dobt2_se)/1000000, rev((sh$H_dobt2+2*sh$H_dobt2_se)/1000000)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,12.00,1)
axis(side = 2, at = ticks,tck=-0.01,col.ticks = "black",labels = NA)
ticks = seq(0,12.00,3.00)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks)
mtext("Nominal fishing power in C4S (GW)",side=2,col="black",line=2,cex=1,font=2,las=0) 
par(new=T)
max2<-max(na.omit(sh$H_dobt3+2*sh$H_dobt3_se2))/1000000
max2
min2<-min(na.omit(sh$H_dobt3))/1000000
plot(sh$H_dobt3/1000000~sh$year,type='l',lwd=2,pch=2,col="blue",ylab="",xlab="",xlim=c(1950,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$H_dobt3-2*sh$H_dobt3_se2)/1000000, rev((sh$H_dobt3+2*sh$H_dobt3_se2)/1000000)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
mtext("Effective fishing power in C4S (GW)",side=4,col="blue",line=2,cex=1,font=2,las=0) 

ticks = seq(0,46.00,1)
axis(side = 4, at = ticks,tck=-0.01,col.ticks = "blue",labels = NA, col = "blue",col.axis="blue")
ticks = seq(0,46,10)
axis(side = 4, at = ticks,tck=-0.03,labels = ticks,col.ticks = "blue", col = "blue",col.axis="blue")
mtext("a",side=3,adj=0,cex=1,font=2)

abline(v=c(1978,1998,2002,2006,2015),lty="dotted",col="grey50")
text(x=1979,y=5,labels = "1978:\nEconomic reform\n(vessel privitization)",pos=2,col="black",cex=1)
text(x=1998,y=41,labels = "1997:\nDouble\ncontrol",pos=2,col="black",cex=1)
text(x=2001,y=43,labels = "2002:\nVessel\nbuyback",pos=4,col="black",cex=1)
text(x=2007,y=15,labels = "2006:\nFuel\nsubsidy",pos=2,col="black",cex=1)
text(x=2016,y=5,labels = "2015:\nFuel\nsubsidy\nreduction",pos=2,col="black",cex=1)

## b) fishing time
max2<-max(na.omit(sh$Fd_dobt+2*sh$Fd_dobt_se))/100
max2
min2<-min(na.omit(sh$Fd_dobt))/100
plot(sh$Fd_dobt/100~sh$year,type='l',lwd=2,pch=2,col="black",ylab="",xlab="",xlim=c(1950,2018),ylim=c(1,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-1,14), rev(rep(4200,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-1,16), rev(rep(4200,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-1,19), rev(rep(4200,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:69],rev(sh$year[47:69])),
        y=c(rep(-1,23), rev(rep(4200,23))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$Fd_dobt-2*sh$Fd_dobt_se)/100, rev((sh$Fd_dobt+2*sh$Fd_dobt_se)/100)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,3,.1)
axis(side = 2, at = ticks,tck=-0.01,col.ticks = "black",labels = NA)
ticks = seq(0,3,1)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks)
mtext("Nominal fishing time in C4S (x 100 days / vessel)",side=2,col="black",line=2,cex=1,font=2,las=0) 
par(new=T)
max2<-max(na.omit(sh$Fh_dobt+2*sh$Fh_dobt_se))/1000
max2
min2<-min(na.omit(sh$Fh_dobt))/1000
plot(sh$Fh_dobt/1000~sh$year,type='l',lwd=2,pch=2,col="blue",ylab="",xlab="",xlim=c(1950,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$Fh_dobt-2*sh$Fh_dobt_se)/1000, rev((sh$Fh_dobt+2*sh$Fh_dobt_se)/1000)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)

abline(v=c(1985,1998,1999,2003),lty="dotted",col="grey50")
text(x=1985,y=2.2,labels = "1985:\nFishing offshore",pos=2,col="black",cex=1)
text(x=1998,y=3,labels = "1998:\nAsian\nfinancial\ncrisis",pos=2,col="black",cex=1)
text(x=1999,y=1.8,labels = "1999:\nSummer\nmoratorium\n(all C4S)",pos=2,col="black",cex=1)
text(x=2002,y=1.7,labels = "2003:\nSummer\nmoratorium\n(beam trawlers)",pos=4,col="black",cex=1)
mtext("Effective fishing time in C4S (x 1000 hours / vessel)",side=4,col="blue",line=2,cex=1,font=2,las=0) 
ticks = seq(0,3.1,0.1)
axis(side = 4, at = ticks,tck=-0.01,col.ticks = "blue",labels = NA, col = "blue",col.axis="blue")
ticks = seq(0,3.1,1.00)
axis(side = 4, at = ticks,tck=-0.03,labels = ticks, col.ticks = "blue", col = "blue",col.axis="blue")
mtext("b",side=3,adj=0,cex=1,font=2)

## c)
par(mar=c(2,4,1,4))
max<-max(na.omit(sh$Ne_dobt+2*sh$Ne_dobt_se))/100
max
min<-min(na.omit(sh$Ne_dobt-2*sh$Ne_dobt_se))/100
min
plot(sh$Ne_dobt/100~sh$year,type='l',col="black",lwd=2,ylim=c(min,max),xlim=c(1950,2018),ylab="",xlab="",xaxt='n',yaxt="n")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-1,14), rev(rep(19,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-1,16), rev(rep(19,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-1,19), rev(rep(19,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:69],rev(sh$year[47:69])),
        y=c(rep(-1,23), rev(rep(19,23))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$Ne_dobt-2*sh$Ne_dobt_se)/100, rev((sh$Ne_dobt+2*sh$Ne_dobt_se)/100)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
mtext("Nominal fishing effort in C4S,\nindexed to 1950 (x 100)",font=2,side=2,col="black",line=1.8,cex=1,las=0) 
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,col.ticks = "black",labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,10,1)
axis(side = 2, at = ticks,tck=-0.01,col.ticks = "black",labels = NA)
ticks = seq(0,10,2)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks,col.ticks = "black", col = "black",col.axis="black")

par(new=T)
max<-max(na.omit(sh$Ee_dobt+2*sh$Ee_dobt_se2))/1000
max
min<-min(na.omit(sh$Ee_dobt-2*sh$Ee_dobt_se2))/1000
min
plot(sh$Ee_dobt/1000~sh$year,type='l',col="blue",lwd=2,ylim=c(min,max),ylab="",xlab="",xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$Ee_dobt-2*sh$Ee_dobt_se2)/1000, rev((sh$Ee_dobt+2*sh$Ee_dobt_se2)/1000)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
mtext("Effective fishing effort in C4S,\nindexed to 1950 (x 1000)",font=2,side=4,col="blue",line=2.8,cex=1,las=0) 
mtext("c",side=3,adj=0,cex=1,font=2)
ticks = seq(0,6,1)
axis(side = 4, at = ticks,tck=-0.01,col.ticks = "blue",labels = NA,,col.axis="blue")
ticks = seq(0,6,2)
axis(side = 4, at = ticks,tck=-0.03,labels = ticks,col.ticks = "blue",col = "blue",col.axis="blue")

## d)
str(sh)
max<-max(na.omit(sh$CPUE_nom+2*sh$CPUE_nom_se))
max
min<-min(na.omit(sh$CPUE_nom-2*sh$CPUE_nom_se))
min
plot(sh$CPUE_nom~sh$year,type='l',col="black",lwd=2,xlim=c(1950,2018),ylim=c(0,max),ylab="",xlab="",las=1,axes=FALSE)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:69],rev(sh$year[47:69])),
        y=c(rep(-10,23), rev(rep(24,23))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CPUE_nom-2*sh$CPUE_nom_se, rev(sh$CPUE_nom+2*sh$CPUE_nom_se)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
mtext("Nominal CPUE in C4S, indexed to 1950",side=2,font=2,col="black",line=1.8,cex=1,las=0) 
box()
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,2.3,.1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,2.5,1)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks)
par(new=T)
max<-max(na.omit(sh$CPUE_eff+2*sh$CPUE_eff_se))
max
min<-min(na.omit(sh$CPUE_eff-2*sh$CPUE_eff_se))
min
plot(sh$CPUE_eff~sh$year,type='l',col="blue",lwd=2,xlim=c(1950,2018),ylim=c(0,max),ylab="",xlab="",las=1,axes=FALSE)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CPUE_eff-2*sh$CPUE_eff_se, rev(sh$CPUE_eff+2*sh$CPUE_eff_se)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
ticks = seq(0,3,0.1)
axis(side = 4, at = ticks,tck=-0.01,col.ticks = "blue",labels = NA)
ticks = seq(0,3,1)
axis(side = 4, at = ticks,tck=-0.03,labels = ticks,col.ticks = "blue", col = "blue",col.axis="blue")
mtext("Effective CPUE in C4S, indexed to 1950",side=4,font=2,col="blue",line=2.2,cex=1,las=0) 
abline(v=c(1955,1963,1981),lty="dotted",col="grey50")
text(x=1955,y=.2,labels = "1955:\nNo-trawl\nzone",pos=4,col="black",cex=1)
text(x=1963,y=1.8,labels = "1963:\nMajor\ntechnology\ndevelopment",pos=4,col="black",cex=1)
text(x=1981,y=.8,labels = "1981:\nSummer\nmoratorium\n(interim provision)",pos=4,col="black",cex=1)
mtext("d",side=3,adj=0,cex=1,font=2)

dev.off()

####### Fig. 2
## read data
sh<-read.csv("Figure_dataset3.csv",header=TRUE,sep=',')
str(sh)
#tiff("Figure/Fig.2 beyond C4S.tif",width = 12, height = 4.4, units = 'in', res = 300)
# Fishing power a)
max<-max(na.omit(sh$H_dbt+2*sh$H_dbt_se))/1000000
max
min<-min(na.omit(sh$H_dbt))/1000000
min
par(mar=c(2,4,1,4),mfrow=c(1,2))
plot(sh$H_dbt/1000000~sh$year,type='l',lwd=2,pch=1,col="black",xlim=c(1984,2018),ylim=c(min,max),ylab="",xlab="",axes=F)
polygon(x=c(sh$year[1:12],rev(sh$year[1:12])),
        y=c(rep(-1,12), rev(rep(1.6,12))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[12:34],rev(sh$year[12:34])),
        y=c(rep(-1,23), rev(rep(1.6,23))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$H_dbt-2*sh$H_dbt_se)/1000000, rev((sh$H_dbt+2*sh$H_dbt_se)/1000000)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
box()
mtext("Nominal fishing power beyond C4S (GW)",side=2,col="black",line=2.5,font=2,cex=1,las=0) 
ticks = seq(0,1.5,.1)
axis(side = 2, at = ticks,tck=-0.01,col.ticks = "black",labels = NA)
ticks = round(seq(0,1.5,0.3),1)
axis(side = 2, at = ticks,tck=-0.03,labels = sprintf("%0.1f", ticks))
par(new=T)
max<-max(na.omit(sh$H_dbt2+2*sh$H_dbt2_se))/1000000
max
min<-min(na.omit(sh$H_dbt2))/1000000
min
plot(sh$H_dbt2/1000000~sh$year,type='l',lwd=2,pch=1,col="blue",xlim=c(1984,2018),ylim=c(min,max),xlab='',ylab='',axes=F)
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$H_dbt2-2*sh$H_dbt2_se)/1000000, rev((sh$H_dbt2+2*sh$H_dbt2_se)/1000000)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
mtext("Effective fishing power beyond C4S (GW)",side=4,col="blue",font=2,line=2.5,cex=1,las=0) 

ticks = seq(1985,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1985,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,3,.1)
axis(side = 4, at = ticks,tck=-0.01,col.ticks = "blue",col = "blue",col.axis="blue",labels = NA)
ticks = seq(0,3,0.5)
axis(side = 4, at = ticks,tck=-0.03,labels = sprintf("%0.1f", ticks),col.ticks = "blue", col = "blue",col.axis="blue")
abline(v=c(1985,2006,2013),lty="dotted",col="grey50")
text(x=1985,y=1.2,labels = "1985:\nDeveloping\ndistant-water\nfisheries",pos=4,col="black",cex=1)
text(x=2006,y=0.3,labels = "2006:\nFuel subsidy",pos=2,col="black",cex=1)
text(x=2013,y=2.2,labels = "2013:\nAccelerating\nfisheries\nupgrading",pos=2,col="black",cex=1)
mtext("a",side=3,adj=0,cex=1,font=2)

# CPUP b)
str(sh)
max<-max(na.omit(sh$CpH_dbt+2*sh$CpH_dbt_se))
max
min<-min(na.omit(sh$CpH_dbt-2*sh$CpH_dbt_se))
min
plot(sh$CpH_dbt~sh$year,type='l',lwd=2,col="black",xlim=c(1985,2018),ylim=c(min,max),ylab="",xlab="",axes=F)
polygon(x=c(sh$year[1:12],rev(sh$year[1:12])),
        y=c(rep(-1,12), rev(rep(70,12))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[12:34],rev(sh$year[12:34])),
        y=c(rep(-1,23), rev(rep(70,23))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CpH_dbt-2*sh$CpH_dbt_se, rev(sh$CpH_dbt+2*sh$CpH_dbt_se)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
box()
mtext("Nominal CPUP beyond C4S (t / kW)",side=2,font=2,col="black",line=2.1,cex=1,las=0) 
ticks = seq(1985,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1985,2018,10)
axis(side = 1, at = ticks)
ticks = seq(0,54,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,54,10)
axis(side = 2, at = ticks, tck=-0.03,labels = ticks)
par(new=T)
max<-max(na.omit(sh$CpH_dbt2+2*sh$CpH_dbt2_se))
max
min<-min(na.omit(sh$CpH_dbt2-2*sh$CpH_dbt2_se))
min
plot(sh$CpH_dbt2~sh$year,type='l',lwd=2,col="blue",xlim=c(1985,2018),ylim=c(min,max),ylab="",xlab="",axes=F)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CpH_dbt2-2*sh$CpH_dbt2_se, rev(sh$CpH_dbt2+2*sh$CpH_dbt2_se)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
mtext("Effective CPUP beyond C4S (t / kW)",side=4,font=2,col="blue",line=2.1,cex=1,las=0) 
ticks = seq(0,54,1)
axis(side = 4, at = ticks,tck=-0.01,col.ticks = "blue",col = "blue",col.axis="blue",labels = NA)
ticks = seq(0,54,10)
axis(side = 4, at = ticks,tck=-0.03,labels = ticks,col.ticks = "blue", col = "blue",col.axis="blue")
mtext("b",side=3,adj=0,cex=1,font=2)
dev.off()

## Figure 3 MTL
setwd("C:/Users/xiong/Desktop/BTF_China")
cp<-read.csv("CN_MTL2.csv",header=TRUE,sep=',')
str(sh)
df<-read.csv("CN_MTL_FIB.csv",header=TRUE,sep=',')
######### plot arima and loess prediction
par(mfrow=c(1,1),mar=c(2,4.5,1,0.1),las=1)
## plot FIB
str(df)
library(fANCOVA)
fit3<-loess.as(df$year,df$FIB,criterion = c("aicc", "gcv")[1],control = loess.control(surface = "direct"))
summary(fit3)
fit3<-loess(df$FIB~df$year,degree=1,span=0.1814,control = loess.control(surface = "direct"))
summary(fit3)
pred<-predict(fit3,df,se=T)
pred
df$FIB_los<-pred$fit
df$FIB_losse<-pred$se.fit
df$FIB_loslw<-pred$fit-2*pred$se.fit
df$FIB_losup<-pred$fit+2*pred$se.fit
# plot
tiff("Figure/Fig.Sx FBI.tif",width = 6.5, height = 4.5, units = 'in', res = 300)
par(mfrow=c(1,1),mar=c(2,4.5,1,0.1),las=1)
max<-max(df$FIB,df$FIB_losup);max
min<-min(df$FIB,df$FIB_loslw);min
str(df)
plot(FIB~year,df,pch=16,type="p",col="black",cex=1,xaxt="n",yaxt="n",ylim=c(min,max),xlab="",ylab="",las=1)
polygon(x=c(cp$year[1:14],rev(cp$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(cp$year[14:29],rev(cp$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(cp$year[29:47],rev(cp$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(cp$year[47:67],rev(cp$year[47:67])),
        y=c(rep(-10,21), rev(rep(24,21))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
lines(df$FIB_los~df$year,lty="solid",col="black")
polygon(x=c(seq(1950,2016,1),rev(seq(1950,2016,1))),
        y=c(df$FIB_loslw, rev(df$FIB_losup)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
xtick<-seq(1950, 2016, by=1)
axis(side = 1, at = xtick, tck=-0.01,labels = NA)
xtick<-seq(1950, 2016, by=5)
axis(side = 1, at = xtick, tck=-0.03,labels = xtick)

ytick<-seq(0,1.87,by=0.1)
axis(side = 2, at = ytick, tck=-0.01,labels = NA)
ytick<-seq(0,1.87,by=0.3)
axis(side = 2, at = ytick, tck=-0.03,labels = round(ytick,digits=2))
mtext("Fishing-in-balance index (FIB) \nin China's claimed EEZ",font=2,side=2,col="black",line=2.5,cex=1,las=0)
abline(v=c(1955,1985,1996,2002),lty="dotted",col="grey50")
text(x=1985,y=.6,labels = "1985:\nDeveloping\ndistant-water\nfisheries",pos=4,col="black",cex=.8)
text(x=2003,y=1.2,labels = "2002:\nVessel buyback",pos=4,col="black",cex=.8)
text(x=1956,y=0.1,labels = "1955:\nNo-trawl zone",pos=4,col="black",cex=.8)
text(x=1996,y=.5,labels = "1996:\nUNCLOS Ratification",pos=4,col="black",cex=.8)
dev.off()

# plot MTL
library(fANCOVA)
# MTL all consumed BTF catch
str(cp)
fit1<-loess.as(cp$year,cp$MTLh,criterion = c("aicc", "gcv")[1],control = loess.control(surface = "direct"))
summary(fit1)
fit1<-loess(cp$MTLh~cp$year,degree=1,span=0.2,control = loess.control(surface = "direct"))
summary(fit1)
pred<-predict(fit1,cp,se=T)
pred
cp$MTLh_los<-pred$fit
cp$MTLh_losse<-pred$se.fit
cp$MTLh_loslw<-pred$fit-2*pred$se.fit
cp$MTLh_losup<-pred$fit+2*pred$se.fit

# MTL all BTF catch
fit2<-loess.as(cp$year,cp$MTL,criterion = c("aicc", "gcv")[1],control = loess.control(surface = "direct"))
summary(fit2)
fit2<-loess(cp$MTL~cp$year,degree=1,span=0.1424,control = loess.control(surface = "direct"))
summary(fit2)
pred<-predict(fit2,cp,se=T)
pred
cp$MTL_los<-pred$fit
cp$MTL_losse<-pred$se.fit
cp$MTL_loslw<-pred$fit-2*pred$se.fit
cp$MTL_losup<-pred$fit+2*pred$se.fit

tiff("Figure/Fig.3 MTL.tif",width = 8, height = 4.5, units = 'in', res = 300)
par(mfrow=c(1,1),mar=c(2,3.5,1,0.1),las=1)
max<-max(cp$MTL_losup);max
min<-min(cp$MTLh);min
plot(cp$MTL~cp$year,pch=16,type="p",col="blue",cex=1,xaxt="n",ylim=c(min,max),xlab="",ylab="",las=1)
polygon(x=c(cp$year[1:14],rev(cp$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(cp$year[14:29],rev(cp$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(cp$year[29:47],rev(cp$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(cp$year[47:67],rev(cp$year[47:67])),
        y=c(rep(-10,21), rev(rep(24,21))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
lines(cp$MTL_los~cp$year,lty="solid",col="blue")
polygon(x=c(seq(1950,2016,1),rev(seq(1950,2016,1))),
        y=c(cp$MTL_loslw, rev(cp$MTL_losup)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
par(new=T)
plot(cp$MTLh~cp$year,pch=17,type="p",col="red",cex=1,xaxt="n",ylim=c(min,max),xlab="",ylab="",las=1)
lines(cp$MTLh_los~cp$year,lty="solid",col="red")
polygon(x=c(seq(1950,2016,1),rev(seq(1950,2016,1))),
        y=c(cp$MTLh_loslw, rev(cp$MTLh_losup)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
abline(v=c(1978,1986,1999,2006,2009),lty="dotted",col="grey50")
text(x=1978,y=3.15,labels = "1978:\nEconomic\nreform",pos=2,col="black",cex=.8)
text(x=1986,y=3.15,labels = "1986:\nFisheries Law",pos=4,col="black",cex=.8)
text(x=1999,y=3.05,labels = "1999:\nSummer\nmoratorium\n(all C4S)",pos=2,col="black",cex=.8)
text(x=2006,y=3.13,labels = "2006:\nCombatting\nillegal fishing",pos=4,col="black",cex=.8)
text(x=2009,y=2.96,labels = "2009:\nExtended\nSummer\nmoratorium",pos=4,col="black",cex=.8)

text(x=1950,y=2.98,labels = "Fishing down / through the food web",pos=4,col="blue",cex=.8)
text(x=1987,y=2.84,labels = "Potential policy impact",pos=4,col="blue",cex=.8)

text(x=2002,y=2.85,labels = "Increasing\nratio of\n'trash' fish",pos=4,col="blue",cex=.8)
text(x=2011,y=2.85,labels = "Reducing\nratio of\n'trash' fish",pos=4,col="blue",cex=.8)

#text(x=1963,y=c(3.68,3.08,2.92),labels =c("3.68","3.08","3.03"),pos=3,col=c("black","blue","red1"))
#text(x=1978,y=c(3.49,3.04,2.82),labels =c("3.49","3.04","2.98"),pos=3,col=c("black","black","red1"))
#text(x=1996,y=c(3.5,2.97,2.80),labels =c("3.5","2.97","2.91"),pos=3,col=c("black","black","red1"))

# Changing x axis
xtick<-seq(1950, 2016, by=1)
axis(side = 1, at = xtick, tck=-0.01,labels = NA)
xtick<-seq(1950, 2016, by=10)
axis(side = 1, at = xtick, tck=-0.03,labels = xtick)

mtext("Mean trophic level (MTL) in China's claimed EEZ",side=2,font=2,col="black",line=2.3,cex=1,las=0) 
legend("bottomleft",legend=c("MTL of BTF catch (all included)",expression("MTL"[h]* " of BTF catch (directly consumed)")),
       cex=1,
       text.col=c("blue","red"),pch=c(16,17),col=c("blue","red"))## Add Legend

dev.off()

### Fig. 4 plot for fishery stock status
## read data
setwd("C:/Users/xiong/Desktop/BTF_China")
sh<-read.csv("CN_bt_fungroup.csv",header=TRUE,sep=',')
str(sh)
tiff("Figure/New/Fig.4 Stock assemblage dominance.tif",width = 8.5, height = 7.8, units = 'in', res = 300)
par(mar=c(2,4,1,1),mfrow=c(2,1),las=1)
## a)
max<-100*max(na.omit(sh$RC_larg),na.omit(sh$RC_ceph),na.omit(sh$RC_shri),na.omit(sh$RC_jell),na.omit(sh$RC_tras))
max
min<-100*min(na.omit(sh$RC_lob),na.omit(sh$RC_ceph),na.omit(sh$RC_shri),na.omit(sh$RC_jell),na.omit(sh$RC_tras))
min
plot(100*sh$RC_larg~sh$year,type='l',col="red1",xlim=c(1940,2020),ylim=c(min,max+5),ylab="",xlab="",lwd=2,axes=F)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-1,14), rev(rep(40,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-1,16), rev(rep(40,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-1,19), rev(rep(40,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:67],rev(sh$year[47:67])),
        y=c(rep(-1,21), rev(rep(40,21))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

lines(100*sh$RC_med~sh$year,type='l',col="blue",lwd=2)
lines(100*sh$RC_tras~sh$year,type='l',col="green3",lwd=1)

lines(100*sh$RC_ceph~sh$year,type='l',col="purple3",lwd=2)
lines(100*sh$RC_shri~sh$year,type='l',col="gold3",lwd=2)
lines(100*sh$RC_jell~sh$year,type='l',col="maroon2",lwd=1)
lines(100*sh$RC_lob~sh$year,type='l',col="black",lwd=2)
box()
mtext("% BTF catch\nwithin China's claimed EEZ",font=2,side=2,col="black",line=2.1,cex=1,las=0) 
text(x=c(1940,1940,1937,1940,1942,1941,2015),y=100*c(0.32,0.18,0.02,0.15,0.224,0.075,0.12),
     labels = c("Large fish","Medium fish","Small fish &\nother invertebrates","Cephalopods","Shrimps","Jellyfish","Crabs &\nlobsters"),pos=4,
     col=c("red1","blue","green3","purple3","gold3","maroon2","black"),cex=1,
     font=c(2,2,1,2,2,1,2))
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
Axis(side=1,at=seq(1950,2020,by=5))
Axis(side=2,at=seq(0,40,by=5))

abline(v=c(1963,1978,1996,2002,2013),lty="dotted",col="grey50")
text(x=1963,y=100*0.34,labels = "1963:\nTechnology\ndevelopment",pos=4,col="black",cex=1)
text(x=1978,y=100*0.35,labels = "1978:\nEconomic reform",pos=4,col="black",cex=1)
text(x=1996,y=100*0.35,labels = "1996:\nUNCLOS\nRatification",pos=4,col="black",cex=1)
text(x=1995,y=100*0.02,labels = "Biomass trawling &\nfishing based on availability",pos=4,col="red1",cex=1)
text(x=2002,y=100*0.26,labels = "2002:\nVessel\nbuyback",pos=4,col="black",cex=1)
text(x=2012,y=100*0.34,labels = "2013:\nAccelerating\nfisheries\nupgrading",pos=4,col="black",cex=1)
text(x=1955,y=3,labels = "Fishing through the food web",pos=4,col="red1",cex=1)
mtext("a",side=3,adj=0,cex=1,font=2)

## ggplot

## b)
sh$X<-NULL
str(sh)
library(reshape2)
df<-melt(sh,id="year")
str(df)
unique(df$variable)
library(ggplot2)
p1<-ggplot(df, aes(x=year, y=value*100, fill=variable)) +
  geom_line(aes(color=variable),size=1.03)+
  scale_x_continuous(breaks=seq(1950,2016,10)
  )+
  scale_color_manual("Functional groups:",
                     labels = c("Large fish","Medium fish","Cephalopods","Shrimps","Crabs & lobsters","Jellyfish","Small fish &\nother invertebrates"),
                     values = c("red1","blue","purple3","gold3","black","maroon2","green3"))+
  labs(x = "", y = "Percentage of catch (%)",face="bold")+
  geom_vline(xintercept = c(1963,1978,1996),colour="black",linetype="dotted")+
  theme(axis.text=element_text(size=10,face = "bold"),
        axis.title.x = element_text(color = "black", size = 12, face = "bold"),
        axis.title.y = element_text(color = "black", size = 12, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.position = "right")  
p1
p2<-ggplot(df,aes(x = year, y = value*100, fill=variable))+
  geom_area(alpha=1 , size=.2, colour="white")+
  scale_x_continuous(breaks=seq(1950,2016,10)
  )+
  scale_fill_manual("Functional groups:",
                    labels = c("Large fish","Medium fish","Cephalopods","Shrimps","Crabs & lobsters","Jellyfish","Small fish &\nother invertebrates"),
                    values = c("red1","blue","purple3","gold3","black","maroon2","green3"))+
  labs(x = "Year", y = "Stacked percentage of catch (%)",face="bold")+
  geom_vline(xintercept = c(1976,1988,2009),colour="black",linetype="dotted",lwd=2)+
  theme(axis.text=element_text(size=10,face = "bold"),
        axis.title.x = element_text(color = "black", size = 12, face = "bold"),
        axis.title.y = element_text(color = "black", size = 12, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.position = "right")  
p2

library(ggpubr)
## For Figure S2.11
tiff("Figure/stacked catch.tif",width = 8, height = 8, units = 'in', res = 300)
ggarrange(p1,p2,ncol=1,nrow=2,common.legend = F,
          labels=c("a", "b")
)
dev.off()

####################
### ## Figure 5. mapping footprint by creating catch by EEZ
setwd("C:/Users/xiong/Desktop/BTF_China")
df1<-read.csv("CN_catch_SAUP.csv",header=TRUE,sep=',') ## Read the cleaned dataset after the correction
df2<-read.csv("CN_catch_SAUP1718.csv",header=TRUE,sep=',')
df<-rbind(df1,df2)
str(df)
#subset data to create catch for only bottom trawl fisheries
str(df)
library(data.table)
eez_df<-df[c(1,3,11,13)]
bt_df<-eez_df[eez_df[,3] %like% "bottom|shrimp",]
summary(bt_df)
str(bt_df)
bt_df[,1]<-gsub('Taiwan','China',bt_df[,1])
bt_df[,1]<-gsub('Hong Kong (China)','China',bt_df[,1])

write.csv(bt_df,"CN_catch_SAUP2.csv")
bt_df<-read.csv("CN_catch_SAUP2.csv",header=TRUE,sep=',')
str(bt_df)
bt_df$X<-NULL
eez<-unique(bt_df[,1]);eez
length(eez)
eez
#### by different eras
eez_1<-subset(bt_df,bt_df[,2]<=1963); summary(eez_1)
eez_2<-subset(bt_df,bt_df[,2]>1963 & bt_df[,2]<=1978); summary(eez_2)
eez_3<-subset(bt_df,bt_df[,2]>1978 & bt_df[,2]<=1996); summary(eez_3)
eez_4<-subset(bt_df,bt_df[,2]>1996); summary(eez_4)
# summarize catch for each EEZ in each era
bt_eez1<-0
bt_eez2<-0
bt_eez3<-0
bt_eez4<-0
unique(eez_1[,1])
for(i in 1:34){
  m<-eez[i]
  bt_eez1[i]<-sum(subset(eez_1[,4],eez_1[,1]==m)) ## 14
}
bt_eez1
df1<-data.frame(EEZ=eez, Catch=bt_eez1)
head(df1);str(df1)
write.csv(df1,"CN_bt_E1.csv")
for(i in 1:34){
  m<-eez[i]
  bt_eez2[i]<-sum(subset(eez_2[,4],eez_2[,1]==m)) ## 15
}
bt_eez2
df2<-data.frame(EEZ=eez, Catch=bt_eez2)
head(df2);str(df2)
write.csv(df2,"CN_bt_E2.csv")
for(i in 1:34){
  m<-eez[i]
  bt_eez3[i]<-sum(subset(eez_3[,4],eez_3[,1]==m)) ## 18
}
bt_eez3
df3<-data.frame(EEZ=eez, Catch=bt_eez3)
head(df3);str(df3)
write.csv(df3,"CN_bt_E3.csv")
for(i in 1:34){
  m<-eez[i]
  bt_eez4[i]<-sum(subset(eez_4[,4],eez_4[,1]==m)) ## 18
}
bt_eez4
df4<-data.frame(EEZ=eez, Catch=bt_eez4)
write.csv(df4,"CN_bt_E4.csv")
head(df4);str(df4)
#### Read the above dataset in each era in ArcMap to cerate the footprint a) to d) in Figure 5
library(reshape2)
# For Figure 5e: summarize catch for each EEZ in each year
cbt<-data.frame(matrix(0,nrow=67,ncol=34,dimnames = list(c(1950:2016),eez)))
cbt
for(i in 1:67){
  df<-subset(bt_df,bt_df$year==i+1949)
  for(j in 1:34){
    m<-eez[j]
    cbt[i,j]<-sum(subset(df$tonnes,df$area_name==m))
  }
}
head(cbt)
str(cbt)
cbt$Year<-c(1950:2016)
df1<-melt(cbt,id="Year")
str(df1)
head(df1)
write.csv(cbt,"Catch_eez_year.csv")
## Fig. 5e: plot the stacked catch 
library(reshape2)
library(ggplot2)
df1<-read.csv("Catch_eez_year20.csv",header=TRUE,sep=',')
df1<-melt(df1,id="Year")
str(df1)
ggplot(df1,aes(x = Year, y = value/1000000, fill=variable))+
  geom_area()+
  labs(x = "Year", y = "Catch (million t)")+
  scale_fill_discrete(name="Fishing Area")+
  geom_vline(xintercept = c(1963,1978,1996),colour="black",linetype="dotted")+
  theme(legend.position = c(0.01,.99),
        legend.justification = c("left", "top"))
## by region
## by region (note you need to stack the catch mannualy to the following regions in the "Catch_eez_year.csv" and rename it to 'Catch_eez_year2.csv' )
df<-read.csv("Catch_eez_year20.csv",header=TRUE,sep=',')
str(df)
df<-df[,1:8]
df<-melt(df,id="Year")
str(df)
df$variable <- factor(df$variable , levels=c("China","East.Asia","West.Africa","Northern.Pacific","Southeast.Asia","Middle.and.South.Asia","Franklin.Islands") )
p1<-ggplot(df,aes(x = Year, y = value/1000000, fill=variable))+
  geom_area(alpha=1 , size=.2, colour="white")+
  labs(x = "", y = "Stacked catch by China's BTF (Mt)",face="bold")+
  scale_x_continuous(breaks=seq(1950,2016,10)
  )+
  scale_fill_manual("Fishing areas:",
                    labels = c("China","East Asia","Africa (Atlantic)","Northern Pacific","Southeast Asia","Middle & South Asia","Franklin Islands (UK)"),
                    values = c("brown","red","pink","yellow","yellow4","green","darkgreen"))+
  geom_vline(xintercept = c(1963,1978,1996),colour="black",linetype="dotted")+
  theme(legend.position = "top",
        axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(color = "black", size = 11, face = "bold"),
        axis.title.y = element_text(color = "black", size = 11, face = "bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11, face = "bold"),
        legend.justification = "top")
p1

library(reshape2)
df<-read.csv("Catch_eez_year20.csv",header=TRUE,sep=',')
str(df)
df2<-subset(df[,c(1,10:16)]);head(df2)
df2<-melt(df2,id="Year")
str(df2)
df2$variable2 <- factor(df2$variable , levels=c("China.1","East.Asia.1","West.Africa.1","Northern.Pacific.1","Southeast.Asia.1","Middle.and.South.Asia.1","Franklin.Islands.1") )
library(ggplot2)
p2<-ggplot(df2,aes(x = Year, y = value*100, fill=variable2))+
  geom_area(alpha=1 , size=.2, colour="white")+
  labs(x = "Year", y = "Stacked percentage of catch (%)")+
  scale_x_continuous(breaks=seq(1950,2014,10))+
  scale_fill_manual(name="Fishing Nations:",
                    values = c("brown","red","pink","yellow","yellow4","green","darkgreen"))+
  geom_vline(xintercept = c(1955,1975,1985,1996),colour="black",linetype="dotted",lwd=2)+
  theme(legend.position = "right",
        axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(color = "black", size = 11, face = "bold"),
        axis.title.y = element_text(color = "black", size = 11, face = "bold"),
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.justification = c("left", "top"))
p2
library(ggpubr)
tiff("Figure/China_BTF.tif",width = 7.8, height = 8, units = 'in', res = 300)
ggarrange(p1,p2,ncol=1,nrow=2,common.legend = T, legend = "top",
          labels=c("a", "b"))
dev.off()

############ Figure 6. Plot Stacked landings by foreign nations in each of the 12 nations that China's distant-water trawlers have significant catch footprint over the history.
### Please download the catch data for each EEZ and corrected the catch by China's fleets before 1985, when these fleets did not yet enter waters beyond China's four seas.
### Save these data as csv files in the same folder, here I used "DW EEZs/new"
## create a list of files including each of the 12 eezs' catch data from SAUP
library(dplyr)
all_precip_files <- list.files("DW EEZs/new", pattern = "*.csv",
                               full.names = TRUE)
all_precip_files
# create an object with the directory name to save your data summarized with below code
the_dir1 <- "DW EEZs/stack"

# create a function to summarize catch for each EEZ in each year
library(reshape2)
library(ggplot2)
summarize_eez<-function(a_csv,the_dir){
  bt_df<-read.csv(a_csv, header = TRUE, sep=',')
  eez<-unique(bt_df$fishing_entity)
  l<-length(eez)
  cbt<-data.frame(matrix(0,nrow=65,ncol=l,dimnames = list(c(1950:2014),eez)))
  for(i in 1:65){
    df<-subset(bt_df,bt_df$year==i+1949)
    for(j in 1:l){
      m<-eez[j]
      cbt[i,j]<-sum(subset(df$tonnes,df$fishing_entity==m))
    }
  }
  head(cbt)
  str(cbt)
  write.csv(cbt, file = paste0(the_dir, "/", basename(a_csv)))
  str(cbt)
  cbt$Year<-c(1950:2014)
  df<-melt(cbt,id="Year")
  str(df)
}
## apply the function to all datasets
lapply(all_precip_files,
       FUN = summarize_eez,
       the_dir = the_dir1)

##### plot the catch
### Before you start the plot, please mannually edit your eez dataset to reduce the number of categories to three: Domestic, China, and Others.
### Build a new folder to save these new datasets, here I use 'DW EEZs/stack/new'.
getwd()
all_precip_files <- list.files("DW EEZs/stack/new", pattern = "*2.csv",
                               full.names = TRUE)
all_precip_files
all_precip_files[1]
df_eez<-read.csv(all_precip_files[1], header = TRUE, sep=',')
df<-melt(df_eez,id="Year")

## multiple plot together: create a list of seperated figure for each eez, and plot them together. 
### IMPORTANT: Check the figure to make sure the color legends are consistent across these figures.
### Here, I used 'darkgreen' for Domestic, 'red2' for China, and 'blue3' for Others
df_eez<-read.csv(all_precip_files[1], header = TRUE, sep=',') ### change the number in 'all_precip_file[1]' to get catch data for each eez
df<-melt(df_eez,id="Year")
## check the plot and make sure you use 'darkgreen' for Domestic, 'red2' for China, and 'blue3' for Others
ggplot(df,aes(x = Year, y = value/1000000, fill=variable))+
  geom_area(alpha=1 , size=.2, colour="white")+
  labs(x = "", y = "")+
  scale_x_continuous(breaks=seq(1950,2014,10)
  )+
  scale_fill_manual("Fishing Entity",
                    values = c("darkgreen","red2","blue3"))+
  theme(legend.position = "right",
        legend.justification = c("left", "top"))  ### geom_vline(xintercept = c(1957),colour="black",linetype="dotted")+
## After the checking, you can save your figure, revise the name accordingly. FOr instance, p1 for all_precip_files[1]
p1<-ggplot(df,aes(x = Year, y = value/1000000, fill=variable))+
  geom_area(alpha=1 , size=.2, colour="white")+
  labs(x = "", y = "")+
  scale_x_continuous(breaks=seq(1950,2014,10)
  )+
  scale_fill_manual("Fishing Entity:",
                    values = c("darkgreen","red2","blue3"))+ ### here you may need to revise the order of these values to make sure they align with the some categories (i.e., Domestic, China, Others) across the eez.
  theme(axis.text=element_text(size=8,face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.position = "top")  ### geom_vline(xintercept = c(1957),colour="black",linetype="dotted")+
## double check if p1 is correct.
p1
### After you create all the 12 nations, you can plot them together:
library(ggpubr)
tiff("DW EEZs/stack/new/stacked catch.tif",width = 13, height = 8, units = 'in', res = 300)
ggarrange(p2,p10,p4,p11,p12,p3,p1,p7,p6,p5,p9,p8,ncol=4,nrow=3,common.legend = T, legend = "top",
          labels=c("a", "b","c","d","e","f","g","h","i","j","k","l")
)
dev.off()

p16

p<-get_legend(p19)
p14
########################################################################################################
## read data
setwd("C:/Users/xiong/Desktop/BTF_China")
bt_df<-read.csv("BT_global20.csv", header = TRUE, sep=',')
eez<-unique(bt_df$fishing_entity)
l<-length(eez);l
cbt<-data.frame(matrix(0,nrow=67,ncol=l,dimnames = list(c(1950:2016),eez)))
for(i in 1:67){
  df<-subset(bt_df,bt_df$year==i+1949)
  for(j in 1:l){
    m<-eez[j]
    cbt[i,j]<-sum(subset(df$tonnes,df$fishing_entity==m))
  }
}
head(cbt)
str(cbt)
str(cbt)
cbt$Year<-c(1950:2016)
write.csv(cbt, "BT_global_by_country20.csv")

## plot
df<-read.csv("BT_top_nations.csv", header = TRUE, sep=',')
df1<-subset(df[,c(1:11)]);head(df1)
library(reshape2)
df1<-melt(df1,id="Year")
str(df1)
df1$variable2<-relevel(df1$variable, 'China')
library(ggplot2)
p1<-ggplot(df1,aes(x = Year, y = value*100, fill=variable2))+
  geom_area(alpha=1 , size=.2, colour="white")+
  labs(x = "Year", y = "Stacked percentage of catch (%)")+
  scale_x_continuous(breaks=seq(1950,2014,10))+
  scale_fill_manual(name="Major fishing powers\nin the history",
                    values = c("red","blue","pink2","yellow","yellow4","green","darkgreen","purple","brown4","grey50"))+
  geom_vline(xintercept = c(1985,1996),colour="black",linetype="dotted",lwd=2)+
  theme(legend.position = "right",
        axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(color = "black", size = 11, face = "bold"),
        axis.title.y = element_text(color = "black", size = 11, face = "bold"),
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.justification ="top")
p1
####
df2<-subset(df[,c(1,13:22)]);head(df2)
library(reshape2)
df2<-melt(df2,id="Year")
str(df2)
df2$variable2<-relevel(df2$variable, 'China.1')
library(ggplot2)
p2<-ggplot(df2,aes(x = Year, y = value/1000000, fill=variable2))+
  geom_area(alpha=1 , size=.2, colour="white")+
  labs(x = "", y = "Stacked catch of gloabl BTF (Mt)")+
  scale_x_continuous(breaks=seq(1950,2014,10))+
  scale_fill_manual(name="Fishing nations:",
                    labels=c("China","Vietnam","Thailand","Indonesia","Russia","Japan","Portugal","USA","Spain","Others"),
                    values = c("red","blue","pink2","yellow","yellow4","green","darkgreen","purple","brown4","grey50"))+
  geom_vline(xintercept = c(1985,1996),colour="black",linetype="dotted",lwd=2)+
  theme(legend.position = "right",
        axis.text=element_text(size=12,face = "bold"),
        axis.title.x = element_text(color = "black", size = 11, face = "bold"),
        axis.title.y = element_text(color = "black", size = 11, face = "bold",margin=margin(r=10,)),
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.justification = c("left", "top"))
p2
library(ggpubr)
tiff("Figure/Global_BTF.tif",width = 7.4, height = 8, units = 'in', res = 300)
ggarrange(p2,p1,ncol=1,nrow=2,common.legend = T, legend = "top",
          labels=c("a", "b"))
dev.off()

### The End ###########


