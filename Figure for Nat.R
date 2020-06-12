############# Figures and maps created in the main text and supplementary information
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
### Plot figures
## read data
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
sh<-read.csv("Catch_new 2020.csv",header=TRUE,sep=',')
str(sh)
####### Fig. 1
tiff("Figure/New/Fig.1 Nature-input and output.tif",width = 9.28, height = 6.8, units = 'in', res = 300)
par(mar=c(2,3.5,1,3.5),mfrow=c(2,2),oma=c(0,0,0,0),las=1)
## a)
max<-max(na.omit(sh$N_bt+2*sh$N_bt_se))/1000
max
min<-min(na.omit(sh$N_bt))/1000
plot(sh$N_bt/1000~sh$year,type='l',lwd=2,pch=1,col="black",xlim=c(1950,2018),ylim=c(min,max),ylab="",xlab="")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-1,14), rev(rep(90,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-1,16), rev(rep(90,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-1,19), rev(rep(90,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:69],rev(sh$year[47:69])),
        y=c(rep(-1,23), rev(rep(90,23))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$N_bt-2*sh$N_bt_se)/1000, rev((sh$N_bt+2*sh$N_bt_se)/1000)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
mtext("No. of trawlers (x1000)",side=2,col="black",line=2.5,cex=1,font=2,las=0) 
ticks = seq(0,80,10)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
par(new=T)
max2<-max(na.omit(sh$H_bt+2*sh$H_bt_se))/1000000
max2
min2<-min(na.omit(sh$H_bt))/1000000
plot(sh$H_bt/1000000~sh$year,type='l',lwd=2,pch=2,col="blue",ylab="",xlab="",xlim=c(1950,2018),ylim=c(min2,max2),xaxt='n',yaxt='n')
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$H_bt-2*sh$H_bt_se)/1000000, rev((sh$H_bt+2*sh$H_bt_se)/1000000)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
mtext("Engine power (GW)",side=4,col="blue",line=1.5,cex=1,font=2,las=0) 
abline(v=c(1978,1997,2002,2006,2015),lty="dotted",col="grey50")
text(x=1978,y=1,labels = "1978:\nEconomic\nreform\n(privitization)",pos=2,col="black",cex=0.8)
text(x=1997,y=3,labels = "1997:\nDouble\nControl",pos=2,col="black",cex=0.8)
text(x=2002,y=7.8,labels = "2002:\nVessel buyback\nFishers transfer",pos=4,col="black",cex=0.8)
text(x=2006,y=5,labels = "2006:\nFuel\nsubsidy",pos=4,col="black",cex=0.8)
text(x=2015,y=1,labels = "2015:\nFuel\nsubsidy\nreduction",pos=2,col="black",cex=0.8)
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,8,1)
axis(side = 4, at = ticks,tck=-0.01,col.ticks = "blue",labels = NA)
ticks = seq(0,8,2)
axis(side = 4, at = ticks,tck=-0.03,labels = ticks,col.ticks = "blue", col = "blue",col.axis="blue")
mtext("a",side=3,adj=0,cex=1,font=2)

## b)
str(sh)
par(mar=c(2,3.5,1,3.5))
max<-max(na.omit(sh$N_dbt+2*sh$N_dbt_se))/1000
max
min<-min(na.omit(sh$N_dbt))/1000
min
plot(sh$N_dbt/1000~sh$year,type='l',lwd=2,pch=1,col="black",xlim=c(1984,2018),ylim=c(min,max),ylab="",xlab="")
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-1,19), rev(rep(1.6,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:69],rev(sh$year[47:69])),
        y=c(rep(-1,23), rev(rep(1.6,23))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$N_dbt-2*sh$N_dbt_se)/1000, rev((sh$N_dbt+2*sh$N_dbt_se)/1000)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
mtext("No. of trawlers beyond C4S (x1000)",side=2,col="black",line=2.5,font=2,cex=1,las=0) 
ticks = seq(0,1.5,.1)
axis(side = 2, at = ticks,tck=-0.01,col.ticks = "black",labels = NA)
par(new=T)
max<-max(na.omit(sh$H_dbt+2*sh$H_dbt_se))/1000000
max
min<-min(na.omit(sh$H_dbt))/1000000
min
plot(sh$H_dbt/1000000~sh$year,type='l',lwd=2,pch=1,col="blue",xlim=c(1984,2018),ylim=c(min,max+0.2),xlab='',ylab='',axes=F)
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$H_dbt-2*sh$H_dbt_se)/1000000, rev((sh$H_dbt+2*sh$H_dbt_se)/1000000)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
mtext("Engine power (GW) beyond C4S",side=4,col="blue",font=2,line=2.5,cex=1,las=0) 

ticks = seq(1985,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,1.4,.1)
axis(side = 4, at = ticks,tck=-0.01,col.ticks = "blue",col = "blue",col.axis="blue",labels = NA)
ticks = seq(0,1.4,0.4)
axis(side = 4, at = ticks,tck=-0.03,labels = ticks,col.ticks = "blue", col = "blue",col.axis="blue")
abline(v=c(1985,2006,2013,2015),lty="dotted",col="grey50")
text(x=1985,y=0.33,labels = "1985:\nDeveloping\ndistant-water\nfisheries",pos=4,col="black",cex=0.8)
text(x=2006,y=0.9,labels = "2006:\nFuel subsidy",pos=2,col="black",cex=0.8)
text(x=2013,y=1.2,labels = "2013:\nAccelerating\nfisheries\nupgrading",pos=2,col="black",cex=0.8)
text(x=2015,y=0.2,labels = "2015:\nFuel\nsubsidy\nreduction",pos=2,col="black",cex=0.8)
mtext("b",side=3,adj=0,cex=1,font=2)

## c)
par(mar=c(2,3.5,1,1))
max<-max(na.omit(sh$HpV_dobt+2*sh$HpV_dobt_se),na.omit(sh$HpV_dbt+2*sh$HpV_dbt_se))/100
max
min<-min(na.omit(sh$HpV_dobt))/100
min
plot(sh$HpV_dobt/100~sh$year,type='l',col="green3",lwd=2,ylim=c(min,max),ylab="",xlab="")
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
        y=c((sh$HpV_dobt-2*sh$HpV_dobt_se)/100, rev((sh$HpV_dobt+2*sh$HpV_dobt_se)/100)),
        col=adjustcolor("green3",alpha.f = 0.2),border = NA)
mtext("Horsepower per vessel (HpV, x100 kW)",font=2,side=2,col="black",line=2.5,cex=1,las=0) 
abline(v=c(1978,1992,2002,2013),lty="dotted",col="black")
text(x=1978,y=6,labels = "1978:\nEconomic reform",pos=2,col="black",cex=.8)
text(x=1992,y=10,labels = "1992:\nAccelerating\neconomic reform",pos=2,col="black",cex=.8)
text(x=2002,y=6,labels = "2002:\nVessel\nbuyback",pos=2,col="black",cex=.8)
text(x=2013,y=15,labels = "2013:\nAccelerating\nfisheries\nupgrading",pos=2,col="black",cex=.8)
lines(sh$HpV_dbt/100~sh$year,type='l',lwd=2,col="red1")
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$HpV_dbt-2*sh$HpV_dbt_se)/100, rev((sh$HpV_dbt+2*sh$HpV_dbt_se)/100)),
        col=adjustcolor("red1",alpha.f = 0.2),border = NA)
mtext("c",side=3,adj=0,cex=1,font=2)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,18,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
legend("topleft",legend=c("BTF within C4S","BTF beyond C4S"),
       text.font=2,lwd=2,
       text.col=c("green3","red1"),lty="solid",col=c("green3","red1"))## Add Legend

## d)
par(mar=c(2,3.5,1,1))
max<-max(na.omit(sh$RHpV_bt_of+2*sh$RHpV_bt_of_se),na.omit(sh$RHpV_dbt_dof+2*sh$RHpV_dbt_dof_se))
max
min<-min(na.omit(sh$RHpV_bt_of-2*sh$RHpV_bt_of_se),na.omit(sh$RHpV_dbt_dof-2*sh$RHpV_dbt_dof_se))
min
plot(sh$RHpV_bt_of~sh$year,type='l',lwd=2,col="green3",ylim=c(min,max),ylab="",xlab="")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-1,14), rev(rep(33,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-1,16), rev(rep(33,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-1,19), rev(rep(33,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:69],rev(sh$year[47:69])),
        y=c(rep(-1,23), rev(rep(33,23))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$RHpV_bt_of-2*sh$RHpV_bt_of_se, rev(sh$RHpV_bt_of+2*sh$RHpV_bt_of_se)),
        col=adjustcolor("green3",alpha.f = 0.2),border = NA)

mtext("HpV of BTF : HpV of other fisheries",side=2,font=2,col="black",line=2.5,cex=1,las=0) 
ticks = seq(0,30,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
abline(v=c(1978,1995,2002),lty="dotted",col="black")
text(x=1978,y=7,labels = "1978:\nEconomic reform",pos=2,col="black",cex=.8)
text(x=2002,y=10,labels = "2002:\nVessel\nbuyback",pos=4,col="black",cex=.8)
text(x=1995,y=28,labels = "1995:\nSummer\nmoratorium",pos=4,col="black",cex=.8)
lines(sh$RHpV_dbt_dof~sh$year,type='l',xlim=c(1950,2020),lwd=2,col="red1",ylab="",xlab="")
polygon(x=c(sh$year[37:69],rev(sh$year[37:69])),
        y=c(sh$RHpV_dbt_dof[37:69]-2*sh$RHpV_dbt_dof_se[37:69], rev(sh$RHpV_dbt_dof[37:69]+2*sh$RHpV_dbt_dof_se[37:69])),
        col=adjustcolor("red1",alpha.f = 0.2),border = NA)

ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,15,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
abline(h=1,col="red1",lty="dashed")
mtext("d",side=3,adj=0,cex=1, font=2)
legend("topleft",legend=c("For vessels within C4S","For vessels beyond C4S"),
       text.font=2,lwd=2,
       text.col=c("green3","red1"),lty="solid",col=c("green3","red1"))## Add Legend

dev.off()



## Fig. 2
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
sh<-read.csv("Catch_new 2020.csv",header=TRUE,sep=',')
str(sh)
tiff("Figure/New/Fig.2 Nature-Mean capacity and fishing efficiency.tif",width = 8.8, height = 6.5, units = 'in', res = 300)
par(mfrow=c(2,2),mar=c(2,3.5,1,1),las=1)
str(sh)
## a)
str(sh)
par(mar=c(2,3.5,1,.2))
max<-max(na.omit(sh$C_bt+2*sh$C_bt_se))/1000000
max
min<-min(na.omit(sh$C_bt_eez))/1000000
min
plot(sh$C_bt/1000000~sh$year,type='l',lwd=2,col="black",ylim=c(min,max),ylab="",xlab="",axes=F)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-1,14), rev(rep(18,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-1,16), rev(rep(18,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-1,19), rev(rep(18,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:69],rev(sh$year[47:69])),
        y=c(rep(-1,23), rev(rep(18,23))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$C_bt-2*sh$C_bt_se)/1000000, rev((sh$C_bt+2*sh$C_bt_se)/1000000)),
        col=adjustcolor("black",alpha.f = 0.1),border = NA)

sh$C_bt_eez_se[65]<-0
lines(sh$C_bt_eez/1000000~sh$year,type='l',lwd=2,col="red")
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$C_bt_eez-2*sh$C_bt_eez_se)/1000000, rev((sh$C_bt_eez+2*sh$C_bt_eez_se)/1000000)),
        col=adjustcolor("red",alpha.f = 0.1),border = NA)
sh$C_bt_dom_se[65]<-0
lines(sh$C_bt_dom/1000000~sh$year,type='l',lwd=2,col="blue")
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$C_bt_dom-2*sh$C_bt_dom_se)/1000000, rev((sh$C_bt_dom+2*sh$C_bt_dom_se)/1000000)),
        col=adjustcolor("blue",alpha.f = 0.1),border = NA)
mtext("Landings (Mt)",side=2,col="black",font=2,line=2,cex=1,las=0) 
box()
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks)
ticks = seq(0,15,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,15,5)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks)
abline(v=c(1978,1985,1996,2013),lty="dotted",col="grey50")
text(x=1996,y=11,labels = "1996:\nUNCLOS\nRatification",pos=2,col="black",cex=0.8)
text(x=1978,y=2.5,labels = "1978:\nEconomic\nreform\n(privitization)",pos=2,col="black",cex=0.8)
text(x=1985,y=7,labels = "1985:\nDeveloping\ndistant-water\nfisheries",pos=2,col="black",cex=0.8)
text(x=2013,y=1,labels = "2013:\nAccelerating\nfisheries\nupgrading",pos=2,col="black",cex=0.8)
mtext("a",side=3,adj=0,cex=1,font=2)
legend("topleft",legend=c("All waters that China fished","China's four seas (C4S)","China's claimed EEZ"),
       text.font=2,lwd=2,
       text.col=c("black","blue","red"),lty="solid",col=c("black","blue","red"))## Add Legend

## b)
str(sh)
plot(sh$RC_bt_all~sh$year,type='l',lwd=2,col="black",ylim=c(0,130),ylab="",xlab="",axes=F)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(140,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(140,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(140,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:69],rev(sh$year[47:69])),
        y=c(rep(-10,23), rev(rep(140,23))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$RC_bt_all-2*sh$RC_bt_all_se, rev(sh$RC_bt_all+2*sh$RC_bt_all_se)),
        col=adjustcolor("black",alpha.f = 0.1),border = NA)
lines(sh$RC_bt_eez~sh$year,type='l',lwd=2,col="red1")
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$RC_bt_eez-2*sh$RC_bt_eez_se, rev(sh$RC_bt_eez+2*sh$RC_bt_eez_se)),
        col=adjustcolor("red1",alpha.f = 0.1),border = NA)
lines(sh$RC_bt_dom~sh$year,type='l',lwd=2,col="blue")
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$RC_bt_dom-2*sh$RC_bt_dom_se, rev(sh$RC_bt_dom+2*sh$RC_bt_dom_se)),
        col=adjustcolor("blue",alpha.f = 0.1),border = NA)
box()
mtext("% landings",side=2,col="black",line=2.1,cex=1,las=0,font=2) 
abline(v=c(1955,1975,1985,1995,2013),lty="dotted",col="grey")
text(x=1955,y=70,labels = "1955:\nNo-trawl\nzone",pos=4,col="black",cex=0.8)
text(x=1957,y=94,labels = "Moving offshore",pos=4,col="red1",cex=0.8)
text(x=1974,y=90,labels = "Moving\ninshore",pos=4,col="red1",cex=0.8)
text(x=1986,y=94,labels = "Moving to distant waters",pos=4,col="red1",cex=0.8)
text(x=1975,y=4,labels = "1975:\nSino-Japanese\nFishery Agreement",pos=2,col="black",cex=0.8)
text(x=1985,y=60,labels = "1985:\nDeveloping\ndistant-water fisheries",pos=2,col="black",cex=0.8)
text(x=1995,y=30,labels = "1995:\nSummer\nmoratorium",pos=2,col="black",cex=0.8)
text(x=2013,y=10,labels = "2013:\nAccelerating\nfisheries\nupgrading",pos=2,col="black",cex=0.8)

abline(h=50,lty="dotted",col="red1")
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks)
ticks = seq(0,100,10)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,100,20)
axis(side = 2, at = ticks)
mtext("b",side=3,adj=0,cex=1,font=2)
legend("topleft",legend=c("% landings from BTF","% BTF landings from C4S","% BTF landings from China's claimed EEZ"),
       text.font=2,lwd=2,
       text.col=c("black","blue","red1"),lty="solid",col=c("black","blue","red1"))## Add Legend
## c)

max<-max(na.omit(sh$CpH_dobt+2*sh$CpH_dobt_se))
max
min<-min(na.omit(sh$CpH_dobt-2*sh$CpH_dobt_se))
min
plot(sh$CpH_dobt~sh$year,type='l',col="black",lwd=2,xlim=c(1950,2018),ylim=c(0,max),ylab="",xlab="",las=1,axes=FALSE)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(8.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(8.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(8.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:69],rev(sh$year[47:69])),
        y=c(rep(-10,23), rev(rep(8.2,23))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CpH_dobt-2*sh$CpH_dobt_se, rev(sh$CpH_dobt+2*sh$CpH_dobt_se)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
mtext("CPUE or VPUE within C4S",side=2,font=2,col="black",line=2.2,cex=1,las=0) 
box()
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks,tck=-0.03,labels = ticks)
ticks = seq(0,8,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,8,2)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks)

abline(v=c(1955,1963,1973,1975,1981,2013),lty="dotted",col="black")
text(x=1955,y=3,labels = "1955:\nNo-trawl\nzone",pos=4,col="black",cex=0.8)
text(x=1973,y=.8,labels = "1973:\nCrude oil\ncrisis",pos=2,col="black",cex=0.8)

text(x=1975,y=4.7,labels = "1975:\nSino-Japanese\nFishery Agreement",pos=4,col="black",cex=0.8)
text(x=1963,y=6.2,labels = "1963:\nTechnology\ndevelopment",pos=4,col="black",cex=0.8)
text(x=1981,y=2,labels = "1981:\nSummer\nmoratorium",pos=4,col="black",cex=0.8)
text(x=2013,y=3,labels = "2013:\nAccelerating\nfisheries\nupgrading",pos=2,col="black",cex=0.8)
par(new=T)
plot(sh$VpH_dobt/10000~sh$year,type='l',lwd=2,col="blue",xlim=c(1950,2018),ylim=c(0,max),ylab="",xlab="",axes=FALSE)
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$VpH_dobt-2*sh$VpH_dobt_se)/10000, rev((sh$VpH_dobt+2*sh$VpH_dobt_se)/10000)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
legend("topright",legend=c("CPUE (t / (kW·year))","VPUE ($1000 / (kW·year))"),
       text.font=2,lwd=2,
       text.col=c("black","blue"),lty="solid",col=c("black","blue"))## Add Legend
mtext("c",side=3,adj=0,cex=1, font=2)

# d)
str(sh)
max<-max(na.omit(sh$CpH_dbt+2*sh$CpH_dbt_se),na.omit(sh$VpH_dbt+2*sh$VpH_dbt_se)/1000)
max
min<-min(na.omit(sh$CpH_dbt-2*sh$CpH_dbt_se),na.omit(sh$VpH_dbt-2*sh$VpH_dbt_se)/1000)
min
plot(sh$CpH_dbt~sh$year,type='l',lwd=2,col="black",xlim=c(1985,2018),ylim=c(min,max),ylab="",xlab="",axes=F)

polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(180,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:69],rev(sh$year[47:69])),
        y=c(rep(-10,23), rev(rep(180,23))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$CpH_dbt-2*sh$CpH_dbt_se, rev(sh$CpH_dbt+2*sh$CpH_dbt_se)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)

lines(sh$VpH_dbt/1000~sh$year,type='l',lwd=2,col="blue")
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$VpH_dbt-2*sh$VpH_dbt_se)/1000, rev((sh$VpH_dbt+2*sh$VpH_dbt_se)/1000)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)

box()
mtext("CPUE or VPUE beyond C4S",side=2,font=2,col="black",line=2.1,cex=1,las=0) 
ticks = seq(1985,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1985,2020,5)
axis(side = 1, at = ticks)
ticks = seq(0,54,10)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,54,20)
axis(side = 2, at = ticks, tck=-0.03,labels = ticks)
mtext("d",side=3,adj=0,cex=1,font=2)
legend("topright",legend=c("CPUE (t / (kW·year))","VPUE ($1000 / (kW·year))"),
       text.font=2,lwd=2,
       text.col=c("black","blue"),lty="solid",col=c("black","blue"))## Add Legend
dev.off()

## Fig. 3
### Plot MTL breakpoints and segmented linear regressions
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
cp<-read.csv("CN_MTL2.csv",header=TRUE,sep=',')
sh<-read.csv("CN_bt_fungroup2 2020.csv",header=TRUE,sep=',')
str(sh)
df<-read.csv("CN_MTL_FIB.csv",header=TRUE,sep=',')
str(df)

tiff("Figure/New/Fig.3 Nature-Fishery health indices.tif",width = 6.8, height = 7.8, units = 'in', res = 300)
library(strucchange)
# define breakpoint functions
breakpoint2_c<-function(cp,i,k,year1,year2){
        n_tr.ts<-cp[c(2,i)]
        n_tr.ts<-na.omit(n_tr.ts)
        n_tr.ts$Date<-as.Date(n_tr.ts$year,format="%Y")
        n_tr.ts$Date<-as.POSIXct(n_tr.ts$Date)
        str(n_tr.ts)
        n_tr_ts<-ts(n_tr.ts[2],start = c(year1,1),end = c(year2,1),frequency=1)
        ntr_ts <- breakpoints(n_tr_ts ~ 1+n_tr.ts$Date,h=3,het.reg=T)# get the breakpoints
        summary(ntr_ts)
        ci_ntr_ts<-confint(ntr_ts)
        ci_ntr_ts
        plot(n_tr_ts/k,xlab="",pch=15,type="p",ylab="",cex=1,ylim=c(2.8,3.85),xaxt="n", yaxt="n")
        ntr_ts
        ci_ntr_ts
} # for plot break points only
breakpoint3_c<-function(cp,i,k,year1,year2){
        n_tr.ts<-cp[c(2,i)]
        n_tr.ts<-na.omit(n_tr.ts)
        n_tr.ts$Date<-as.Date(n_tr.ts$year,format="%Y")
        n_tr.ts$Date<-as.POSIXct(n_tr.ts$Date)
        str(n_tr.ts)
        n_tr_ts<-ts(n_tr.ts[2],start = c(year1,1),end = c(year2,1),frequency=1)
        ntr_ts <- breakpoints(n_tr_ts ~ 1+n_tr.ts$Date,h=3,het.reg=T)# get the breakpoints
        summary(ntr_ts)
        ci_ntr_ts<-confint(ntr_ts)
        ci_ntr_ts
        plot(n_tr_ts/k,xlab="",pch=16,col="blue",type="p",ylab="",cex=1,axes=F,ylim=c(2.8,3.85))
} # for plot break points only
breakpoint4_c<-function(cp,i,k,year1,year2){
        n_tr.ts<-cp[c(2,i)]
        n_tr.ts<-na.omit(n_tr.ts)
        n_tr.ts$Date<-as.Date(n_tr.ts$year,format="%Y")
        n_tr.ts$Date<-as.POSIXct(n_tr.ts$Date)
        str(n_tr.ts)
        n_tr_ts<-ts(n_tr.ts[2],start = c(year1,1),end = c(year2,1),frequency=1)
        ntr_ts <- breakpoints(n_tr_ts ~ 1+n_tr.ts$Date,h=3,het.reg=T)# get the breakpoints
        summary(ntr_ts)
        ci_ntr_ts<-confint(ntr_ts)
        ci_ntr_ts
        plot(n_tr_ts/k,xlab="",pch=17,col="red",type="p",ylab="",cex=1,axes=F,ylim=c(2.8,3.85))
} # for plot break points only
str(cp)
par(mar=c(2,5.2,1.2,0.1),mfrow=c(3,1),las=1)
# MTL of all landings
breakpoint2_c(cp,3,1,1950,2014)
## plot era shade
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(5,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(5,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(5,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(5,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
## build segmented regression models based on breakpoints
lm1<-lm(cp[1:23,3]~cp[1:23,2]);summary(lm1) # r2=0.59, s=-0.004, p<.001
lm2<-lm(cp[24:28,3]~cp[24:28,2]);summary(lm2) # r2=0.96, s=-0.063, p<.01
lm3<-lm(cp[28:30,3]~cp[28:30,2]);summary(lm3) # r2=0.93, s=0.095, p=.17
lm4<-lm(cp[30:39,3]~cp[30:39,2]);summary(lm4) # r2=0.94, s=-0.041, p<.001
lm5<-lm(cp[40:65,3]~cp[40:65,2]);summary(lm5) # r2=0.42, s=0.0027, p<.001
lm7<-lm(cp[39:41,3]~cp[39:41,2]);summary(lm7) # r2=0.86, s=0.065, p =.24

pred_lm1<-as.data.frame(na.omit(predict(lm1,interval = "confidence")))
pred_lm2<-as.data.frame(na.omit(predict(lm2,interval = "confidence")))
pred_lm3<-as.data.frame(na.omit(predict(lm3,interval = "confidence")))
pred_lm4<-as.data.frame(na.omit(predict(lm4,interval = "confidence")))
pred_lm5<-as.data.frame(na.omit(predict(lm5,interval = "confidence")))

## plot fit
lines(x=seq(1950,1972,1),fitted(lm1),col="black",lty="solid",lwd=1)
polygon(x=c(seq(1950,1972,1),rev(seq(1950,1972,1))),
        y=c(pred_lm1$lwr, rev(pred_lm1$upr)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
lines(x=seq(1973,1977,1),fitted(lm2),col="black",lty="solid",lwd=1)
polygon(x=c(seq(1973,1977,1),rev(seq(1973,1977,1))),
        y=c(pred_lm2$lwr, rev(pred_lm2$upr)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
lines(x=seq(1977,1979,1),fitted(lm3),col="black",lty="solid",lwd=1)
lines(x=seq(1979,1988,1),fitted(lm4),col="black",lty="solid",lwd=1)
polygon(x=c(seq(1979,1988,1),rev(seq(1979,1988,1))),
        y=c(pred_lm4$lwr, rev(pred_lm4$upr)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)
lines(x=seq(1988,1990,1),fitted(lm7),col="black",lty="solid",lwd=1)
lines(x=seq(1989,2014,1),fitted(lm5),col="black",lty="solid",lwd=1)
polygon(x=c(seq(1989,2014,1),rev(seq(1989,2014,1))),
        y=c(pred_lm5$lwr, rev(pred_lm5$upr)),
        col=adjustcolor("black",alpha.f = 0.2),border = NA)


### MTL of BTF landings
par(new=T)
breakpoint3_c(cp,4,1,1950,2014)
lm1<-lm(cp[1:22,4]~cp[1:22,2]);summary(lm1) # r2=0.90, s=-0.004, p<.001
lm2<-lm(cp[23:28,4]~cp[23:28,2]);summary(lm2) # r2=0.95, s=-0.024, p<.01
lm3<-lm(cp[28:32,4]~cp[28:32,2]);summary(lm3) # r2=0.86, s=0.021, p<.05
lm4<-lm(cp[32:39,4]~cp[32:39,2]);summary(lm4) # r2=0.95, s=-0.036, p<.001
lm5<-lm(cp[40:53,4]~cp[40:53,2]);summary(lm5) # r2=0.004, s=0.000, p =.84
lm6<-lm(cp[54:65,4]~cp[54:65,2]);summary(lm6) # r2=0.47, s=0.0034, p <0.05
lm7<-lm(cp[39:41,4]~cp[39:41,2]);summary(lm7) # r2=0.95, s=0.052, p =.14
lm8<-lm(cp[53:55,4]~cp[53:55,2]);summary(lm8) # r2=0.90, s=0.032, p =.20

pred_lm1<-as.data.frame(na.omit(predict(lm1,interval = "confidence")))
pred_lm2<-as.data.frame(na.omit(predict(lm2,interval = "confidence")))
pred_lm3<-as.data.frame(na.omit(predict(lm3,interval = "confidence")))
pred_lm4<-as.data.frame(na.omit(predict(lm4,interval = "confidence")))
pred_lm5<-as.data.frame(na.omit(predict(lm5,interval = "confidence")))
pred_lm6<-as.data.frame(na.omit(predict(lm6,interval = "confidence")))

## plot fit
lines(x=seq(1950,1971,1),fitted(lm1),col="blue",lty="solid",lwd=1)
polygon(x=c(seq(1950,1971,1),rev(seq(1950,1971,1))),
        y=c(pred_lm1$lwr, rev(pred_lm1$upr)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
lines(x=seq(1972,1977,1),fitted(lm2),col="blue",lty="solid",lwd=1)
polygon(x=c(seq(1972,1977,1),rev(seq(1972,1977,1))),
        y=c(pred_lm2$lwr, rev(pred_lm2$upr)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
lines(x=seq(1977,1981,1),fitted(lm3),col="blue",lty="solid",lwd=1)
polygon(x=c(seq(1977,1981,1),rev(seq(1977,1981,1))),
        y=c(pred_lm3$lwr, rev(pred_lm3$upr)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
lines(x=seq(1981,1988,1),fitted(lm4),col="blue",lty="solid",lwd=1)
polygon(x=c(seq(1981,1988,1),rev(seq(1981,1988,1))),
        y=c(pred_lm4$lwr, rev(pred_lm4$upr)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
lines(x=seq(1988,1990,1),fitted(lm7),col="blue",lty="solid",lwd=1)
lines(x=seq(2002,2004,1),fitted(lm8),col="blue",lty="solid",lwd=1)
lines(x=seq(1989,2002,1),fitted(lm5),col="blue",lty="solid",lwd=1)
polygon(x=c(seq(1989,2002,1),rev(seq(1989,2002,1))),
        y=c(pred_lm5$lwr, rev(pred_lm5$upr)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
lines(x=seq(2003,2014,1),fitted(lm6),col="blue",lty="solid",lwd=1)
polygon(x=c(seq(2003,2014,1),rev(seq(2003,2014,1))),
        y=c(pred_lm6$lwr, rev(pred_lm6$upr)),
        col=adjustcolor("blue",alpha.f = 0.2),border = NA)
### MTL of BTF landings consumed directly
par(new=T)
breakpoint4_c(cp,5,1,1950,2014)
lm1<-lm(cp[1:22,5]~cp[1:22,2]);summary(lm1) # r2=0.73, s=-0.0054, p<.001
lm2<-lm(cp[23:27,5]~cp[23:27,2]);summary(lm2) # r2=0.98, s=-0.028, p<.01
lm3<-lm(cp[27:32,5]~cp[27:32,2]);summary(lm3) # r2=0.42, s=0.009, p=.16
lm4<-lm(cp[32:39,5]~cp[32:39,2]);summary(lm4) # r2=0.87, s=-0.022, p<.001
lm5<-lm(cp[41:56,5]~cp[41:56,2]);summary(lm5) # r2=0.41, s=-0.0024, p <0.01
lm6<-lm(cp[56:65,5]~cp[56:65,2]);summary(lm6) # r2=0.80, s=0.0057, p <0.001
lm7<-lm(cp[39:41,5]~cp[39:41,2]);summary(lm7) # r2=0.86, s=0.042, p =.25


pred_lm1<-as.data.frame(na.omit(predict(lm1,interval = "confidence")))
pred_lm2<-as.data.frame(na.omit(predict(lm2,interval = "confidence")))
pred_lm3<-as.data.frame(na.omit(predict(lm3,interval = "confidence")))
pred_lm4<-as.data.frame(na.omit(predict(lm4,interval = "confidence")))
pred_lm5<-as.data.frame(na.omit(predict(lm5,interval = "confidence")))
pred_lm6<-as.data.frame(na.omit(predict(lm6,interval = "confidence")))

## plot fit
lines(x=seq(1950,1971,1),fitted(lm1),col="red",lty="solid",lwd=1)
polygon(x=c(seq(1950,1971,1),rev(seq(1950,1971,1))),
        y=c(pred_lm1$lwr, rev(pred_lm1$upr)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
lines(x=seq(1972,1976,1),fitted(lm2),col="red",lty="solid",lwd=1)
polygon(x=c(seq(1972,1976,1),rev(seq(1972,1976,1))),
        y=c(pred_lm2$lwr, rev(pred_lm2$upr)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
lines(x=seq(1976,1981,1),fitted(lm3),col="red",lty="solid",lwd=1)
polygon(x=c(seq(1976,1981,1),rev(seq(1976,1981,1))),
        y=c(pred_lm3$lwr, rev(pred_lm3$upr)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
lines(x=seq(1981,1988,1),fitted(lm4),col="red",lty="solid",lwd=1)
polygon(x=c(seq(1981,1988,1),rev(seq(1981,1988,1))),
        y=c(pred_lm4$lwr, rev(pred_lm4$upr)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
lines(x=seq(1988,1990,1),fitted(lm7),col="red",lty="solid",lwd=1)
lines(x=seq(1990,2005,1),fitted(lm5),col="red",lty="solid",lwd=1)
polygon(x=c(seq(1990,2005,1),rev(seq(1990,2005,1))),
        y=c(pred_lm5$lwr, rev(pred_lm5$upr)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)
lines(x=seq(2005,2014,1),fitted(lm6),col="red",lty="solid",lwd=1)
polygon(x=c(seq(2005,2014,1),rev(seq(2005,2014,1))),
        y=c(pred_lm6$lwr, rev(pred_lm6$upr)),
        col=adjustcolor("red",alpha.f = 0.2),border = NA)

abline(v=c(1978,1987,1996,2002,2006),lty="dotted",col="black")
text(x=1978,y=3.75,labels = "1978:\nEconomic\nreform",pos=2,col="black",cex=1)
text(x=1987,y=3.75,labels = "1987:\nSummer\nmoratorium",pos=2,col="black",cex=1)

text(x=1996,y=3.2,labels = "1996:\nUNCLOS\nRatification",pos=2,col="black",cex=1)
text(x=2002,y=3.3,labels = "2002:\nVessel\nbuyback",pos=2,col="black",cex=1)
text(x=2006,y=3.22,labels = "2006:\nExtended\nSummer\nmoratorium\n& combatting\nillegal fishing",pos=4,col="black",cex=1)

text(x=1950,y=2.85,labels = "Fishing down/through the food web",pos=4,col="red1",cex=1)
text(x=1990,y=2.8,labels = "Intensified biomass trawling",pos=4,col="red1",cex=1)
text(x=1988,y=c(3.20,2.89,2.74),labels = c("3.34","2.85","2.82"),pos=3,col=c("black","blue","red1"),cex=1)

text(x=1950,y=c(3.72,3.17,3.01),labels =c("3.67","3.17","3.12"),pos=3,col=c("black","blue","red1"))

#text(x=1963,y=c(3.68,3.08,2.92),labels =c("3.68","3.08","3.03"),pos=3,col=c("black","blue","red1"))
#text(x=1978,y=c(3.49,3.04,2.82),labels =c("3.49","3.04","2.98"),pos=3,col=c("black","black","red1"))
#text(x=1996,y=c(3.5,2.97,2.80),labels =c("3.5","2.97","2.91"),pos=3,col=c("black","black","red1"))

text(x=2015,y=c(3.54,2.97,2.81),labels =c("3.54","3.06","2.92"),pos=3,col=c("black","blue","red1"),cex=1)
# Changing x axis
xtick<-seq(1950, 2015, by=1)
axis(side = 1, at = xtick, tck=-0.01,labels = NA)
xtick<-seq(1950, 2015, by=5)
axis(side = 1, at = xtick, tck=-0.03,labels = xtick)

# Changing y axis
ytick<-seq(2.8, 3.8, by=0.1)
labely<-seq(2.8,3.8,by=0.2)
axis(side = 2, at = ytick, tck=-0.01,labels = NA)
axis(side = 2, at = labely, tck=-0.03,labels =round(labely,digits=2))
mtext("")
mtext("MTL\nin China's claimed EEZ",side=2,font=2,col="black",line=2.5,cex=1,las=0) 
legend("topright",legend=c("MTL of all landings (estimated by SAUP)","MTL of BTF landings (this study)",expression("MTL"[h]* " of BTF landings (this study)")),
       cex=1,
       text.col=c("black","blue","red"),pch=c(15,16,17),col=c("black","blue","red"))## Add Legend
mtext("a",side=3,adj=0,cex=1, font=2)

## plot b)
par(mar=c(2,5.2,1.2,0.1))
str(df)
max<-max(df$df_LRPI,df$df_LRPI_f);max
min<-min(df$df_LRPI,df$df_LRPI_f);min
plot(df_LRPI~year,df,type='l',xlim=c(1950,2015),ylim=c(min,max),xlab="",ylab="",las=1,xaxt="n", yaxt="n")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(4,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(4,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(4,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(4,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
lines(df_LRPI_f~year,df,type='l',col="blue",lwd=2)
abline(h=0,lty="dashed",col="red1")
xtick<-seq(1950, 2015, by=1)
axis(side = 1, at = xtick, tck=-0.01,labels = NA)
xtick<-seq(1950, 2015, by=5)
axis(side = 1, at = xtick, tck=-0.03,labels = xtick)

ytick<-seq(-0.35,0.25,by=0.05)
axis(side = 2, at = ytick, tck=-0.01,labels = NA)
ytick<-seq(-0.3,0.2,by=0.1)
axis(side = 2, at = ytick, tck=-0.03,labels = round(ytick,digits=2))

mtext("LRPI of BTF\nin China's claimed EEZ",side=2,font=2,col="black",line=2.5,cex=1,las=0)
abline(v=c(1978),lty="dotted",col="black")
text(x=1978,y=0.2,labels = "1978:\nEconomic reform",pos=2,col="black",cex=1)
text(x=1952,y=-0.33,labels = "Biased by the estimated high prices of shrimps",pos=4,col="red1",cex=1)
text(x=1994,y=-0.3,labels = "Biomass trawling",pos=4,col="red1",cex=1)
legend("topright",legend=c("Fish species","All species"),
       cex=1,text.font=c(2,1),lwd=c(2,1),
       text.col=c("blue","black"),lty=c("solid","solid"),col=c("blue","black"))## Add Legend
mtext("b",side=3,adj=0,cex=1, font=2)

## plot c)
str(df)
max<-max(df$FIB,df$fib_index);max
min<-min(df$FIB,df$fib_index);min
plot(FIB~year,df,type='o',lty="solid",pch=1,xlim=c(1950,2015),ylim=c(min,max),xlab="",ylab="",las=1,xaxt="n", yaxt="n")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(4,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(4,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(4,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(4,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

lines(fib_index~year,df,type='o',pch=17,col="black")

xtick<-seq(1950, 2015, by=1)
axis(side = 1, at = xtick, tck=-0.01,labels = NA)
xtick<-seq(1950, 2015, by=5)
axis(side = 1, at = xtick, tck=-0.03,labels = xtick)

ytick<-seq(0,2.3,by=0.1)
axis(side = 2, at = ytick, tck=-0.01,labels = NA)
ytick<-seq(0,2.3,by=0.5)
axis(side = 2, at = ytick, tck=-0.03,labels = round(ytick,digits=2))
mtext("FIBI\nin China's claimed EEZ",font=2,side=2,col="black",line=2.5,cex=1,las=0)
abline(v=c(1955,1985,1978,1996,2002),lty="dotted",col="black")
text(x=1978,y=.5,labels = "1978:\nEconomic reform\n(vessel privatization)",pos=2,col="black",cex=1)
text(x=1985,y=.6,labels = "1985:\nDeveloping\ndistant-water\nfisheries",pos=4,col="black",cex=1)
text(x=2003,y=1.2,labels = "2002:\nVessel buyback",pos=4,col="black",cex=1)
text(x=1956,y=0.1,labels = "1955:\nNo-trawl zone",pos=4,col="black",cex=1)
text(x=1996,y=.5,labels = "1996:\nUNCLOS Ratification",pos=4,col="black",cex=1)
text(x=1958,y=1.5,labels = "Fishing offshore",pos=4,col="red1",cex=1)
text(x=1981,y=1.9,labels = "Expansion sped up",pos=4,col="red1",cex=1)
text(x=1996,y=1.7,labels = "Expansion slowed down",pos=4,col="red1",cex=1)
legend("topleft",legend=c("All fisheries (estimated by SAUP)","BTF (this study)"),
       cex = 1,
       text.col=c("black","black"),lty=c("solid","solid"),pch=c(17,1),col=c("black","black"))## Add Legend
mtext("c",side=3,adj=0,cex=1, font=2)

dev.off()
### Fig. 4 plot for fishery stock status
## read data
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
sh<-read.csv("CN_bt_fungroup2 2020.csv",header=TRUE,sep=',')
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
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-1,19), rev(rep(40,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

lines(100*sh$RC_med~sh$year,type='l',col="blue",lwd=2)
lines(100*sh$RC_small~sh$year,type='l',col="green3",lwd=1)

lines(100*sh$RC_ceph~sh$year,type='l',col="purple3",lwd=2)
lines(100*sh$RC_shri~sh$year,type='l',col="gold3",lwd=2)
lines(100*sh$RC_jell~sh$year,type='l',col="maroon2",lwd=1)
lines(100*sh$RC_lob~sh$year,type='l',col="black",lwd=1)
box()
mtext("% landings by China's BTF\nin its claimed EEZ",font=2,side=2,col="black",line=2.1,cex=1,las=0) 
text(x=c(1940,1940,1937,1940,1942,1941,2014),y=100*c(0.32,0.18,0.02,0.15,0.224,0.075,0.12),
     labels = c("Large fish","Medium fish","Small fish &\nother invertebrates","Cephalopods","Shrimps","Jellyfish","Crabs &\nlobsters"),pos=4,
     col=c("red1","blue","green3","purple3","gold3","maroon2","black"),cex=1,
     font=c(2,2,1,2,2,1,1))
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
Axis(side=1,at=seq(1950,2020,by=5))
Axis(side=2,at=seq(0,40,by=5))

abline(v=c(1963,1978,1996,2002),lty="dotted",col=c("black","black","black","black","black","black"))
text(x=1963,y=100*0.34,labels = "1963:\nTechnology\ndevelopment",pos=4,col="black",cex=0.8)
text(x=1978,y=100*0.35,labels = "1978:\nEconomic reform",pos=4,col="black",cex=0.8)
text(x=1996,y=100*0.35,labels = "1996:\nUNCLOS Ratification",pos=4,col="black",cex=0.8)
text(x=1995,y=100*0.02,labels = "Biomass trawling &\nfishing based on availability",pos=4,col="red1",cex=0.8)
text(x=2002,y=100*0.26,labels = "2002:\nVessel\nbuyback",pos=4,col="black",cex=0.8)
text(x=1955,y=3,labels = "Fishing through the food web",pos=4,col="red1",cex=0.8)
mtext("a",side=3,adj=0,cex=1,font=2)

## b)
max<-100*max(na.omit(sh$RV_larg),na.omit(sh$RV_ceph),na.omit(sh$RV_shri),na.omit(sh$RV_jell),na.omit(sh$RV_tras))
max
min<-100*min(na.omit(sh$RV_lob),na.omit(sh$RV_ceph),na.omit(sh$RV_shri),na.omit(sh$RV_jell),na.omit(sh$RV_tras))
min
plot(100*sh$RV_larg~sh$year,type='l',col="red1",xlim=c(1940,2020),ylim=c(min,max+10),ylab="",xlab="",lwd=2,axes=F)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(92,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(92,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(92,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(92,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
lines(100*sh$RV_med~sh$year,type='l',col="blue",lwd=2)
lines(100*sh$RV_small~sh$year,type='l',col="green3",lwd=1)

lines(100*sh$RV_ceph~sh$year,type='l',col="purple3",lwd=2)
lines(100*sh$RV_shri~sh$year,type='l',col="gold3",lwd=2)
lines(100*sh$RV_jell~sh$year,type='l',col="maroon2",lwd=1)
lines(100*sh$RV_lob~sh$year,type='l',col="black",lwd=1)
box()
mtext("% landed value by China's BTF\nin its claimed EEZ",font=2,side=2,col="black",line=2.1,cex=1,las=0) 
## text(x=1948,y=c(0.25,0.17,0.45,0.11,0.07,0.01),labels = c("large fish","medium fish","shrimps,lobsters & crabs","cephalopods","jellyfish","trash fish"),pos=4,col=c("black","black51","black","blue","yellow3","green2"),cex=1)
text(x=c(1939,1940,1937,1937,1940,2014,2014),y=100*c(0.2,0.14,0.03,0.1,0.5,0.45,0.11),
     labels = c("Large fish","Medium fish","Small fish &\nother invertebrates","Cephalopods","Shrimps","Jellyfish","Crabs &\nlobsters"),pos=4,
     col=c("red1","blue","green3","purple3","gold3","maroon2","black"),
     cex=c(1,1,0.8,1,1,1,1),
     font=c(2,2,1,2,2,1,1))
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
Axis(side=1,at=seq(1950,2020,by=5))
Axis(side=2,at=seq(0,80,by=10))
abline(v=c(1963,1978,1996,2002),lty="dotted",col=c("black","black","black","black","black","black"))
text(x=1963,y=100*0.6,labels = "1963:\nTechnology\ndevelopment",pos=4,col="black",cex=0.8)
text(x=1978,y=100*0.82,labels = "1978:\nEconomic reform",pos=4,col="black",cex=0.8)
text(x=1996,y=100*0.8,labels = "1996:\nUNCLOS Ratification",pos=4,col="black",cex=0.8)
text(x=2002,y=100*0.56,labels = "2002:\nVessel\nbuyback",pos=4,col="black",cex=0.8)
mtext("b",side=3,adj=0,cex=1, font=2)
dev.off()

####################
### ## Figure 5. mapping footprint by creating catch by EEZ
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
df<-read.csv("CN_catch_SAUP.csv",header=TRUE,sep=',') ## Read the cleaned dataset after the above correction
#subset data to create catch for only bottom trawl fisheries
str(df)
library(data.table)
eez_df<-df[c(1,3,11,13)]
bt_df<-eez_df[eez_df[,3] %like% "bottom|shrimp",]
summary(bt_df)
str(bt_df)
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
for(i in 1:36){
        m<-eez[i]
        bt_eez1[i]<-sum(subset(eez_1[,4],eez_1[,1]==m)) ## 14
}
bt_eez1
df1<-data.frame(EEZ=eez, Catch=bt_eez1)
head(df1);str(df1)
write.csv(df1,"CN_bt_E1.csv")
for(i in 1:36){
        m<-eez[i]
        bt_eez2[i]<-sum(subset(eez_2[,4],eez_2[,1]==m)) ## 15
}
bt_eez2
df2<-data.frame(EEZ=eez, Catch=bt_eez2)
head(df2);str(df2)
write.csv(df2,"CN_bt_E2.csv")
for(i in 1:36){
        m<-eez[i]
        bt_eez3[i]<-sum(subset(eez_3[,4],eez_3[,1]==m)) ## 18
}
bt_eez3
df3<-data.frame(EEZ=eez, Catch=bt_eez3)
head(df3);str(df3)
write.csv(df3,"CN_bt_E3.csv")
for(i in 1:36){
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
cbt<-data.frame(matrix(0,nrow=65,ncol=36,dimnames = list(c(1950:2014),eez)))
cbt
for(i in 1:65){
        df<-subset(bt_df,bt_df$year==i+1949)
        for(j in 1:36){
                m<-eez[j]
                cbt[i,j]<-sum(subset(df$tonnes,df$area_name==m))
        }
}
head(cbt)
str(cbt)
cbt$Year<-c(1950:2014)
df1<-melt(cbt,id="Year")
str(df1)
head(df1)
write.csv(cbt,"Catch_eez_year.csv")
## Fig.5e: plot the stacked catch 
library(reshape2)
library(ggplot2)
ggplot(df1,aes(x = Year, y = value/1000000, fill=variable))+
        geom_area()+
        labs(x = "Year", y = "Catch (million t)")+
        scale_fill_discrete(name="Fishing Area")+
        geom_vline(xintercept = c(1963,1978,1996),colour="black",linetype="dotted")+
        theme(legend.position = c(0.01,.99),
              legend.justification = c("left", "top"))
## by region
## by region (note you need to stack the catch mannualy to the following regions in the "Catch_eez_year.csv" and rename it to 'Catch_eez_year2.csv' )
df<-read.csv("Catch_eez_year2.csv",header=TRUE,sep=',')
str(df)
df<-melt(df,id="Year")
str(df)
df$variable <- factor(df$variable , levels=c("China","East.Asia","Northern.Pacific","West.Africa","Southeast.Asia","Middle.and.South.Asia","Franklin.Islands") )
tiff('test.tif', units="in", width=8.4, height=3.5, res=300, compression = 'lzw')
ggplot(df,aes(x = Year, y = value/1000000, fill=variable))+
        geom_area(alpha=1 , size=.2, colour="white")+
        labs(x = "Year", y = "Stacked landings by China's BTF (Mt)",face="bold")+
        scale_x_continuous(breaks=seq(1950,2014,10)
        )+
        scale_fill_manual("Fishing Area",
                          labels = c("China","East Asia","Northern Pacific","Africa (Atlantic)","Southeast Asia","Middle & South Asia","Franklin Islands (UK)"),
                          values = c("brown","red","pink","yellow","yellow4","green","darkgreen"))+
        geom_vline(xintercept = c(1963,1978,1996),colour="black",linetype="dotted")+
        theme(legend.position = c(0.01,.99),
              axis.text=element_text(size=12,face = "bold"),
              axis.title.x = element_text(color = "black", size = 12, face = "bold"),
              axis.title.y = element_text(color = "black", size = 12, face = "bold"),
              legend.title = element_text(size = 14, face = "bold"),
              legend.text = element_text(size = 12, face = "bold"),
              legend.justification = c("left", "top"))

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
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
bt_df<-read.csv("BT_global.csv", header = TRUE, sep=',')
eez<-unique(bt_df$fishing_entity)
l<-length(eez);l
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
str(cbt)
cbt$Year<-c(1950:2014)
write.csv(cbt, "BT_global_by_country.csv")

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
        labs(x = "", y = "% landings of global BTF")+
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
        labs(x = "Year", y = "Stacked landings of gloabl BTF (Mt)")+
        scale_x_continuous(breaks=seq(1950,2014,10))+
        scale_fill_manual(name="Fishing Nations:",
                          values = c("red","blue","pink2","yellow","yellow4","green","darkgreen","purple","brown4","grey50"))+
        geom_vline(xintercept = c(1985,1996),colour="black",linetype="dotted",lwd=2)+
        theme(legend.position = "right",
              axis.text=element_text(size=12,face = "bold"),
              axis.title.x = element_text(color = "black", size = 11, face = "bold"),
              axis.title.y = element_text(color = "black", size = 11, face = "bold"),
              legend.title = element_text(size = 13, face = "bold"),
              legend.text = element_text(size = 12, face = "bold"),
              legend.justification = c("left", "top"))
p2
library(ggpubr)
tiff("DW EEZs/Global_BTF.tif",width = 7.4, height = 8, units = 'in', res = 300)
ggarrange(p1,p2,ncol=1,nrow=2,common.legend = T, legend = "top",
          labels=c("a)", "b)"))
dev.off()

### The End ###########
