###### The code here was use to derive the supplementary figures for the "A 'slash-and-burn' fishing dragon at sea: China's bottom trawl fisheries and their global impact"
### read data
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
sh<-read.csv("Catch_new 2020.csv",header=TRUE,sep=',')
str(sh)
## For figure S2.1
## a)
par(mfrow=c(2,1))
max<-max(na.omit(sh$V_bt+2*sh$V_bt_se))/1000000000
max
min<-min(na.omit(sh$V_bt_eez))/1000000000
min
sh$V_bt_se[65]<-0
plot(sh$V_bt/1000000000~sh$year,type='l',col="black",ylim=c(min,max),ylab="",xlab="",axes=F)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-1,14), rev(rep(31,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-1,16), rev(rep(31,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-1,19), rev(rep(31,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:69],rev(sh$year[47:69])),
        y=c(rep(-1,23), rev(rep(31,23))),
        col=adjustcolor("cyan",alpha.f = 0.4),border = NA)

polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$V_bt-2*sh$V_bt_se)/1000000000, rev((sh$V_bt+2*sh$V_bt_se)/1000000000)),
        col=adjustcolor("black",alpha.f = 0.1),border = NA)
sh$V_bt_eez_se[65]<-0
lines(sh$V_bt_eez/1000000000~sh$year,type='l',col="blue")
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$V_bt_eez-2*sh$V_bt_eez_se)/1000000000, rev((sh$V_bt_eez+2*sh$V_bt_eez_se)/1000000000)),
        col=adjustcolor("blue",alpha.f = 0.1),border = NA)
sh$V_bt_dom_se[65]<-0
lines(sh$V_bt_dom/1000000000~sh$year,type='l',col="red")
polygon(x=c(sh$year,rev(sh$year)),
        y=c((sh$V_bt_dom-2*sh$V_bt_dom_se)/1000000000, rev((sh$V_bt_dom+2*sh$V_bt_dom_se)/1000000000)),
        col=adjustcolor("red",alpha.f = 0.1),border = NA)
box()
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks)
ticks = seq(0,30,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,30,5)
axis(side = 2, at = ticks,tck=-0.03,labels = ticks)
mtext("Total landed value by China's BTF (billion US$)",side=2,col="black",line=2.5,cex=0.8,las=0) 
abline(v=c(1975,1996,2003,2009,2013),lty="dotted",col="grey50")
abline(v=c(1996,2003,2009,2013),lty="dotted",col="grey50")
text(x=1975,y=9,labels = "1975:\nSino-Japanese\nFishery Agreement",pos=2,col="grey50",cex=0.8)
text(x=1996,y=22,labels = "1996:\nUNCLOS\nRatification",pos=2,col="grey50",cex=0.8)
text(x=2003,y=25,labels = "2003:\nEnhanced\nsummer\nmoratorium",pos=4,col="grey50",cex=0.8)
text(x=2009,y=15,labels = "2009:\nExtended\nsummer\nmoratorium",pos=2,col="grey50",cex=0.8)
text(x=2013,y=2,labels = "2013:\nAccelerating\nfisheries\nupgrading",pos=2,col="grey50",cex=0.8)
mtext("a)",side=3,adj=0,cex=0.8)
legend("topleft",legend=c("All areas","China's four seas (C4S)","China's claimed EEZ"),
       text.col=c("black","red","blue"),lty="solid",col=c("black","red","blue"))## Add Legend
## b)

plot(sh$RV_bt_eez~sh$year,type='l',col="red1",ylim=c(0,140),ylab="",xlab="",axes=F)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(146,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(146,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(146,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:69],rev(sh$year[47:69])),
        y=c(rep(-10,23), rev(rep(146,23))),
        col=adjustcolor("cyan",alpha.f = 0.4),border = NA)
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$RV_bt_eez-2*sh$RV_bt_eez_se, rev(sh$RV_bt_eez+2*sh$RV_bt_eez_se)),
        col=adjustcolor("red",alpha.f = 0.1),border = NA)

lines(sh$RV_bt_all~sh$year,type='l',col="black")
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$RV_bt_all-2*sh$RV_bt_all_se, rev(sh$RV_bt_all+2*sh$RV_bt_all_se)),
        col=adjustcolor("black",alpha.f = 0.1),border = NA)

lines(sh$RV_bt_dom~sh$year,type='l',col="blue")
polygon(x=c(sh$year,rev(sh$year)),
        y=c(sh$RV_bt_dom-2*sh$RV_bt_dom_se, rev(sh$RV_bt_dom+2*sh$RV_bt_dom_se)),
        col=adjustcolor("blue",alpha.f = 0.1),border = NA)
box()
mtext("Percentage (%) of the landed value",side=2,col="black",line=2.5,cex=0.8,las=0) 
abline(v=c(1975,1985,1995,2013),lty="dotted",col="grey")
text(x=1958,y=94,labels = "Moving offshore",pos=4,col="red1",cex=0.8)
text(x=1978,y=90,labels = "Moving\ninshore",pos=4,col="red1",cex=0.8)
text(x=1996,y=94,labels = "Moving to\ndistant waters",pos=4,col="red1",cex=0.8)
text(x=1975,y=4,labels = "1975:\nSino-Japanese\nFishery Agreement",pos=2,col="grey50",cex=0.8)
text(x=1985,y=60,labels = "1985:\nDeveloping\ndistant-water fisheries",pos=2,col="grey50",cex=0.8)
text(x=1995,y=40,labels = "1995:\nSummer moratorium",pos=4,col="grey50",cex=0.8)
text(x=2013,y=10,labels = "2013:\nAccelerating\nfisheries\nupgrading",pos=2,col="grey50",cex=0.8)

abline(h=50,lty="dotted",col="red1")
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(1950,2018,10)
axis(side = 1, at = ticks)
ticks = seq(0,100,10)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,100,20)
axis(side = 2, at = ticks)
mtext("b)",side=3,adj=0,cex=0.8)
legend("topleft",legend=c("% of total landed value from BTF","% of BTF landed value from C4S","% of BTF landed value from China's claimed EEZ"),
       text.col=c("black","blue","red1"),lty="solid",col=c("black","blue","red1"))## Add Legend

## Fig. S2.2 plot stock catch over time
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
sh<-read.csv("CN_bt_fungroup.csv",header=TRUE,sep=',')
sh2<-read.csv("CN_humancons_fungroup 2020.csv",header=TRUE,sep=',')
str(sh);str(sh2)
sh2$X<-NULL
str(sh)
#
par(mar=c(2,3.8,1.2,1),mfrow=c(4,2),las=1)
max<-max(sh[,4]);max
plot(sh[,4]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,2]~sh$year,type='l',col="blue")
mtext("Ratio",side=2,col="black",line=2.4,cex=.8,las=0) 
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(1.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(1.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
mtext("a) Large fish",side=3,line=0,adj=0,cex=.8)
legend("topleft",legend=c("Catch : Highest catch","Human consumption : All end-use type"),
       cex=1,
       text.col=c("red","blue"),lty=c("solid","solid"),col=c("red","blue"))## Add Legend

max<-max(sh[,7]);max
plot(sh[,7]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,3]~sh2$year,type='l',col="blue")
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
mtext("Ratio",side=2,col="black",line=2.4,cex=.8,las=0) 
mtext("b) Medium fish",side=3,line=0,adj=0,cex=.8)

max<-max(sh[,9]);max
plot(sh[,9]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,5]~sh2$year,type='l',col="blue")
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
mtext("Ratio",side=2,col="black",line=2.4,cex=.8,las=0) 
mtext("c) Cephalopods",side=3,line=0,adj=0,cex=.8)

max<-max(sh[,11]);max
plot(sh[,11]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,6]~sh2$year,type='l',col="blue")
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
mtext("Ratio",side=2,col="black",line=2.4,cex=.8,las=0) 
mtext("d) Shrimps",side=3,line=0,adj=0,cex=.8)

max<-max(sh[,13]);max
plot(sh[,13]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,7]~sh2$year,type='l',col="blue")
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
mtext("Ratio",side=2,col="black",line=2.4,cex=.8,las=0) 
mtext("e) Crabs & lobsters",side=3,line=0,adj=0,cex=.8)
max<-max(sh[,15]);max
plot(sh[,15]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,8]~sh2$year,type='l',col="blue")
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
mtext("Ratio",side=2,col="black",line=2.4,cex=.8,las=0) 
mtext("f) Jellyfish",side=3,line=0,adj=0,cex=.8)

max<-max(sh[,17]);max
plot(sh[,17]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,4]~sh2$year,type='l',col="blue")
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
mtext("Ratio",side=2,col="black",line=2.4,cex=.8,las=0) 
mtext("g) Small fish & other invertebrates",side=3,line=0,adj=0,cex=.8)


###for major stocks in each assemblage
## ## Fig. S2.3 
sh<-read.csv("CN_bt_stocks 2020.csv",header=TRUE,sep=',')
str(sh)
sh$X<-NULL
sh2<-read.csv("CN_humancons_stocks 2020.csv",header=TRUE,sep=',')
sh2$X<-NULL
str(sh2)
par(mar=c(2,3.8,1.6,1),mfrow=c(4,3),las=1)
max<-max(sh[,2]);max
plot(sh[,2]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,2]~sh2$year,type='l',col="blue")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(1.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(1.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
text(x=1994,y=.3,labels = "Dominated by\njuveniles",pos=4,col="red1",cex=1)
mtext("Ratio",side=2,col="black",line=2.4,cex=1,las=0) 
mtext(expression("a) "*italic("Trichiurus lepturus")*" (large fish)"),side=3,line=,adj=0,cex=.8)

legend("topleft",legend=c("Catch : Maximum catch","Human consumption :\nAll catch"),
       cex=1,
       text.col=c("red","blue"),lty=c("solid","solid"),col=c("red","blue"))## Add Legend

max<-max(sh[,3]);max
plot(sh[,3]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,3]~sh2$year,type='l',col="blue")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(1.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(1.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

mtext("Ratio",side=2,col="black",line=2.4,cex=1,las=0) 
mtext(expression("b) "*italic("Muraenesox cinereus")*" (large fish)"),side=3,line=,adj=0,cex=.8)

max<-max(sh[,7]);max
plot(sh[,7]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,7]~sh2$year,type='l',col="blue")
mtext("Ratio",side=2,col="black",line=2.4,cex=1,las=0) 
mtext(expression("c) "*italic("Larimichthys crocea")*" (medium fish)"),side=3,line=,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(1.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(1.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

abline(v=c(1974,1986,1995),lty="dotted",col="red1")
text(x=1975,y=.1,labels = "Overfished",pos=4,col="red1",cex=1)
text(x=1994,y=.3,labels = "Dominated by\njuveniles",pos=4,col="red1",cex=1)

max<-max(sh[,8]);max
plot(sh[,8]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,8]~sh2$year,type='l',col="blue")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(1.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(1.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
text(x=1996,y=.3,labels = "Dominated by\njuveniles",pos=4,col="red1",cex=1)
mtext("Ratio",side=2,col="black",line=2.4,cex=1,las=0) 
mtext(expression("d) "*italic("Larimichthys polyactis")*" (medium fish)"),side=3,line=,adj=0,cex=.8)
abline(v=c(1996,2010),lty="dotted",col="red1")

max<-max(sh[,10]);max
plot(sh[,10]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,10]~sh2$year,type='l',col="blue")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(1.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(1.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
mtext("Ratio",side=2,col="black",line=2.4,cex=1,las=0) 
mtext(expression("e) "*italic("Pampus spp.")*" (medium fish)"),side=3,line=,adj=0,cex=.8)

max<-max(sh[,14]);max
plot(sh[,14]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,14]~sh2$year,type='l',col="blue")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(1.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(1.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
mtext("Ratio",side=2,col="black",line=2.4,cex=1,las=0) 
mtext(expression("f) "*italic("Nemipterus spp.")*" (small fish)"),side=3,line=,adj=0,cex=.8)

max<-max(sh[,19]);max
plot(sh[,19]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,19]~sh2$year,type='l',col="blue")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(1.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(1.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
mtext("Ratio",side=2,col="black",line=2.4,cex=1,las=0) 
text(x=2000,y=.1,labels = "Overfished",pos=4,col="red1",cex=1)
mtext(expression("g) "*italic("Todarodes pacificus")*" (cephalopods)"),side=3,line=,adj=0,cex=.8)

max<-max(sh[,21]);max
plot(sh[,21]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,21]~sh2$year,type='l',col="blue")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(1.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(1.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
mtext("Ratio",side=2,col="black",line=2.4,cex=1,las=0) 
mtext(expression("h) "*italic("Trachysalambria curvirostris")*" (shrimp)"),side=3,line=,adj=0,cex=.8)

max<-max(sh[,24]);max
plot(sh[,24]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,24]~sh2$year,type='l',col="blue")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(1.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(1.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
mtext("Ratio",side=2,col="black",line=2.4,cex=1,las=0) 
mtext(expression("i) "*italic("Portunus pelagicus")*" (crab)"),side=3,line=,adj=0,cex=.8)

max<-max(sh[,25]);max
plot(sh[,25]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,25]~sh2$year,type='l',col="blue")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(1.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(1.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
mtext("Ratio",side=2,col="black",line=2.4,cex=1,las=0) 
mtext("j) Scyphozoa (jellyfish)",side=3,line=.1,adj=0,cex=.8)

max<-max(sh[,17]);max
plot(sh[,17]/max~sh$year,type='l',col="red1",xlim=c(1950,2015),ylab="",xlab="")
lines(sh2[,17]~sh2$year,type='l',col="blue")
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(1.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(1.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(1.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
mtext("Ratio",side=2,col="black",line=2.4,cex=1,las=0) 
mtext("k) Mollusca (other invertebrates)",side=3,line=,adj=0,cex=.8)

## Fig. S2.4 Mean price of each stock assemblage
par(mar=c(2,4,1,1),mfrow=c(1,1),las=1)
sh<-read.csv("CN_bt_fungroup2 2020.csv",header=TRUE,sep=',')
str(sh)
max<-max(na.omit(sh$VpC_larg),na.omit(sh$VpC_med),na.omit(sh$VpC_smed),na.omit(sh$VpC_sm),na.omit(sh$VpC_ceph),na.omit(sh$VpC_shri),na.omit(sh$VpC_jell),na.omit(sh$VpC_lob))/1000
max
min<-min(na.omit(sh$VpC_larg),na.omit(sh$VpC_ceph),na.omit(sh$VpC_shri),na.omit(sh$VpC_jell),na.omit(sh$VpC_tras))/1000
min
plot(sh$VpC_larg/1000~sh$year,type='l',col="red1",xlim=c(1950,2015),ylim=c(min,max),ylab="",xlab="",lwd=2,axes=F)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(30,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(30,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(30,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(30,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
lines(sh$VpC_med/1000~sh$year,type='l',col="blue",lwd=2)
lines(sh$VpC_sm/1000~sh$year,type='l',col="green3",lwd=1)
lines(sh$VpC_ceph/1000~sh$year,type='l',col="purple3",lwd=2)
lines(sh$VpC_shri/1000~sh$year,type='l',col="gold3",lwd=2)
lines(sh$VpC_jell/1000~sh$year,type='l',col="maroon2",lwd=1)
lines(sh$VpC_lob/1000~sh$year,type='l',col="black",lwd=1)
box()
mtext("Mean price (US$1000 / t)",side=2,col="black",line=2,cex=1,las=0) 
abline(v=c(1955,1973,1978),lty="dotted",col="grey50")
text(x=1955,y=8,labels = "1955:\nNo-trawl\nzone",pos=4,col="grey50",cex=0.8)
text(x=1970,y=8,labels = "Shrimp\nfarming\n(since 1970s)",pos=3,col="red1",cex=0.8)
text(x=1973,y=19,labels = "1973:\nCrude\nOil crisis",pos=2,col="grey50",cex=0.8)
text(x=1978,y=10,labels = "1978:\nEconomic reform\n(price liberalization)",pos=4,col="grey50",cex=0.8)
text(x=1981,y=7,labels = "Biomass trawling",pos=4,col="red1",cex=0.8)
legend("topright",legend=c("Large fish","Medium fish","Cephalopods","Shrimps","Jellyfish","Lobsters & crabs","Small fish & other invertebrates"),
       cex = 1,
       text.col=c("red1","blue","purple3","gold3","maroon2","black","green3"),text.font=c(2,2,2,2,1,1,1),lwd=c(2,2,2,2,1,1,1),lty="solid",col=c("red1","blue","purple3","gold3","maroon2","black","green3"))## Add Legend
ticks = seq(1950,2015,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
Axis(side=1,at=seq(1950,2015,by=10))
Axis(side=2,at=seq(0,20,by=5))
ticks = seq(0,20,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)

# Fig. S2.5 Large fish
sh<-read.csv("CN_bt_stocks 2020.csv",header=TRUE,sep=',')
str(sh)
sh2<-read.csv("CN_bt_fungroup.csv",header=TRUE,sep=',')
par(mar=c(2,3.8,1.6,1),mfrow=c(3,2),las=1)
max<-max(sh[,2])/100000;max
plot(sh[,2]/100000~sh$year,type='l',col="red1",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (100,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("a) Largehead hairtail ("*italic("Trichiurus lepturus")*")"),side=3,line=,adj=0,cex=.8)

polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(50,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(50,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(50,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(50,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
max<-max(sh[,3])/10000;max
plot(sh[,3]/10000~sh$year,type='l',col="red1",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (10,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("b) Daggertooth pike conger ("*italic("Muraenesox cinereus")*")"),side=3,line=,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(12,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(12,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(12,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(12,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
max<-max(sh[,4])/1000;max
plot(sh[,4]/1000~sh$year,type='l',col="red1",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (1000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("c) Amberjacks ("*italic("Seriola spp.")*")"),side=3,line=,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(13,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(13,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(13,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(13,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh[,5])/1000;max
plot(sh[,5]/1000~sh$year,type='l',col="red1",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (1000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("d) Bastard halibut ("*italic("Paralichthys olivaceus")*")"),side=3,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(5.2,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(5.2,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(5.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(5.2,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh[,6])/1000;max
plot(sh[,6]/1000~sh$year,type='l',col="red1",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (1000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("e) Pacific cod ("*italic("Gadus macrocephalus")*")"),side=3,line=,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(6,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(6,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(6,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(6,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh2[,5])/100000;max
plot(sh2[,5]/100000~sh$year,type='l',lwd=2,col="red1",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (100,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("f) All large fish",side=3,adj=0,line=.1,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(7,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(7,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(7,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(7,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)


### Fig. S2.6 medium fish
par(mar=c(2,3.8,1.6,1),mfrow=c(3,2),las=1)
max<-max(sh[,7])/10000;max
plot(sh[,7]/10000~sh$year,type='l',col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (10,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("a) Large Yellow Croacker ("*italic("Larimichthys crocea")*")"),side=3,line=,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(7,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(7,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(7,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(7,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh[,8])/10000;max
plot(sh[,8]/10000~sh$year,type='l',col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (10,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("b) Small Yellow Croacker ("*italic("Larimichthys polyactis")*")"),side=3,line=,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh[,9])/10000;max
plot(sh[,9]/10000~sh$year,type='l',col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (10,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("c) Filefish (Monacanthidae)",side=3,line=,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh[,10])/10000;max
plot(sh[,10]/10000~sh$year,type='l',col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (10,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("d) Silver pomfrets ("*italic("Pampus spp.")*")"),side=3,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh[,11])/10000;max
plot(sh[,11]/10000~sh$year,type='l',col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (10,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("e) Porgies & seabreams (Sparidae)",side=3,line=,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)
str(sh2)
max<-max(sh2[,7])/100000;max
plot(sh2[,7]/100000~sh$year,type='l',lwd=2,col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (100,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("f) All medium fish",side=3,adj=0,line=.1,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)


## Fig. S2.7 for shrimps
par(mar=c(2,3.8,1.6,1),mfrow=c(3,1),las=1)

max<-max(sh[,19])/100000;max
plot(sh[,19]/100000~sh$year,type='l',col="gold3",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (100,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("a) Southern rough shrimp ("*italic("Trachysalambria curvirostris")*")"),side=3,line=,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh[,20])/10000;max
plot(sh[,20]/10000~sh$year,type='l',col="gold3",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (10,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("b) Akiami paste shrimp (Acetes japonicus)",side=3,line=.1,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh2[,11])/100000;max
plot(sh2[,11]/100000~sh$year,type='l',lwd=2,col="gold3",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (100,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("c) All shrimps",side=3,adj=0,line=.1,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

## Fig. S2.8 for Cephelopods
par(mar=c(2,3.8,1.6,1),mfrow=c(3,1),las=1)
max<-max(sh[,17])/100000;max
plot(sh[,17]/100000~sh$year,type='l',col="purple3",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (100,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("a) Japanese flying squid ("*italic("Todarodes pacificus")*")"),side=3,line=,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh[,18])/1000;max
plot(sh[,18]/1000~sh$year,type='l',col="purple3",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (1000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("b) Cuttlefishes & bobtail squids (Sepiida)",side=3,line=.1,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh2[,9])/100000;max
plot(sh2[,9]/100000~sh$year,type='l',lwd=2,col="purple3",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (100,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("c) All cephalopods",side=3,adj=0,line=.1,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)


#
## Fig. S2.9 for crabs & lobsters
par(mar=c(2,3.8,1.6,1),mfrow=c(3,1),las=1)

max<-max(sh[,22])/100000;max
plot(sh[,22]/100000~sh$year,type='l',col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (100,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("a) Blue swimming crab ("*italic("Portunus pelagicus")*")"),side=3,line=,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh[,21])/100000;max
plot(sh[,21]/100000~sh$year,type='l',col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (100,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("b) Gazami crab ("*italic("Portunus trituberculatus")*")"),side=3,line=,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh2[,13])/100000;max
plot(sh2[,13]/100000~sh$year,type='l',lwd=2,col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (100,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("c) All crabs & lobsters",side=3,adj=0,line=.1,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

### Fig. S2.10 small fish & other invertebrates
par(mar=c(2,3.8,1.6,1),mfrow=c(3,2),las=1)
max<-max(sh[,17])/100000;max
plot(sh[,17]/100000~sh$year,type='l',col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (100,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("a) Clams & seasnails (Mollusca)",side=3,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh[,14])/10000;max
plot(sh[,14]/10000~sh$year,type='l',col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (10,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("b) Threadfin breams ("*italic("Nemipterus spp.")*")"),side=3,line=.1,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh[,18])/10000;max
plot(sh[,18]/10000~sh$year,type='l',col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (10,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("c) Japanese carpet shell ("*italic("Ruditapes philippinarum")*")"),side=3,line=.1,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh[,15])/1000;max
plot(sh[,15]/1000~sh$year,type='l',col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (1000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("d) Sand eels ("*italic("Ammodytes personatus")*")"),side=3,line=.1,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh[,16])/1000;max
plot(sh[,16]/1000~sh$year,type='l',col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (1000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext(expression("e) Japanese sea cucumber ("*italic("Apostichopus japonicus")*")"),side=3,line=.1,adj=0,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

max<-max(sh2[,17])/100000;max
plot(sh2[,17]/100000~sh$year,type='l',lwd=2,col="blue",xlim=c(1950,2015),ylim=c(0,max),ylab="",xlab="")
mtext("Catch (100,000 t)",side=2,line=2.5,las=0)
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("f) All small fish & other invertebrates",side=3,adj=0,line=.1,cex=.8)
polygon(x=c(sh$year[1:14],rev(sh$year[1:14])),
        y=c(rep(-10,14), rev(rep(24,14))),
        col=adjustcolor("cyan",alpha.f = 0.1),border = NA)
polygon(x=c(sh$year[14:29],rev(sh$year[14:29])),
        y=c(rep(-10,16), rev(rep(24,16))),
        col=adjustcolor("cyan",alpha.f = 0.2),border = NA)
polygon(x=c(sh$year[29:47],rev(sh$year[29:47])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.3),border = NA)
polygon(x=c(sh$year[47:65],rev(sh$year[47:65])),
        y=c(rep(-10,19), rev(rep(24,19))),
        col=adjustcolor("cyan",alpha.f = 0.5),border = NA)

##################################################
##############Figure S2.12 & S2.13: Codes provided here are for the less important eezs
### Similar code used to derive the catch estimate in each eez can be found in Fig. 6 in 'Fiugre for Nat.R'
getwd()
all_precip_files <- list.files("DW EEZs/stack/new2", pattern = "*.csv",
                               full.names = TRUE)
all_precip_files
all_precip_files[1]
df_eez<-read.csv(all_precip_files[1], header = TRUE, sep=',')
df<-melt(df_eez,id="Year")
ggplot(df,aes(x = Year, y = value/1000000, fill=variable))+
        geom_area(alpha=1 , size=.2, colour="white")+
        labs(x = "Year", y = "Catch (million t)")+
        scale_x_continuous(breaks=seq(1950,2014,10)
        )+
        scale_fill_manual("Fishing Entity",
                          values = c("darkgreen","red2","blue3"))+
        theme(legend.position = "right",
              legend.justification = c("left", "top"))

p1<-ggplot(df,aes(x = Year, y = value/1000000, fill=variable))+
        geom_area(alpha=1 , size=.2, colour="white")+
        labs(x = "", y = "")+
        scale_x_continuous(breaks=seq(1950,2014,10)
        )+
        scale_fill_manual("Fishing Entity:",
                          values = c("darkgreen","red2","blue3"))+
        theme(axis.text=element_text(size=8,face = "bold"),
              legend.title = element_text(size = 15, face = "bold"),
              legend.text = element_text(size = 15, face = "bold"),
              legend.position = "top")  
p1
### Please make sure the same rules have been followed as you create Figure 6.
library(ggpubr)
## For Figure S2.12
tiff("DW EEZs/stack/new2/stacked catch2.tif",width = 13, height = 8, units = 'in', res = 300)
ggarrange(p2,p3,p17,p1,p13,p12,p6,p15,p11,p16,p5,p10,ncol=4,nrow=3,common.legend = T, legend = "top",
          labels=c("a", "b","c","d","e","f","g","h","i","j","k","l")
)
dev.off()
## For Figure S2.13
tiff("DW EEZs/stack/new2/stacked catch3.tif",width = 13, height = 5.3, units = 'in', res = 300)
ggarrange(p19,p8,p9,p14,p7,p18,p4,p,ncol=4,nrow=2,common.legend = T, legend.grob = get_legend(p14),
          labels=c("a", "b","c","d","e","f","g")
)
dev.off()
#