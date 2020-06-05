############################ Reconstructing catch by China's BTF and related analyses, based on timeseries statistics from SAUP datasets
## install packages
install.packages('data.table','reshape2','ggplot2','forecast')
## download the dataset from Sea Around Us Project by fishing entity = China
## Correct the data in excel according to the the methodology in "Reconstructing fishing capacity and landings of China's bottom trawl fisheries (1950 - 2018)", by Xiong Zhang & Amanda C.J. Vincent, published as a working paper by Institute for the Oceans and Fisheries, UBC.
## This includes: 1) correct functional groups of several taxa that were wrongly identified for China's BTF catch; 2) corrected the reconstruction for the early period between 1950 and 1984, when China's fishing fleets did not yet fish in waters beyond China's four seas.

### Section 1: spliting data by year for China's four seas
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
df<-read.csv("CN_catch_SAUP.csv",header=TRUE,sep=',') ## Read the cleaned dataset after the above correction

## create pelagic-trawler's data by year
#subset data
str(df)
df<-df[c(3,11,13,14)]
str(df)
library(data.table)
pl_df<-df[df[,2] %like% "pelagic",]
head(pl_df)
bt_df<-df[df[,2] %like% "bottom|shrimp",]
summary(bt_df)
head(bt_df)
# combine data in the same year
tr_c<-0
tr_v<-0
j<-0
all_c<-0
all_v<-0
rc_bt_tr<-0
rv_bt_tr<-0
rc_bt_all<-0
rv_bt_all<-0
for(i in 1:65){
  j=1949+i
  all_c[i]<-sum(subset(df[,3],df[,1]==j))
  tr_c[i]<-sum(subset(bt_df[,3],bt_df[,1]==j))+sum(subset(pl_df[,3],pl_df[,1]==j))
  all_v[i]<-sum(subset(df[,4],df[,1]==j))
  tr_v[i]<-sum(subset(bt_df[,4],bt_df[,1]==j))+sum(subset(pl_df[,4],pl_df[,1]==j))
  rc_bt_tr[i]<-sum(subset(bt_df[,3],bt_df[,1]==j))/tr_c[i]
  rv_bt_tr[i]<-sum(subset(bt_df[,4],bt_df[,1]==j))/tr_v[i]
  rc_bt_all[i]<-sum(subset(bt_df[,3],bt_df[,1]==j))/all_c[i]
  rv_bt_all[i]<-sum(subset(bt_df[,4],bt_df[,1]==j))/all_v[i]
}
y<-seq(1950,2014,1)
df<-data.frame(year=y, all_c=all_c, tr_c=tr_c, all_v=all_v, tr_v=tr_v, rc_bt_tr=rc_bt_tr, rv_bt_tr=rv_bt_tr, rc_bt_all=rc_bt_all, rv_bt_all=rv_bt_all)
head(df)
str(df)
head(df)
write.csv(df,"CN_bt_tr_all_catch 2020.csv") 
## add 2015 - 2018 under 'year' before you run the following code
##### 'forecast' catch timeseries to 2018
### arima
df<-read.csv("CN_bt_tr_all_catch 2020.csv",header=TRUE,sep=',')
#
library(forecast)
## arima for rc_bt_all
ari0<-auto.arima(df$rc_bt_all,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (0,1,0) with drift (0.007 +- 0.0023)
r0<-forecast(ari0,h=4)
df$rc_bt_all_ar<-ari0$fitted
df$rc_bt_all_se<-0
df$rc_bt_all_ar
df$rc_bt_all_ar[66:69]<-r0$mean
df$rc_bt_all_se[66:69]<-(r0$mean-r0$lower[,2])/2
## arima for rv_bt_all
ari1<-auto.arima(df$rv_bt_all,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari1) # ARIMA (0,1,1) (ma1: -0.2126 +- 0.1256) with drift (0.0076 +- 0.0026)
r1<-forecast(ari1,h=4)
df$rv_bt_all_ar<-ari1$fitted
df$rv_bt_all_se<-0
df$rv_bt_all_ar
df$rv_bt_all_ar[66:69]<-r1$mean
df$rv_bt_all_se[66:69]<-(r1$mean-r1$lower[,2])/2
## arima for rc_bt_tr
ari2<-auto.arima(df$rc_bt_tr,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari2) # ARIMA (0,1,0) without drift
r2<-forecast(ari2,h=4)
df$rc_bt_tr_ar<-ari2$fitted
df$rc_bt_tr_se<-0
df$rc_bt_tr_ar[66:69]<-r2$mean
df$rc_bt_tr_se[66:69]<-(r2$mean-r2$lower[,2])/2
## arima for rv_bt_tr
ari3<-auto.arima(df$rv_bt_tr,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari3) # ARIMA (0,1,1) (ma1: -0.5256 +- 0.1607) without drift
r3<-forecast(ari3,h=4)
df$rv_bt_tr_ar<-ari3$fitted
df$rv_bt_tr_se<-0
df$rv_bt_tr_ar[66:69]<-r3$mean
df$rv_bt_tr_se[66:69]<-(r3$mean-r3$lower[,2])/2
## output data
str(df)
head(df)
write.csv(df,"CN_bt_tw_stat 2020.csv")

# Fig A25: plot ratio of pelagic trawling catch
par(mfrow=c(2,1),mar=c(4,5,1,0.5))
max<-max(na.omit(df$rc_bt_all),na.omit(df$rv_bt_all),r0$upper[,2],r1$upper[,2])*100;max
min<-min(na.omit(df$rc_bt_all),na.omit(df$rv_bt_all))*100;min
plot(100*rc_bt_all~year,df,typ="p",ylim=c(min,max),xlab="",las=1,cex.lab=1, ylab="Contribution (%) of BTF in all fisheries\nin terms of catch or landed value")
lines(df$rc_bt_all_ar*100~df$year,df,lty="solid",col="black")
lines((df$rc_bt_all_ar+1.96*df$rc_bt_all_se)*100~df$year,df,lty="dashed",col="black")
lines((df$rc_bt_all_ar-1.96*df$rc_bt_all_se)*100~df$year,df,lty="dashed",col="black")
par(new=T)
plot(100*rv_bt_all~year,df,typ="p",col="red",ylim=c(min,max),xlab="",las=1,ylab="")
lines(df$rv_bt_all_ar*100~df$year,df,lty="solid",col="red")
lines((df$rv_bt_all_ar+1.96*df$rv_bt_all_se)*100~df$year,df,lty="dashed",col="red")
lines((df$rv_bt_all_ar-1.96*df$rv_bt_all_se)*100~df$year,df,lty="dashed",col="red")
abline(v=c(1995,2013),lty="dashed",col="grey")
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
legend("topleft",legend=c("Landed value (Mean, estimated)", "Landed value (95% CI, estimated)","Catch (Mean, estimated)", "Catch (95% CI, estimated)"),
       text.col=c("red","red","black","black"),lty=c("solid","dashed","solid","dashed"),col=c("red","red","black","black"))## Add Legend
mtext("a)",side=3,adj=0,cex=1)

max<-max(na.omit(df$rc_bt_tr),na.omit(df$rv_bt_tr),r2$upper[,2],r3$upper[,2])*100;max
min<-min(na.omit(df$rc_bt_tr),na.omit(df$rv_bt_tr),r2$lower[,2],r3$lower[,2])*100;min
plot(100*rc_bt_tr~year,df,typ="p",ylim=c(min,max),xlab="",las=1,cex.lab=1, ylab="Contribution (%) of BTF in all trawling fisheries\nin terms of catch or landed value")
lines(df$rc_bt_tr_ar*100~df$year,df,lty="solid",col="black")
lines((df$rc_bt_tr_ar+1.96*df$rc_bt_tr_se)*100~df$year,df,lty="dashed",col="black")
lines((df$rc_bt_tr_ar-1.96*df$rc_bt_tr_se)*100~df$year,df,lty="dashed",col="black")
par(new=T)
plot(100*rv_bt_tr~year,df,typ="p",col="red",ylim=c(min,max),xlab="",las=1,ylab="")
lines(df$rv_bt_tr_ar*100~df$year,df,lty="solid",col="red")
lines((df$rv_bt_tr_ar+1.96*df$rv_bt_tr_se)*100~df$year,df,lty="dashed",col="red")
lines((df$rv_bt_tr_ar-1.96*df$rv_bt_tr_se)*100~df$year,df,lty="dashed",col="red")
abline(v=c(1970,1988,1998,2003,2014),lty="dashed",col="grey")
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0) 
legend("bottomleft",legend=c("Landed value (Mean, estimated)", "Landed value (95% CI, estimated)","Catch (Mean, estimated)", "Catch (95% CI, estimated)"),
       text.col=c("red","red","black","black"),lty=c("solid","dashed","solid","dashed"),col=c("red","red","black","black"))## Add Legend
mtext("b)",side=3,adj=0,cex=1)
summary(df)

## create catch data for BTF in China's four seas by year 
df<-read.csv("CN_dom_catch.csv",header=TRUE,sep=',')
#subset data
str(df)
df<-df[c(1,11,12)]
str(df)
tm<-0
j<-0
vm<-0
head(df)
for(i in 1:65){
  j=1949+i
  tm[i]<-sum(subset(df[,2],df[,1]==j))
  vm[i]<-sum(subset(df[,3],df[,1]==j))
}
y<-seq(1950,2014,1)

df<-data.frame(year=y,C_bt_dom=tm,V_bt_dom=vm)
head(df)
str(df)
head(df)
write.csv(df,"CN_bt_dom_stat.csv")

### Section 2: spliting data by year for China's EEZ
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
## create bottom-trawler's data by year in China's seas
df<-read.csv("CN_eez_catch.csv",header=TRUE,sep=',')
str(df)
# unique name and functional group
sn<-unique(df$scientific_name)
sn
write.csv(sn,"CN_sci_names2.csv")

fg<-unique(df$functional_group)
fg
write.csv(fg,"CN_function_group2.csv")
#subset data:C_bt_eez, V_bt_eez
str(df)
df<-df[c(1,4,11,12)]
str(df)
tm1<-0
vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  tm1[i]<-sum(subset(df[,3],df[,1]==j))
  vm1[i]<-sum(subset(df[,4],df[,1]==j))
}
y<-seq(1950,2014,1)

df<-data.frame(year=y,C_bt_eez=tm1,V_bt_eez=vm1)
head(df)
write.csv(df,"CN_bt_eez_stat 2020.csv")

##### estimate catch and landed value for each stock assemblage
## before read the data, you need to correct the functional group accordingly as mentioned in the methodology (reclassify them to seven groups: large fish, medium fish, cephelopod, shrimps, lobsters & crabs, jellyfish, small fish & other invertebrates)
## for large benthic and benthopelagic fish
df<-read.csv("CN_eez_catch.csv",header=TRUE,sep=',')
str(df)
df<-df[c(1,4,11,12)]
library(data.table)
large_df<-df[df[,2] %like% "Large",]
head(large_df)
large_tm1<-0
large_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  large_tm1[i]<-sum(subset(large_df[,3],large_df[,1]==j))
  large_vm1[i]<-sum(subset(large_df[,4],large_df[,1]==j))
}

## for medium benthic and benthopelagic fish
library(data.table)
medium_df<-df[df[,2] %like% "Medium",]
head(medium_df)
summary(medium_df)
medium_tm1<-0
medium_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  medium_tm1[i]<-sum(subset(medium_df[,3],medium_df[,1]==j))
  medium_vm1[i]<-sum(subset(medium_df[,4],medium_df[,1]==j))
}

## for Cephalopods 
# subset data
library(data.table)
ceph_df<-df[df[,2] %like% "Cephalopod",]
head(ceph_df)
ceph_tm1<-0
ceph_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  ceph_tm1[i]<-sum(subset(ceph_df[,3],ceph_df[,1]==j))
  ceph_vm1[i]<-sum(subset(ceph_df[,4],ceph_df[,1]==j))
}

## for shrimps
df2<-df[df[,2] %like% "Shrimp",]
head(df2)
shri_tm1<-0
shri_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  shri_tm1[i]<-sum(subset(df2[,3],df2[,1]==j))
  shri_vm1[i]<-sum(subset(df2[,4],df2[,1]==j))
}

## for lobsters & crabs

df3<-df[df[,2] %like% "Lobster",]
head(df3)
lob_tm1<-0
lob_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  lob_tm1[i]<-sum(subset(df3[,3],df3[,1]==j))
  lob_vm1[i]<-sum(subset(df3[,4],df3[,1]==j))
}

## jellyfish
str(df)
df3<-df[df[,2] %like% "Jellyfish",]
head(df3)
jell_tm1<-0
jell_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  jell_tm1[i]<-sum(subset(df3[,3],df3[,1]==j))
  jell_vm1[i]<-sum(subset(df3[,4],df3[,1]==j))
}

## small fish & other invertebrates
df2<-read.csv("CN_bt_eez_small.csv",header=TRUE,sep=',')
str(df2)
df2<-df2[c(1,4,11,12)]
str(df2)
trash_tm1<-0
trash_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  trash_tm1[i]<-sum(subset(df2[,3],df2[,1]==j))
  trash_vm1[i]<-sum(subset(df2[,4],df2[,1]==j))
}

df<-data.frame(year=y,C_bt_eez=tm1,V_bt_eez=vm1,C_larg_eez=large_tm1,V_large_eez=large_vm1,C_med_eez=medium_tm1,V_med_eez=medium_vm1,C_ceph_eez=ceph_tm1,V_ceph_eez=ceph_vm1,
               C_shri_eez=shri_tm1,V_shri_eez=shri_vm1,C_lob_eez=lob_tm1,V_lob_eez=lob_vm1,C_jell_eez=jell_tm1,V_jell_eez=jell_vm1,C_small_eez=trash_tm1,V_small_eez=trash_vm1)
str(df)
head(df)
write.csv(df,"CN_bt_fungroup.csv")

#### create catch dataset for landings ended up with direct human consumption
df<-read.csv("CN_eez_catch.csv",header=TRUE,sep=',')
str(df)
df<-df[c(1,4,10,11,12)]
library(data.table)
# all catch
catch_df2<-df[df[,3] %like% "Direct",]
head(catch_df2)
head(catch_df2)
catch_tm1<-0
catch_tm2<-0
value_tm1<-0
catch_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  catch_tm1[i]<-sum(subset(catch_df2[,4],catch_df2[,1]==j))
  catch_tm2[i]<-sum(subset(df[,4],df[,1]==j))
  catch_r[i]<-catch_tm1[i]/catch_tm2[i]
  value_tm1[i]<-sum(subset(catch_df2[,5],catch_df2[,1]==j))
}

# large fish
large_df<-df[df[,2] %like% "Large",]
large_df2<-large_df[large_df[,3] %like% "Direct",]
head(large_df)
head(large_df2)
large_tm1<-0
large_tm2<-0
large_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  large_tm1[i]<-sum(subset(large_df2[,4],large_df2[,1]==j))
  large_tm2[i]<-sum(subset(large_df[,4],large_df[,1]==j))
  large_r[i]<-large_tm1[i]/large_tm2[i]
}
plot(large_r~year)

## for medium benthic and benthopelagic fish
library(data.table)
medium_df<-df[df[,2] %like% "Medium",]
medium_df2<-medium_df[medium_df[,3] %like% "Direct",]
head(medium_df)
head(medium_df2)
medium_tm1<-0
medium_tm2<-0
medium_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  medium_tm1[i]<-sum(subset(medium_df2[,4],medium_df2[,1]==j))
  medium_tm2[i]<-sum(subset(medium_df[,4],medium_df[,1]==j))
  medium_r[i]<-medium_tm1[i]/medium_tm2[i]
}
plot(medium_r~year)
## for Cephalopods 
# subset data
library(data.table)
ceph_df<-df[df[,2] %like% "Cephalopod",]
ceph_df2<-ceph_df[ceph_df[,3] %like% "Direct",]
head(ceph_df)
head(ceph_df2)
ceph_tm1<-0
ceph_tm2<-0
ceph_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  ceph_tm1[i]<-sum(subset(ceph_df2[,4],ceph_df2[,1]==j))
  ceph_tm2[i]<-sum(subset(ceph_df[,4],ceph_df[,1]==j))
  ceph_r[i]<-ceph_tm1[i]/ceph_tm2[i]
}
plot(ceph_r~year)
## for shrimps
shrimp_df<-df[df[,2] %like% "Shrimp",]
shrimp_df2<-shrimp_df[shrimp_df[,3] %like% "Direct",]
head(shrimp_df2)
head(shrimp_df)
shrimp_tm1<-0
shrimp_tm2<-0
shrimp_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  shrimp_tm1[i]<-sum(subset(shrimp_df2[,4],shrimp_df2[,1]==j))
  shrimp_tm2[i]<-sum(subset(shrimp_df[,4],shrimp_df[,1]==j))
  shrimp_r[i]<-shrimp_tm1[i]/shrimp_tm2[i]
}
plot(shrimp_r~year)

## for lobsters & crabs
crab_df<-df[df[,2] %like% "Lobster",]
head(df3)
crab_df2<-crab_df[crab_df[,3] %like% "Direct",]
head(crab_df2)
head(crab_df)
crab_tm1<-0
crab_tm2<-0
crab_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  crab_tm1[i]<-sum(subset(crab_df2[,4],crab_df2[,1]==j))
  crab_tm2[i]<-sum(subset(crab_df[,4],crab_df[,1]==j))
  crab_r[i]<-crab_tm1[i]/crab_tm2[i]
}
plot(crab_r~year)
## jellyfish
str(df)
jelly_df<-df[df[,2] %like% "Jellyfish",]
jelly_df2<-jelly_df[jelly_df[,3] %like% "Direct",]
head(jelly_df2)
head(jelly_df)
jelly_tm1<-0
jelly_tm2<-0
jelly_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  jelly_tm1[i]<-sum(subset(jelly_df2[,4],jelly_df2[,1]==j))
  jelly_tm2[i]<-sum(subset(jelly_df[,4],jelly_df[,1]==j))
  jelly_r[i]<-jelly_tm1[i]/jelly_tm2[i]
}
plot(jelly_r~year)
## small fish & other invertebrates
df2<-read.csv("CN_bt_eez_small.csv",header=TRUE,sep=',')
str(df2)
smot_df2<-df2[df2[,10] %like% "Direct",]
head(df2)
head(smot_df2)
smot_tm1<-0
smot_tm2<-0
smot_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  smot_tm1[i]<-sum(subset(smot_df2[,11],smot_df2[,1]==j))
  smot_tm2[i]<-sum(subset(df2[,11],df2[,1]==j))
  smot_r[i]<-smot_tm1[i]/smot_tm2[i]
}
plot(smot_r~year)
plot(medium_tm2~year)
y<-c(1950:2014)
df<-data.frame(year=y,Large=large_r,medium=medium_r,small=smot_r,ceph=ceph_r,shri=shrimp_r,crab=crab_r,jell=jelly_r,catch=catch_r)
str(df)
head(df)
write.csv(df,"CN_humancons_fungroup 2020.csv")

########################################### for specific BTF stocks ########################
### split catch data based on major BTF stocks
df<-read.csv("CN_eez_catch.csv",header=TRUE,sep=',')
str(df)
df<-df[c(1,3,11,12)]
library(data.table)
## for Largehead hairtail - large fish
largehead_df<-df[df[,2] %like% "Largehead hairtail",]
head(largehead_df)
largehead_tm1<-0
largehead_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  largehead_tm1[i]<-sum(subset(largehead_df[,3],largehead_df[,1]==j))
  largehead_vm1[i]<-sum(subset(largehead_df[,4],largehead_df[,1]==j))
}
largehead_tm1
## for Daggertooth pike conger (Muraenesox cinereus) - large fish 
conger_df<-df[df[,2] %like% "Daggertooth pike conger",]
head(conger_df)
conger_tm1<-0
conger_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  conger_tm1[i]<-sum(subset(conger_df[,3],conger_df[,1]==j))
  conger_vm1[i]<-sum(subset(conger_df[,4],conger_df[,1]==j))
}
## for Amberjacks   - large fish 
amber_df<-df[df[,2] %like% "Amberjacks",]
head(amber_df)
amber_tm1<-0
amber_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  amber_tm1[i]<-sum(subset(amber_df[,3],amber_df[,1]==j))
  amber_vm1[i]<-sum(subset(amber_df[,4],amber_df[,1]==j))
}
## for Bastard halibut   - large fish 
halibut_df<-df[df[,2] %like% "Bastard halibut",]
head(halibut_df)
halibut_tm1<-0
halibut_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  halibut_tm1[i]<-sum(subset(halibut_df[,3],halibut_df[,1]==j))
  halibut_vm1[i]<-sum(subset(halibut_df[,4],halibut_df[,1]==j))
}
## for Pacific cod   - large fish 
cod_df<-df[df[,2] %like% "Pacific cod",]
head(cod_df)
cod_tm1<-0
cod_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  cod_tm1[i]<-sum(subset(cod_df[,3],cod_df[,1]==j))
  cod_vm1[i]<-sum(subset(cod_df[,4],cod_df[,1]==j))
}
## for Large Yellow Croacker - medium fish
large_df<-df[df[,2] %like% "Large yellow croaker",]
head(large_df)
large_tm1<-0
large_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  large_tm1[i]<-sum(subset(large_df[,3],large_df[,1]==j))
  large_vm1[i]<-sum(subset(large_df[,4],large_df[,1]==j))
}

##
## for Small Yellow Croacker - medium fish
small_df<-df[df[,2] %like% "Yellow croaker",]
head(small_df)
small_tm1<-0
small_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  small_tm1[i]<-sum(subset(small_df[,3],small_df[,1]==j))
  small_vm1[i]<-sum(subset(small_df[,4],small_df[,1]==j))
}
##

## for Filefish (Monacanthidae) - medium fish 
filefish_df<-df[df[,2] %like% "Filefish",]
head(filefish_df)
filefish_tm1<-0
filefish_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  filefish_tm1[i]<-sum(subset(filefish_df[,3],filefish_df[,1]==j))
  filefish_vm1[i]<-sum(subset(filefish_df[,4],filefish_df[,1]==j))
}
filefish_tm1
## for silver pomfrets - medium fish 
pomfret_df<-df[df[,2] %like% "Silver pomfrets",]
head(pomfret_df)
pomfret_tm1<-0
pomfret_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  pomfret_tm1[i]<-sum(subset(pomfret_df[,3],pomfret_df[,1]==j))
  pomfret_vm1[i]<-sum(subset(pomfret_df[,4],pomfret_df[,1]==j))
}
pomfret_tm1
## for porgies, seabreams - medium fish 
porgies_df<-df[df[,2] %like% "Porgies, seabreams",]
head(porgies_df)
porgies_tm1<-0
porgies_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  porgies_tm1[i]<-sum(subset(porgies_df[,3],porgies_df[,1]==j))
  porgies_vm1[i]<-sum(subset(porgies_df[,4],porgies_df[,1]==j))
}
porgies_tm1

## for Threadfin breams (????????????) - small fish
bream_df<-df[df[,2] %like% "Threadfin breams",]
head(bream_df)
bream_tm1<-0
bream_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  bream_tm1[i]<-sum(subset(bream_df[,3],bream_df[,1]==j))
  bream_vm1[i]<-sum(subset(bream_df[,4],bream_df[,1]==j))
}
## for Sand eels (Ammodytes personatus ?????????????????? ) - small fish
sandeel_df<-df[df[,2] %like% "Sand eels",]
head(sandeel_df)
sandeel_tm1<-0
sandeel_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  sandeel_tm1[i]<-sum(subset(sandeel_df[,3],sandeel_df[,1]==j))
  sandeel_vm1[i]<-sum(subset(sandeel_df[,4],sandeel_df[,1]==j))
}
## for Japanese sea cucumber  - other invertebrates#, Clams, seasnails, squids, octopuses (Mollusca), Japanese carpet shell
cucumber_df<-df[df[,2] %like% "Japanese sea cucumber",]
head(cucumber_df)
cucumber_tm1<-0
cucumber_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  cucumber_tm1[i]<-sum(subset(cucumber_df[,3],cucumber_df[,1]==j))
  cucumber_vm1[i]<-sum(subset(cucumber_df[,4],cucumber_df[,1]==j))
}
## for mollusca - other invertebrates
mollusca_df<-df[df[,2] %like% "Clams, seasnails, squids, octopuses",]
head(mollusca_df)
mollusca_tm1<-0
mollusca_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  mollusca_tm1[i]<-sum(subset(mollusca_df[,3],mollusca_df[,1]==j))
  mollusca_vm1[i]<-sum(subset(mollusca_df[,4],mollusca_df[,1]==j))
}
## for Japanese carpet shell - other invertebrates
carpetshell_df<-df[df[,2] %like% "Japanese carpet shell",]
head(carpetshell_df)
carpetshell_tm1<-0
carpetshell_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  carpetshell_tm1[i]<-sum(subset(carpetshell_df[,3],carpetshell_df[,1]==j))
  carpetshell_vm1[i]<-sum(subset(carpetshell_df[,4],carpetshell_df[,1]==j))
}
## for Japanese flying squid (Todarodes pacificus) - Cephelopods
squid_df<-df[df[,2] %like% "Japanese flying squid",]
head(squid_df)
squid_tm1<-0
squid_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  squid_tm1[i]<-sum(subset(squid_df[,3],squid_df[,1]==j))
  squid_vm1[i]<-sum(subset(squid_df[,4],squid_df[,1]==j))
}
## for Cuttlefishes, bobtail squids - Cephelopods
cuttlefish_df<-df[df[,2] %like% "Cuttlefishes, bobtail squids",]
head(cuttlefish_df)
cuttlefish_tm1<-0
cuttlefish_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  cuttlefish_tm1[i]<-sum(subset(cuttlefish_df[,3],cuttlefish_df[,1]==j))
  cuttlefish_vm1[i]<-sum(subset(cuttlefish_df[,4],cuttlefish_df[,1]==j))
}

## for Southern rough shrimp (Trachysalambria curvirostris) - shrimps
df2<-df[df[,2] %like% "Southern rough shrimp",]
head(df2)
sr_shri_tm1<-0
sr_shri_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  sr_shri_tm1[i]<-sum(subset(df2[,3],df2[,1]==j))
  sr_shri_vm1[i]<-sum(subset(df2[,4],df2[,1]==j))
}
## for Akiami paste shrimp (Acetes japonicus) - shrimps
df2<-df[df[,2] %like% "Akiami paste shrimp",]
head(df2)
ap_shri_tm2<-0
ap_shri_vm2<-0
j<-0
for(i in 1:65){
  j=1949+i
  ap_shri_tm2[i]<-sum(subset(df2[,3],df2[,1]==j))
  ap_shri_vm2[i]<-sum(subset(df2[,4],df2[,1]==j))
}

## for Gazami crab (Portunus trituberculatus) - lobsters & crabs
df2<-df[df[,2] %like% "Gazami crab",]
head(df2)
g_crab_tm1<-0
g_crab_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  g_crab_tm1[i]<-sum(subset(df2[,3],df2[,1]==j))
  g_crab_vm1[i]<-sum(subset(df2[,4],df2[,1]==j))
}
## for Blue swimming crab (Portunus pelagicus) - lobsters & crabs
df2<-df[df[,2] %like% "Blue swimming crab",]
head(df2)
bs_crab_tm1<-0
bs_crab_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  bs_crab_tm1[i]<-sum(subset(df2[,3],df2[,1]==j))
  bs_crab_vm1[i]<-sum(subset(df2[,4],df2[,1]==j))
}
## jellyfish
str(df)
df3<-df[df[,2] %like% "jellyfish",]
head(df3)
jell_tm1<-0
jell_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  jell_tm1[i]<-sum(subset(df3[,3],df3[,1]==j))
  jell_vm1[i]<-sum(subset(df3[,4],df3[,1]==j))
}
## flounder
str(df)
df3<-df[df[,2] %like% "Yellow striped flounder",]
head(df3)
flound_tm1<-0
flound_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  flound_tm1[i]<-sum(subset(df3[,3],df3[,1]==j))
  flound_vm1[i]<-sum(subset(df3[,4],df3[,1]==j))
}
##
## flatfish
str(df)
df3<-df[df[,2] %like% "Flatfishes",]
head(df3)
flatfish_tm1<-0
flatfish_vm1<-0
j<-0
for(i in 1:65){
  j=1949+i
  flatfish_tm1[i]<-sum(subset(df3[,3],df3[,1]==j))
  flatfish_vm1[i]<-sum(subset(df3[,4],df3[,1]==j))
}
#
df<-data.frame(year=y, L_largehead =largehead_tm1, L_conger =conger_tm1, L_amber=amber_tm1, L_halibut=halibut_tm1, 
               L_cod=cod_tm1, M_largeyc=large_tm1, M_smallyc=small_tm1, M_filefish =filefish_tm1, 
               M_pomfret=pomfret_tm1, M_porgie=porgies_tm1, SM_flound=flound_tm1, SM_flatfish=flatfish_tm1,
            S_bream =bream_tm1, S_sandeel = sandeel_tm1, 
               I_cucumb=cucumber_tm1, I_mollus=mollusca_tm1, I_shell=carpetshell_tm1, 
               Ce_squid=squid_tm1, Ce_cuttl=cuttlefish_tm1, Sr_srshim=sr_shri_tm1, Sr_apshrim=ap_shri_tm2, 
               Cr_gcrab=g_crab_tm1, Cr_bcrab=bs_crab_tm1, J_jell=jell_tm1)
str(df)
head(df)
write.csv(df,"CN_bt_stocks 2020.csv")

########## fishery stocks for human consumption
df<-read.csv("CN_eez_catch.csv",header=TRUE,sep=',')
str(df)
df<-df[c(1,3,10,11,12)]
par(mfrow=c(4,6))
library(data.table)
## for Largehead hairtail - large fish
largehead_df<-df[df[,2] %like% "Largehead hairtail",]
head(largehead_df)
largehead_df2<-largehead_df[largehead_df[,3] %like% "Direct",]
head(largehead_df)
head(largehead_df2)
largehead_tm1<-0
largehead_tm2<-0
largehead_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  largehead_tm1[i]<-sum(subset(largehead_df2[,4],largehead_df2[,1]==j))
  largehead_tm2[i]<-sum(subset(largehead_df[,4],largehead_df[,1]==j))
  largehead_r[i]<-largehead_tm1[i]/largehead_tm2[i]
}
plot(largehead_r~year,ylim=c(0,1))
## for Daggertooth pike conger (Muraenesox cinereus) - large fish
conger_df<-df[df[,2] %like% "Daggertooth pike conger",]
head(conger_df)
conger_df2<-conger_df[conger_df[,3] %like% "Direct",]
head(conger_df)
head(conger_df2)
conger_tm1<-0
conger_tm2<-0
conger_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  conger_tm1[i]<-sum(subset(conger_df2[,4],conger_df2[,1]==j))
  conger_tm2[i]<-sum(subset(conger_df[,4],conger_df[,1]==j))
  conger_r[i]<-conger_tm1[i]/conger_tm2[i]
}
plot(conger_r~year)
## for Amberjacks   - large fish 
amber_df<-df[df[,2] %like% "Amberjacks",]
head(amber_df)
amber_df2<-amber_df[amber_df[,3] %like% "Direct",]
head(amber_df)
head(amber_df2)
amber_tm1<-0
amber_tm2<-0
amber_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  amber_tm1[i]<-sum(subset(amber_df2[,4],amber_df2[,1]==j))
  amber_tm2[i]<-sum(subset(amber_df[,4],amber_df[,1]==j))
  amber_r[i]<-amber_tm1[i]/amber_tm2[i]
}
plot(amber_r~year)
## for Bastard halibut   - large fish 
halibut_df<-df[df[,2] %like% "Bastard halibut",]
head(halibut_df)
halibut_df2<-halibut_df[halibut_df[,3] %like% "Direct",]
head(halibut_df)
head(halibut_df2)
halibut_tm1<-0
halibut_tm2<-0
halibut_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  halibut_tm1[i]<-sum(subset(halibut_df2[,4],halibut_df2[,1]==j))
  halibut_tm2[i]<-sum(subset(halibut_df[,4],halibut_df[,1]==j))
  halibut_r[i]<-halibut_tm1[i]/halibut_tm2[i]
}
plot(halibut_r~year)
## for Pacific cod   - large fish 
cod_df<-df[df[,2] %like% "Pacific cod",]
head(cod_df)
cod_df2<-cod_df[cod_df[,3] %like% "Direct",]
head(cod_df)
head(cod_df2)
cod_tm1<-0
cod_tm2<-0
cod_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  cod_tm1[i]<-sum(subset(cod_df2[,4],cod_df2[,1]==j))
  cod_tm2[i]<-sum(subset(cod_df[,4],cod_df[,1]==j))
  cod_r[i]<-cod_tm1[i]/cod_tm2[i]
}
plot(cod_r~year)
## for Large Yellow Croacker - medium fish
large_df<-df[df[,2] %like% "Large yellow croaker",]
large_df2<-large_df[large_df[,3] %like% "Direct",]
head(large_df)
head(large_df2)
large_tm1<-0
large_tm2<-0
large_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  large_tm1[i]<-sum(subset(large_df2[,4],large_df2[,1]==j))
  large_tm2[i]<-sum(subset(large_df[,4],large_df[,1]==j))
  large_r[i]<-large_tm1[i]/large_tm2[i]
}
plot(large_r~year)
##
## for Small Yellow Croacker - medium fish
small_df<-df[df[,2] %like% "Yellow croaker",]
head(small_df)
small_df2<-small_df[small_df[,3] %like% "Direct",]
head(small_df)
head(small_df2)
small_tm1<-0
small_tm2<-0
small_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  small_tm1[i]<-sum(subset(small_df2[,4],small_df2[,1]==j))
  small_tm2[i]<-sum(subset(small_df[,4],small_df[,1]==j))
  small_r[i]<-small_tm1[i]/small_tm2[i]
}
plot(small_r~year)
##

## for Filefish (Monacanthidae) - medium fish
filefish_df<-df[df[,2] %like% "Filefish",]
head(filefish_df)
filefish_df2<-filefish_df[filefish_df[,3] %like% "Direct",]
head(filefish_df)
head(filefish_df2)
filefish_tm1<-0
filefish_tm2<-0
filefish_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  filefish_tm1[i]<-sum(subset(filefish_df2[,4],filefish_df2[,1]==j))
  filefish_tm2[i]<-sum(subset(filefish_df[,4],filefish_df[,1]==j))
  filefish_r[i]<-filefish_tm1[i]/filefish_tm2[i]
}
plot(filefish_r~year)
## for Silver pomfrets - medium fish
pomfret_df<-df[df[,2] %like% "Silver pomfrets",]
head(pomfret_df)
pomfret_df2<-pomfret_df[pomfret_df[,3] %like% "Direct",]
head(pomfret_df)
head(pomfret_df2)
pomfret_tm1<-0
pomfret_tm2<-0
pomfret_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  pomfret_tm1[i]<-sum(subset(pomfret_df2[,4],pomfret_df2[,1]==j))
  pomfret_tm2[i]<-sum(subset(pomfret_df[,4],pomfret_df[,1]==j))
  pomfret_r[i]<-pomfret_tm1[i]/pomfret_tm2[i]
}
plot(pomfret_r~year)
## for porgies, seabreams - medium fish 
porgies_df<-df[df[,2] %like% "Porgies, seabreams",]
head(porgies_df)
porgies_df2<-porgies_df[porgies_df[,3] %like% "Direct",]
head(porgies_df)
head(porgies_df2)
porgies_tm1<-0
porgies_tm2<-0
porgies_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  porgies_tm1[i]<-sum(subset(porgies_df2[,4],porgies_df2[,1]==j))
  porgies_tm2[i]<-sum(subset(porgies_df[,4],porgies_df[,1]==j))
  porgies_r[i]<-porgies_tm1[i]/porgies_tm2[i]
}
plot(porgies_r~year)
## for Threadfin breams  - small fish
bream_df<-df[df[,2] %like% "Threadfin breams",]
head(bream_df)
bream_df2<-bream_df[bream_df[,3] %like% "Direct",]
head(bream_df)
head(bream_df2)
bream_tm1<-0
bream_tm2<-0
bream_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  bream_tm1[i]<-sum(subset(bream_df2[,4],bream_df2[,1]==j))
  bream_tm2[i]<-sum(subset(bream_df[,4],bream_df[,1]==j))
  bream_r[i]<-bream_tm1[i]/bream_tm2[i]
}
plot(bream_r~year)
## for Sand eels (Ammodytes personatus) - small fish
sandeel_df<-df[df[,2] %like% "Sand eels",]
head(sandeel_df)
sandeel_df2<-sandeel_df[sandeel_df[,3] %like% "Direct",]
head(sandeel_df)
head(sandeel_df2)
sandeel_tm1<-0
sandeel_tm2<-0
sandeel_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  sandeel_tm1[i]<-sum(subset(sandeel_df2[,4],sandeel_df2[,1]==j))
  sandeel_tm2[i]<-sum(subset(sandeel_df[,4],sandeel_df[,1]==j))
  sandeel_r[i]<-sandeel_tm1[i]/sandeel_tm2[i]
}
plot(sandeel_r~year)
## for Japanese sea cucumber  - other invertebrates
cucumber_df<-df[df[,2] %like% "Japanese sea cucumber",]
head(cucumber_df)
cucumber_df2<-cucumber_df[cucumber_df[,3] %like% "Direct",]
head(cucumber_df)
head(cucumber_df2)
cucumber_tm1<-0
cucumber_tm2<-0
cucumber_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  cucumber_tm1[i]<-sum(subset(cucumber_df2[,4],cucumber_df2[,1]==j))
  cucumber_tm2[i]<-sum(subset(cucumber_df[,4],cucumber_df[,1]==j))
  cucumber_r[i]<-cucumber_tm1[i]/cucumber_tm2[i]
}
plot(cucumber_r~year)
## for Clams, seasnails, squids, octopuses  - other invertebrates
mollusca_df<-df[df[,2] %like% "Clams, seasnails, squids, octopuses",]
head(mollusca_df)
mollusca_df2<-mollusca_df[mollusca_df[,3] %like% "Direct",]
head(mollusca_df)
head(mollusca_df2)
mollusca_tm1<-0
mollusca_tm2<-0
mollusca_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  mollusca_tm1[i]<-sum(subset(mollusca_df2[,4],mollusca_df2[,1]==j))
  mollusca_tm2[i]<-sum(subset(mollusca_df[,4],mollusca_df[,1]==j))
  mollusca_r[i]<-mollusca_tm1[i]/mollusca_tm2[i]
}
plot(mollusca_r~year)
## for Japanese carpet shell  - other invertebrates
carpetshell_df<-df[df[,2] %like% "Japanese carpet shell",]
head(carpetshell_df)
carpetshell_df2<-carpetshell_df[carpetshell_df[,3] %like% "Direct",]
head(carpetshell_df)
head(carpetshell_df2)
carpetshell_tm1<-0
carpetshell_tm2<-0
carpetshell_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  carpetshell_tm1[i]<-sum(subset(carpetshell_df2[,4],carpetshell_df2[,1]==j))
  carpetshell_tm2[i]<-sum(subset(carpetshell_df[,4],carpetshell_df[,1]==j))
  carpetshell_r[i]<-carpetshell_tm1[i]/carpetshell_tm2[i]
}
plot(carpetshell_r~year)
## for Japanese flying squid (Todarodes pacificus) - Cephelopods
squid_df<-df[df[,2] %like% "Japanese flying squid",]
head(squid_df)
squid_df2<-squid_df[squid_df[,3] %like% "Direct",]
head(squid_df)
head(squid_df2)
squid_tm1<-0
squid_tm2<-0
squid_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  squid_tm1[i]<-sum(subset(squid_df2[,4],squid_df2[,1]==j))
  squid_tm2[i]<-sum(subset(squid_df[,4],squid_df[,1]==j))
  squid_r[i]<-squid_tm1[i]/squid_tm2[i]
}
plot(squid_r~year)
## for Cuttlefishes, bobtail squids - Cephelopods
cuttlefish_df<-df[df[,2] %like% "Cuttlefishes, bobtail squids",]
head(cuttlefish_df)
cuttlefish_df2<-cuttlefish_df[cuttlefish_df[,3] %like% "Direct",]
head(cuttlefish_df)
head(cuttlefish_df2)
cuttlefish_tm1<-0
cuttlefish_tm2<-0
cuttlefish_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  cuttlefish_tm1[i]<-sum(subset(cuttlefish_df2[,4],cuttlefish_df2[,1]==j))
  cuttlefish_tm2[i]<-sum(subset(cuttlefish_df[,4],cuttlefish_df[,1]==j))
  cuttlefish_r[i]<-cuttlefish_tm1[i]/cuttlefish_tm2[i]
}
plot(cuttlefish_r~year)

## for Southern rough shrimp (Trachysalambria curvirostris) - shrimps
sr_shri_df<-df[df[,2] %like% "Southern rough shrimp",]
head(sr_shri_df)
sr_shri_df2<-sr_shri_df[sr_shri_df[,3] %like% "Direct",]
head(sr_shri_df)
head(sr_shri_df2)
sr_shri_tm1<-0
sr_shri_tm2<-0
sr_shri_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  sr_shri_tm1[i]<-sum(subset(sr_shri_df2[,4],sr_shri_df2[,1]==j))
  sr_shri_tm2[i]<-sum(subset(sr_shri_df[,4],sr_shri_df[,1]==j))
  sr_shri_r[i]<-sr_shri_tm1[i]/sr_shri_tm2[i]
}
plot(sr_shri_r~year)

## for Akiami paste shrimp - shrimps
ap_shri_df<-df[df[,2] %like% "Akiami paste shrimp",]
head(ap_shri_df)
ap_shri_df2<-ap_shri_df[ap_shri_df[,3] %like% "Direct",]
head(ap_shri_df)
head(ap_shri_df2)
ap_shri_tm1<-0
ap_shri_tm2<-0
ap_shri_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  ap_shri_tm1[i]<-sum(subset(ap_shri_df2[,4],ap_shri_df2[,1]==j))
  ap_shri_tm2[i]<-sum(subset(ap_shri_df[,4],ap_shri_df[,1]==j))
  ap_shri_r[i]<-ap_shri_tm1[i]/ap_shri_tm2[i]
}
plot(ap_shri_r~year)
## for Japanese blue crab (Portunus trituberculatus) - lobsters & crabs
g_crab_df<-df[df[,2] %like% "Gazami crab",]
head(df2)
g_crab_df2<-g_crab_df[g_crab_df[,3] %like% "Direct",]
head(g_crab_df)
head(g_crab_df2)
g_crab_tm1<-0
g_crab_tm2<-0
g_crab_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  g_crab_tm1[i]<-sum(subset(g_crab_df2[,4],g_crab_df2[,1]==j))
  g_crab_tm2[i]<-sum(subset(g_crab_df[,4],g_crab_df[,1]==j))
  g_crab_r[i]<-g_crab_tm1[i]/g_crab_tm2[i]
}
plot(g_crab_r~year)
## for Blue swimming crab  - lobsters & crabs
bs_crab_df<-df[df[,2] %like% "Gazami crab",]
head(df2)
bs_crab_df2<-bs_crab_df[bs_crab_df[,3] %like% "Direct",]
head(bs_crab_df)
head(bs_crab_df2)
bs_crab_tm1<-0
bs_crab_tm2<-0
bs_crab_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  bs_crab_tm1[i]<-sum(subset(bs_crab_df2[,4],bs_crab_df2[,1]==j))
  bs_crab_tm2[i]<-sum(subset(bs_crab_df[,4],bs_crab_df[,1]==j))
  bs_crab_r[i]<-bs_crab_tm1[i]/bs_crab_tm2[i]
}
plot(bs_crab_r~year)
## jellyfish
str(df)
jelly_df<-df[df[,2] %like% "jellyfish",]
head(jelly_df)
jelly_df2<-jelly_df[jelly_df[,3] %like% "Direct",]
head(jelly_df)
head(jelly_df2)
jelly_tm1<-0
jelly_tm2<-0
jelly_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  jelly_tm1[i]<-sum(subset(jelly_df2[,4],jelly_df2[,1]==j))
  jelly_tm2[i]<-sum(subset(jelly_df[,4],jelly_df[,1]==j))
  jelly_r[i]<-jelly_tm1[i]/jelly_tm2[i]
}
plot(jelly_r~year)
## for Yellow striped flounder - small-to-medium fish
flounder_df<-df[df[,2] %like% "Yellow striped flounder",]
head(flounder_df)
flounder_df2<-flounder_df[flounder_df[,3] %like% "Direct",]
head(flounder_df)
head(flounder_df2)
flounder_tm1<-0
flounder_tm2<-0
flounder_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  flounder_tm1[i]<-sum(subset(flounder_df2[,4],flounder_df2[,1]==j))
  flounder_tm2[i]<-sum(subset(flounder_df[,4],flounder_df[,1]==j))
  flounder_r[i]<-flounder_tm1[i]/flounder_tm2[i]
}
plot(flounder_r~year)
## for Flatfishes - small-to-medium fish
flatfish_df<-df[df[,2] %like% "Flatfishes",]
head(flatfish_df)
flatfish_df2<-flatfish_df[flatfish_df[,3] %like% "Direct",]
head(flatfish_df)
head(flatfish_df2)
flatfish_tm1<-0
flatfish_tm2<-0
flatfish_r<-0
j<-0
for(i in 1:65){
  j=1949+i
  flatfish_tm1[i]<-sum(subset(flatfish_df2[,4],flatfish_df2[,1]==j))
  flatfish_tm2[i]<-sum(subset(flatfish_df[,4],flatfish_df[,1]==j))
  flatfish_r[i]<-flatfish_tm1[i]/flatfish_tm2[i]
}
plot(flatfish_r~year)
#
df<-data.frame(year=y, L_largehead =largehead_r, L_conger =conger_r, L_ambj=amber_r, L_halibut=halibut_r, 
               L_cod=cod_r, M_largeyc=large_r, M_smallyc=small_r, M_filefish =filefish_r, 
               M_pomfret=pomfret_r, M_porgie=porgies_r, SM_flound=flounder_r, SM_flatfish=flatfish_r,
               S_bream =bream_r, S_sandeel = sandeel_r, 
               I_cucumb=cucumber_r, I_mollus=mollusca_r, I_shell=carpetshell_r, 
               Ce_squid=squid_r, Ce_cuttl=cuttlefish_r, Sr_srshim=sr_shri_r, Sr_apshrim=ap_shri_r, 
               Cr_gcrab=g_crab_r, Cr_bcrab=bs_crab_r, J_jell=jelly_r)
str(df)
head(df)
write.csv(df,"CN_humancons_stocks 2020.csv")

########## section 3: forcast catch data
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
### arima

df<-read.csv("CN_bt_stat 2020.csv",header=TRUE,sep=',')
library(forecast)
str(df)
## all_c (all catches by China's marine capture fisheries)
ari0<-auto.arima(df$all_c,trace = T) 
summary(ari0) # ARIMA (0,1,0) with drift (0.007 +- 0.0023)
r0<-forecast(ari0,h=4)
df$all_c_ar<-ari0$fitted
df$all_c_se<-0
df$all_c_ar[66:69]<-r0$mean
df$all_c_se[66:69]<-(r0$mean-r0$lower[,2])/2
head(df)
df
str(df)
df$all_c_ar<-as.numeric(df$all_c_ar)
## all_v (all landed values by China's marine capture fisheries)
ari0<-auto.arima(df$all_v,trace = T) 
summary(ari0) # ARIMA (0,1,0) with drift (0.007 +- 0.0023)
r0<-forecast(ari0,h=4)
df$all_v_ar<-ari0$fitted
df$all_v_se<-0
df$all_v_ar[66:69]<-r0$mean
df$all_v_se[66:69]<-(r0$mean-r0$lower[,2])/2
head(df)
df
str(df)
df$all_v_ar<-as.numeric(df$all_v_ar)
write.csv(df,"CN_bt_all 2020.csv")
## Fig A26
par(mfrow=c(2,1))
str(df)
max<-max(na.omit(df$all_c_ar+1.96*df$all_c_se))/10000000;max
min<-min(na.omit(df$all_c_ar+1.96*df$all_c_se))/10000000;min
plot(df$all_c[-(66:69)]/10000000~df$year[-(66:69)],pch=1,xlim=c(1950,2020),ylim=c(min,max),col="black",xlab="",las=1, ylab="")
lines(df$all_c_ar/10000000~df$year,lty="solid",col="black")
lines((df$all_c_ar+1.96*df$all_c_se)/10000000~df$year,df,lty="dashed",col="black")
lines((df$all_c_ar-1.96*df$all_c_se)/10000000~df$year,df,lty="dashed",col="black")
mtext("Catch by China's marine catchers\n(10 million tons)",side=2,col="black",line=2.5,cex=1,las=0) 
abline(v=c(1980,1992,1998,2003),lty="dashed",col="grey")
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,3,0.1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0,cex=1) 
legend("topleft",legend=c("Mean", "95% confidence intervals"),
       text.col=c("black","black"),lty=c("solid","dashed"),col=c("black","black"))## Add Legend
mtext("a)",side=3,line=0.05,adj=0,cex=0.8)

max<-max(na.omit(df$all_v_ar+1.96*df$all_v_se))/10000000000;max
min<-min(na.omit(df$all_v_ar+1.96*df$all_v_se))/10000000000;min
plot(df$all_v[-(66:69)]/10000000000~df$year[-(66:69)],pch=1,xlim=c(1950,2020),ylim=c(min,max),col="black",xlab="",las=1, ylab="")
lines(df$all_v_ar/10000000000~df$year,lty="solid",col="black")
lines((df$all_v_ar+1.96*df$all_v_se)/10000000000~df$year,df,lty="dashed",col="black")
lines((df$all_v_ar-1.96*df$all_v_se)/10000000000~df$year,df,lty="dashed",col="black")
mtext("Landed value by China's marine catchers\n(10 billion $)",side=2,col="black",line=2.5,cex=1,las=0) 
abline(v=c(1975,1992,2003,2014),lty="dashed",col="grey")
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,4,0.1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0,cex=1) 
legend("topleft",legend=c("Mean", "95% confidence intervals"),
       text.col=c("black","black"),lty=c("solid","dashed"),col=c("black","black"))## Add Legend
mtext("b)",side=3,line=0.05,adj=0,cex=0.8)
## C_bt (Catch by bottom trawlers)
ari0<-auto.arima(df$C_bt,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (0,1,0) with drift (0.007 +- 0.0023)
r0<-forecast(ari0,h=4)
df$C_bt_ar<-ari0$fitted
df$C_bt_se<-0
df$C_bt_ar[66:69]<-r0$mean
df$C_bt_se[66:69]<-(r0$mean-r0$lower[,2])/2
head(df)
df
str(df)
df$C_bt_ar<-as.numeric(df$C_bt_ar)
df$C_bt_se<-as.numeric(df$C_bt_se)
## C_bt_eez (Catch by bottom trawlers within China's claimed EEZ)
ari0<-auto.arima(df$C_bt_eez,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (0,1,0) with drift (0.007 +- 0.0023)
r0<-forecast(ari0,h=4)
df$C_bt_eez_ar<-ari0$fitted
df$C_bt_eez_se<-0
df$C_bt_eez_ar[66:69]<-r0$mean
df$C_bt_eez_se[66:69]<-(r0$mean-r0$lower[,2])/2
head(df)
df
str(df)
df$C_bt_eez_ar<-as.numeric(df$C_bt_eez_ar)

## C_bt_dom (Catch by bottom trawlers within China's 'domestic waters', i.e. the four seas).
ari0<-auto.arima(df$C_bt_dom,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (0,1,0) with drift (0.007 +- 0.0023)
r0<-forecast(ari0,h=4)
df$C_bt_dom_ar<-ari0$fitted
df$C_bt_dom_se<-0
df$C_bt_dom_ar[66:69]<-r0$mean
df$C_bt_dom_se[66:69]<-(r0$mean-r0$lower[,2])/2
head(df)
df
str(df)
df$C_bt_dom_ar<-as.numeric(df$C_bt_dom_ar)

df
str(df)

#
### calculate C_bt_beyeez (BTF catch beyond the eez), C_bt_bydom (BTF catch beyond domestic water)
df$C_bt_byeez
df$C_bt_byeez[66:69]<-df$C_bt_ar[66:69]-df$C_bt_eez_ar[66:69]
df$C_bt_byeez_se<-0
df$C_bt_byeez_se[66:69]<-sqrt(df$C_bt_se[66:69]^2+df$C_bt_eez_se[66:69]^2)
str(df)
df$C_bt_bydom
df$C_bt_bydom[66:69]<-df$C_bt_ar[66:69]-df$C_bt_dom_ar[66:69]
df$C_bt_bydom_se<-0
df$C_bt_bydom_se[66:69]<-sqrt(df$C_bt_se[66:69]^2+df$C_bt_dom_se[66:69]^2)
str(df)

############################################
## V_bt (landed value of BTF)
ari0<-auto.arima(df$V_bt,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (0,1,0) with drift (0.007 +- 0.0023)
r0<-forecast(ari0,h=4)
df$V_bt_ar<-ari0$fitted
df$V_bt_se<-0
df$V_bt_ar[66:69]<-r0$mean
df$V_bt_se[66:69]<-(r0$mean-r0$lower[,2])/2
head(df)
df
str(df)
df$V_bt_ar<-as.numeric(df$V_bt_ar)
df$V_bt_se<-as.numeric(df$V_bt_se)
## V_bt_eez (landed value of BTF within China's claimed eez)
ari0<-auto.arima(df$V_bt_eez,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (0,1,0) with drift (0.007 +- 0.0023)
r0<-forecast(ari0,h=4)
df$V_bt_eez_ar<-ari0$fitted
df$V_bt_eez_se<-0
df$V_bt_eez_ar[66:69]<-r0$mean
df$V_bt_eez_se[66:69]<-(r0$mean-r0$lower[,2])/2
head(df)
df
str(df)
df$V_bt_eez_ar<-as.numeric(df$V_bt_eez_ar)

## V_bt_dom (landed value of BTF within China's 'domestic water')
ari0<-auto.arima(df$V_bt_dom,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (0,1,0) with drift (0.007 +- 0.0023)
r0<-forecast(ari0,h=4)
df$V_bt_dom_ar<-ari0$fitted
df$V_bt_dom_se<-0
df$V_bt_dom_ar[66:69]<-r0$mean
df$V_bt_dom_se[66:69]<-(r0$mean-r0$lower[,2])/2
head(df)
df
str(df)
df$V_bt_dom_ar<-as.numeric(df$V_bt_dom_ar)
df$V_bt_dom_ar
df
str(df)

#
### calculate V_bt_beyeez (BTF landed value beyond the eez), V_bt_bydom (BTF landed value beyond China's 'domestic water')
df$V_bt_byeez
df$V_bt_byeez[66:69]<-df$V_bt_ar[66:69]-df$V_bt_eez_ar[66:69]
df$V_bt_byeez_se<-0
df$V_bt_byeez_se[66:69]<-sqrt(df$V_bt_se[66:69]^2+df$V_bt_eez_se[66:69]^2)
str(df)
df$V_bt_bydom
df$V_bt_bydom[66:69]<-df$V_bt_ar[66:69]-df$V_bt_dom_ar[66:69]
df$V_bt_bydom_se<-0
df$V_bt_bydom_se[66:69]<-sqrt(df$V_bt_se[66:69]^2+df$V_bt_dom_se[66:69]^2) ## the se was too large 
str(df)
## negative lower bounds were found in V_bt_bydom

## reestimate V_bt_bydom based on arima
## df<-read.csv("CN_VpC.csv",header=TRUE,sep=',')
ari0<-auto.arima(df$VpC_bt_bydom,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (0,1,0) 
r0<-forecast(ari0,h=4)
df2$VpC_bt_bydom_ar<-ari0$fitted
df2$VpC_bt_bydom_se<-0
df2$VpC_bt_bydom_ar[66:69]<-r0$mean
df2$VpC_bt_bydom_se[66:69]<-(r0$mean-r0$lower[,2])/2
str(df2)
df2$VpC_bt_bydom_ar[66:69];df2$VpC_bt_bydom_se[66:69]
write.csv(df2,"CN_VpC.csv")
### output data
### output data
write.csv(df,"CN_bt_stat_estimate 1950-2018.csv")

################ plot bottom trawling catch
df<-read.csv("CN_bt_stat_estimate 1950-2018.csv",header=TRUE,sep=',')
#### Fig A28

par(mfrow=c(2,1),mar=c(3,5,1,0.5))
max<-max(na.omit(df$C_bt),df$C_bt_ar+1.96*df$C_bt_se)/1000000;max
min<-min(na.omit(df$C_bt))/1000000;min
plot(df$C_bt/1000000~df$year,pch=1,ylim=c(min,max),xlab="",las=1, ylab="")
lines(df$C_bt_ar/1000000~df$year,df,lty="solid",col="black")
lines((df$C_bt_ar+1.96*df$C_bt_se)/1000000~df$year,df,lty="dashed",col="black")
lines((df$C_bt_ar-1.96*df$C_bt_se)/1000000~df$year,df,lty="dashed",col="black")
par(new=T)
plot(df$C_bt_eez/1000000~df$year,pch=1,ylim=c(min,max),col="red",xlab="",las=1, ylab="",axes=F)
lines(df$C_bt_eez_ar/1000000~df$year,df,lty="solid",col="red")
lines((df$C_bt_eez_ar+1.96*df$C_bt_eez_se)/1000000~df$year,df,lty="dashed",col="red")
lines((df$C_bt_eez_ar-1.96*df$C_bt_eez_se)/1000000~df$year,df,lty="dashed",col="red")
par(new=T)
plot(df$C_bt_dom/1000000~df$year,pch=1,ylim=c(min,max),col="blue",xlab="",las=1, ylab="",axes=F)
lines(df$C_bt_dom_ar/1000000~df$year,lty="solid",col="blue")
lines((df$C_bt_dom_ar+1.96*df$C_bt_dom_se)/1000000~df$year,df,lty="dashed",col="blue")
lines((df$C_bt_dom_ar-1.96*df$C_bt_dom_se)/1000000~df$year,df,lty="dashed",col="blue")

abline(v=c(1985,1999,2013),lty="dashed",col="grey")
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,18,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0,cex=0.8) 
mtext("Catch by China's bottom trawlers\n(million tons)",side=2.5,col="black",line=2.5,cex = 1)
legend("topleft",legend=c("All waters (Mean, estimated)", "All waters (95% CI, estimated)","In China's seas (Mean, estimated)", "In China's seas (95% CI, estimated)","In China's EEZs (Mean, estimated)", "In China's EEZs (95% CI, estimated)"),
       text.col=c("black","black","blue","blue","red","red"),lty=c("solid","dashed","solid","dashed","solid","dashed"),col=c("black","black","blue","blue","red","red"))## Add Legend
mtext("a)",line=0.05,side=3,adj=0,cex=0.8)
##
max<-max(df$C_bt_byeez+1.96*df$C_bt_byeez_se,df$C_bt_bydom+1.96*df$C_bt_bydom_se)/1000000;max
min<-min(df$C_bt_byeez+1.96*df$C_bt_byeez_se,df$C_bt_bydom+1.96*df$C_bt_bydom_se)/1000000;min
plot(df$C_bt_byeez[-(66:69)]/1000000~df$year[-(66:69)],pch=2,xlim=c(1950,2020),ylim=c(min,max),xlab="",col="blue",las=1, ylab="")
lines(df$C_bt_byeez/1000000~df$year,df,lty="solid",col="blue")
lines((df$C_bt_byeez+1.96*df$C_bt_byeez_se)/1000000~df$year,df,lty="dashed",col="blue")
lines((df$C_bt_byeez-1.96*df$C_bt_byeez_se)/1000000~df$year,df,lty="dashed",col="blue")
par(new=T)
plot(df$C_bt_bydom[-(66:69)]/1000000~df$year[-(66:69)],pch=2,xlim=c(1950,2020),ylim=c(min,max),col="red",xlab="",las=1, ylab="",axes=F)
lines(df$C_bt_bydom/1000000~df$year,df,lty="solid",col="red")
lines((df$C_bt_bydom+1.96*df$C_bt_bydom_se)/1000000~df$year,df,lty="dashed",col="red")
lines((df$C_bt_bydom-1.96*df$C_bt_bydom_se)/1000000~df$year,df,lty="dashed",col="red")

mtext("Catch by China's bottom trawlers\n(million tons)",side=2,col="black",line=2.5,cex=1,las=0) 
abline(v=c(1985,1999,2013),lty="dashed",col="grey")
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,18,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0,cex=0.8) 
legend("topleft",legend=c("Beyond China's EEZs (Mean, estimated)", "Beyond China's EEZs (95% CI, estimated)","Beyond China's seas (Mean, estimated)", "Beyond China's seas (95% CI, estimated)"),
       text.col=c("blue","blue","red","red"),lty=c("solid","dashed","solid","dashed"),col=c("blue","blue","red","red"))## Add Legend
mtext("b)",side=3,line=0.05,adj=0,cex=0.8)

#

#### Fig A29
par(mfrow=c(2,1),mar=c(3,5,1,0.5))
max<-max(na.omit(df$V_bt),df$V_bt_ar+1.96*df$V_bt_se)/10000000000;max
min<-min(na.omit(df$V_bt))/10000000000;min
plot(df$V_bt/10000000000~df$year,pch=1,ylim=c(min,max),xlab="",las=1, ylab="")
lines(df$V_bt_ar/10000000000~df$year,df,lty="solid",col="black")
lines((df$V_bt_ar+1.96*df$V_bt_se)/10000000000~df$year,df,lty="dashed",col="black")
lines((df$V_bt_ar-1.96*df$V_bt_se)/10000000000~df$year,df,lty="dashed",col="black")
par(new=T)
plot(df$V_bt_eez/10000000000~df$year,pch=1,ylim=c(min,max),col="red",xlab="",las=1, ylab="",axes=F)
lines(df$V_bt_eez_ar/10000000000~df$year,df,lty="solid",col="red")
lines((df$V_bt_eez_ar+1.96*df$V_bt_eez_se)/10000000000~df$year,df,lty="dashed",col="red")
lines((df$V_bt_eez_ar-1.96*df$V_bt_eez_se)/10000000000~df$year,df,lty="dashed",col="red")
par(new=T)
plot(df$V_bt_dom/10000000000~df$year,pch=1,ylim=c(min,max),col="blue",xlab="",las=1, ylab="",axes=F)
lines(df$V_bt_dom_ar/10000000000~df$year,lty="solid",col="blue")
lines((df$V_bt_dom_ar+1.96*df$V_bt_dom_se)/10000000000~df$year,df,lty="dashed",col="blue")
lines((df$V_bt_dom_ar-1.96*df$V_bt_dom_se)/10000000000~df$year,df,lty="dashed",col="blue")

abline(v=c(1985,1999,2013),lty="dashed",col="grey")
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,3,0.1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0,cex=0.8) 
mtext("Landed value by China's bottom trawlers\n(billion $)",side=2.5,col="black",line=2.5,cex = 1)
legend("topleft",legend=c("All waters (Mean, estimated)", "All waters (95% CI, estimated)","In China's seas (Mean, estimated)", "In China's seas (95% CI, estimated)","In China's EEZs (Mean, estimated)", "In China's EEZs (95% CI, estimated)"),
       text.col=c("black","black","blue","blue","red","red"),lty=c("solid","dashed","solid","dashed","solid","dashed"),col=c("black","black","blue","blue","red","red"))## Add Legend
mtext("a)",line=0.05,side=3,adj=0,cex=0.8)
##
max<-max(df$V_bt_byeez+1.96*df$V_bt_byeez_se,df$V_bt_bydom+1.96*df$V_bt_bydom_se)/10000000000;max
min<-min(df$V_bt_byeez+1.96*df$V_bt_byeez_se,df$V_bt_bydom+1.96*df$V_bt_bydom_se)/10000000000;min
plot(df$V_bt_byeez[-(66:69)]/10000000000~df$year[-(66:69)],pch=2,xlim=c(1950,2020),ylim=c(min,max),xlab="",col="blue",las=1, ylab="")
lines(df$V_bt_byeez/10000000000~df$year,df,lty="solid",col="blue")
lines((df$V_bt_byeez+1.96*df$V_bt_byeez_se)/10000000000~df$year,df,lty="dashed",col="blue")
lines((df$V_bt_byeez-1.96*df$V_bt_byeez_se)/10000000000~df$year,df,lty="dashed",col="blue")
par(new=T)
plot(df$V_bt_bydom[-(66:69)]/10000000000~df$year[-(66:69)],pch=2,xlim=c(1950,2020),ylim=c(min,max),col="red",xlab="",las=1, ylab="",axes=F)
lines(df$V_bt_bydom/10000000000~df$year,df,lty="solid",col="red")
lines((df$V_bt_bydom+1.96*df$V_bt_bydom_se)/10000000000~df$year,df,lty="dashed",col="red")
lines((df$V_bt_bydom-1.96*df$V_bt_bydom_se)/10000000000~df$year,df,lty="dashed",col="red")

mtext("Landed value by China's bottom trawlers\n(billion $)",side=2,col="black",line=2.5,cex=1,las=0) 
abline(v=c(1985,1999,2013),lty="dashed",col="grey")
ticks = seq(1950,2018,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,2,0.1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0,cex=0.8) 
legend("topleft",legend=c("Beyond China's EEZs (Mean, estimated)", "Beyond China's EEZs (95% CI, estimated)","Beyond China's seas (Mean, estimated)", "Beyond China's seas (95% CI, estimated)"),
       text.col=c("blue","blue","red","red"),lty=c("solid","dashed","solid","dashed"),col=c("blue","blue","red","red"))## Add Legend
mtext("b)",side=3,line=0.05,adj=0,cex=0.8)

#
########## Estimate catch and landed value for other fishing vessels
setwd("C:/Users/xiong/Desktop/MS_history of BTF China")
### arima
df<-read.csv("CN_c_of.csv",header=TRUE,sep=',')
library(forecast)
ari0<-auto.arima(df[,2],trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (0,1,0) with drift (0.007 +- 0.0023)
r0<-forecast(ari0,h=4)
r0
df$C_of_ar<-ari0$fitted
df$C_of_se<-0
df$C_of_ar[66:69]<-r0$mean
df$C_of_se[66:69]<-(r0$mean-r0$lower[,2])/2
head(df)
df
ari0<-auto.arima(df[,3],trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (0,1,0) with drift (0.007 +- 0.0023)
r0<-forecast(ari0,h=4)
r0
df$V_of_ar<-ari0$fitted
df$V_of_se<-0
df$V_of_ar[66:69]<-r0$mean
df$V_of_se[66:69]<-(r0$mean-r0$lower[,2])/2
head(df)
df
write.csv(df,"CN_of.csv")

## Fig A30
par(mfrow=c(2,1))
str(df)
max<-max(na.omit(df$C_of_ar+1.96*df$C_of_se))/1000000;max
min<-min(na.omit(df$C_of_ar+1.96*df$C_of_se))/1000000;min
plot(df$C_of[-(66:69)]/1000000~df$year[-(66:69)],pch=1,xlim=c(1950,2020),ylim=c(min,max),col="black",xlab="",las=1, ylab="")
lines(df$C_of_ar/1000000~df$year,lty="solid",col="black")
lines((df$C_of_ar+1.96*df$C_of_se)/1000000~df$year,df,lty="dashed",col="black")
lines((df$C_of_ar-1.96*df$C_of_se)/1000000~df$year,df,lty="dashed",col="black")
mtext("Catch by China's marine catchers\nexcluding bottom trawlers (million tons)",side=2,col="black",line=2.5,cex=0.8,las=0) 
abline(v=c(1980,1992,2000,2013),lty="dashed",col="grey")
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,9,1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0,cex=0.8) 
legend("topleft",legend=c("Mean", "95% confidence intervals"),
       text.col=c("black","black"),lty=c("solid","dashed"),col=c("black","black"))## Add Legend
mtext("a)",side=3,line=0.05,adj=0,cex=0.8)

max<-max(na.omit(df$V_of_ar+1.96*df$V_of_se))/10000000000;max
min<-min(na.omit(df$V_of_ar+1.96*df$V_of_se))/10000000000;min
plot(df$V_of[-(66:69)]/10000000000~df$year[-(66:69)],pch=1,xlim=c(1950,2020),ylim=c(min,max),col="black",xlab="",las=1, ylab="")
lines(df$V_of_ar/10000000000~df$year,lty="solid",col="black")
lines((df$V_of_ar+1.96*df$V_of_se)/10000000000~df$year,df,lty="dashed",col="black")
lines((df$V_of_ar-1.96*df$V_of_se)/10000000000~df$year,df,lty="dashed",col="black")
mtext("Landed value by China's marine catchers \nexcluding bottom trawlers (10 billion $)",side=2,col="black",line=2.5,cex=0.8,las=0) 
abline(v=c(1975,1992,2003,2013),lty="dashed",col="grey")
ticks = seq(1950,2020,1)
axis(side = 1, at = ticks,tck=-0.01,labels = NA)
ticks = seq(0,1.5,0.1)
axis(side = 2, at = ticks,tck=-0.01,labels = NA)
mtext("Year",side=1,col="black",line=2,las=0,cex=0.8) 
legend("topleft",legend=c("Mean", "95% confidence intervals"),
       text.col=c("black","black"),lty=c("solid","dashed"),col=c("black","black"))## Add Legend
mtext("b)",side=3,line=0.05,adj=0,cex=0.8)

