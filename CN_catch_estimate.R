############################ Reconstructing catch by China's BTF and related analyses, based on timeseries statistics from SAUP datasets
## install packages
install.packages('data.table','reshape2','ggplot2','forecast')
## download the dataset from Sea Around Us Project by fishing entity = China
## Correct the data in excel according to the the methodology in "Reconstructing fishing capacity and landings of China's bottom trawl fisheries (1950 - 2018)", by Xiong Zhang & Amanda C.J. Vincent, published as a working paper by Institute for the Oceans and Fisheries, UBC.
## This includes: 1) correct functional groups of several taxa that were wrongly identified for China's BTF catch; 2) corrected the reconstruction for the early period between 1950 and 1984, when China's fishing fleets did not yet fish in waters beyond China's four seas.

######### Section 1: BTF catch from C4S
# download catch datasets from 'LME' = Yellow Sea, East China Sea, and South China Sea, respectively
# unpack the file and stored it in a local space, here "C:/Users/xiong/Desktop/BTF_China/SAU C4S"
### Section 1: extract BTF catch data for China's seas
setwd("C:/Users/xiong/Desktop/BTF_China")
df1<-read.csv("SAU C4S/SAU LME 36 v48-0.csv",header=TRUE,sep=',')
df2<-read.csv("SAU C4S/SAU LME 47 v48-0.csv",header=TRUE,sep=',')
df3<-read.csv("SAU C4S/SAU LME 48 v48-0.csv",header=TRUE,sep=',')
str(df1);str(df2);str(df3)
df<-rbind(df1,df2,df3)
str(df)
library(data.table)
bt_df<-subset(df,(df[,12]=="bottom trawl") & (df[,8]=="China"))[,c(3,14)];str(bt_df)

## integrate data by year
#subset data
tm<-0
j<-0
vm<-0
head(bt_df)
for(i in 1:67){
  j=1949+i
  tm[i]<-sum(subset(bt_df[,2],bt_df[,1]==j))
}
y<-seq(1950,2016,1)
tm
df<-data.frame(year=y,C_bt_dom=tm)
head(df)
write.csv(df,"CN_bt_dom_stat.csv",row.names = F) ## output
## extrapolate based on ARIMA
## mannually add reported catch by Chinese trawlers (2007 - 2018) to the above CN_bt_dom_stat, and extended the year column value to 2018
df<-read.csv("CN_bt_dom_stat.csv",header=TRUE,sep=',')
df$Ratio<-df$C_bt_dom/df$C_bt_r; df$Ratio
library(forecast)
## arima for Ratio
ari0<-auto.arima(df$Ratio,trace = T) # change N_YL to H_YL for create model for the latter
summary(ari0) # ARIMA (0,1,0) 
r0<-forecast(ari0,h=2)
r0$upper[,2]
## gam model for Ratio
library(mgcv)
gam1<-gam(Ratio~s(year),data=df, select=TRUE, method='GCV.Cp');BIC(gam1)
summary(gam1)
plot(gam1,shade = T)
pred<-predict(gam1,df[68:69,],se=T)
pred
df$Ratio[68:69]<-pred$fit
df$Ratio_se[68:69]<-pred$se.fit
df
df$C_bt_dom[68:69]<-df$C_bt_r[68:69]*df$Ratio[68:69]
df$C_bt_dom_se[68:69]<-df$Ratio_se[68:69]*df$C_bt_r[68:69]
## gam model for Ratio
df$Ratio<-df$C_bt_eez/df$C_bt_r; df$Ratio
library(mgcv)
gam1<-gam(Ratio~s(year),data=df, select=TRUE, method='GCV.Cp');BIC(gam1)
summary(gam1)
plot(gam1,shade = T)
pred<-predict(gam1,df[68:69,],se=T)
pred
df$Ratio[68:69]<-pred$fit
df$Ratio_se[68:69]<-pred$se.fit
df
df$C_bt_dom[68:69]<-df$C_bt_r[68:69]*df$Ratio[68:69]
df$C_bt_dom_se[68:69]<-df$Ratio_se[68:69]*df$C_bt_r[68:69]
write.csv(df,"CN_bt_dom_stat2.csv",row.names = F) ## output

##### Section 2: BTF catch from China's EEZ
setwd("C:/Users/xiong/Desktop/BTF_China")
df4<-read.csv("SAU China/SAU FishingEntity 31 v48-0.csv",header=TRUE,sep=',');str(df4)
df4<-subset(df4,(df4$area_name=="China")&(df4$gear_type=="bottom trawl"));str(df4)
df4<-df4[,-2];str(df4)
write.csv(df4,"CN_eez_catch 2020.csv",row.names = F)
## create bottom-trawler's data by year in China's eez
df<-df4
str(df)
# unique name and functional group
sn<-unique(df$scientific_name)
sn
write.csv(sn,"CN_sci_names2.csv",row.names = F)

fg<-unique(df$functional_group)
fg
write.csv(fg,"CN_function_group2.csv",row.names = F)

##### Section 3: estimate BTF catch for each functional group in China's EEZ
## before read the data, you need to correct the functional group accordingly as mentioned in the methodology (reclassify them to seven groups: large fish, medium fish, cephelopod, shrimps, lobsters & crabs, jellyfish, small fish & other invertebrates)
## for all catch
df<-read.csv("CN_eez_catch 2020.csv",header=TRUE,sep=',')
str(df)
df<-df[c(2,5,12)]
library(data.table)
tm1<-0
j<-0
for(i in 1:67){
  j=1949+i
  tm1[i]<-sum(subset(df[,3],df[,1]==j))
}
y<-seq(1950,2016,1)
head(tm1)
## for large benthic and benthopelagic fish
large_df<-df[df[,2] %like% "Large",]
head(large_df)
large_tm1<-0
j<-0
for(i in 1:67){
  j=1949+i
  large_tm1[i]<-sum(subset(large_df[,3],large_df[,1]==j))
}

## for medium benthic and benthopelagic fish
library(data.table)
medium_df<-df[df[,2] %like% "Medium",]
head(medium_df)
summary(medium_df)
medium_tm1<-0
j<-0
for(i in 1:67){
  j=1949+i
  medium_tm1[i]<-sum(subset(medium_df[,3],medium_df[,1]==j))
}

## for Cephalopods 
# subset data
library(data.table)
ceph_df<-df[df[,2] %like% "Cephalopod",]
head(ceph_df)
ceph_tm1<-0
j<-0
for(i in 1:67){
  j=1949+i
  ceph_tm1[i]<-sum(subset(ceph_df[,3],ceph_df[,1]==j))
}

## for shrimps
df2<-df[df[,2] %like% "Shrimp",]
head(df2)
shri_tm1<-0
j<-0
for(i in 1:67){
  j=1949+i
  shri_tm1[i]<-sum(subset(df2[,3],df2[,1]==j))
}

## for lobsters & crabs

df3<-df[df[,2] %like% "Lobster",]
head(df3)
lob_tm1<-0
lob_vm1<-0
j<-0
for(i in 1:67){
  j=1949+i
  lob_tm1[i]<-sum(subset(df3[,3],df3[,1]==j))
  lob_vm1[i]<-sum(subset(df3[,4],df3[,1]==j))
}

## jellyfish
str(df)
df3<-df[df[,2] %like% "Jellyfish",]
head(df3)
jell_tm1<-0
j<-0
for(i in 1:67){
  j=1949+i
  jell_tm1[i]<-sum(subset(df3[,3],df3[,1]==j))
}

## small fish & other invertebrates
str(df2)
df31<-rbind(df[df[,2] %like% "Small",],df[df[,2] %like% "Other",])
str(df31)
head(df31)
trash_tm1<-0
j<-0
for(i in 1:67){
  j=1949+i
  trash_tm1[i]<-sum(subset(df31[,3],df31[,1]==j))
}

df0<-data.frame(year=y,RC_larg=large_tm1/tm1,RC_med=medium_tm1/tm1,RC_ceph=ceph_tm1/tm1,
                RC_shri=shri_tm1/tm1,RC_lob=lob_tm1/tm1,RC_jell=jell_tm1/tm1,RC_tras=trash_tm1/tm1)
str(df0)
head(df0)
write.csv(df0,"CN_bt_fungroup.csv",row.names = F)

##### Section 4: estimate MTL for BTF catch in China's EEZ
##### Calculate and plot mean trophic level ~ year
setwd("C:/Users/xiong/Desktop/BTF_China")
df<-read.csv("CN_eez_catch 2020.csv",header=TRUE,sep=',')
df2<-read.csv("CN_troplevl.csv",header=TRUE,sep=',')
df3<-read.csv("China_TL.csv",header=TRUE,sep=',')
str(df)
## select data
df<-df[c(2,3,12)]
df3<-df3[c(1,6)]
str(df);str(df3);str(df2)

df2$TL
MTL<-0
j<-0
dfc<-0
dft<-0
for(i in 1:67){
  j=1949+i
  dfs<-subset(df[,c(2,3)],df[,1]==j)
  for(m in 1:length(unique(dfs[,1]))){
    dfc[m]<-sum(subset(dfs[,2],dfs[,1]==unique(dfs[,1])[m]))
    dft[m]<-df2[,2][df2[,1]==unique(dfs[,1])[m]]
  }
  MTL[i]<-sum(dfc*dft)/sum(dfc)
}
MTL
str(df3)
df3$MTL<-MTL
## select data for human consumption only
library(data.table)
df<-read.csv("CN_eez_catch 2020.csv",header=TRUE,sep=',')
str(df)
df_h<-df[df[,11] %like% "Direct",];str(df_h)
df<-df_h[c(2,3,12)];str(df)
str(df2)
MTL<-0
j<-0
dfc<-0
dft<-0
for(i in 1:67){
  j=1949+i
  dfs<-subset(df[,c(2,3)],df[,1]==j)
  for(m in 1:length(unique(dfs[,1]))){
    dfc[m]<-sum(subset(dfs[,2],dfs[,1]==unique(dfs[,1])[m]))
    dft[m]<-df2[,2][df2[,1]==unique(dfs[,1])[m]]
  }
  MTL[i]<-sum(dfc*dft)/sum(dfc)
}
MTL
## save MTL to df3
str(df3)
df3$MTL2<-MTL
write.csv(df3,"CN_MTL2.csv",row.names = F)

