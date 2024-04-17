#rm(list = ls())
library(dplyr)
library(tidyverse)
library(xtable)
library(here)
library(coin)

d<-read.csv(here("detf_new.csv"),sep=",",header=T, stringsAsFactors = FALSE)
#i==67
do<-d %>% filter(i>0, r>0)  %>% select(sess,tre,r,i,activo,bb,bo,z)
dw<-do %>%
  pivot_wider(names_from = activo, values_from = c(bb,bo,z))

dw$sp1<-dw$bo_1-dw$bb_1
dw$sp2<-dw$bo_2-dw$bb_2
dw$sp3<-dw$bo_3-dw$bb_3
dw$sp4<-dw$bo_4-dw$bb_4

dw$bo32<-dw$bb_3-dw$bo_2

dw<- dw %>% group_by(sess, tre)  %>%
  summarize(sp1= mean(sp1,na.rm=T),sp2= mean(sp2,na.rm=T),sp3= mean(sp3,na.rm=T),sp4= mean(sp4,na.rm=T), 
            z1= mean(z_1,na.rm=T),z2= mean(z_2,na.rm=T),z3= mean(z_3,na.rm=T),z4= mean(z_4,na.rm=T),bo32= mean(bo32,na.rm=T))

save(dw,file="spreads_session.Rda")

mean(dw$sp2[dw$tre==1],na.rm=T)
mean(dw$sp2[dw$tre==2],na.rm=T)
mean(dw$sp2[dw$tre==3],na.rm=T)

mean(dw$z2[dw$tre==1],na.rm=T)
mean(dw$z2[dw$tre==2],na.rm=T)
mean(dw$z2[dw$tre==3],na.rm=T)
##
#Test Spread c treatment 2 v. 3 
sprc_23<-as.data.frame(cbind(c(dw$sp3[dw$tre==2],dw$sp3[dw$tre==3]),c(rep("one",5),rep("two",5))))
names(sprc_23)<-c("values","tre")
sprc_23$values<-as.numeric(sprc_23$values)
sprc_23$tre<-as.factor(sprc_23$tre)
oneway_test(values~tre,data=sprc_23)

#Test Spread c treatment 1 v. 2 
sprc_23<-as.data.frame(cbind(c(dw$sp3[dw$tre==2],dw$sp3[dw$tre==1]),c(rep("one",5),rep("two",5))))
names(sprc_23)<-c("values","tre")
sprc_23$values<-as.numeric(sprc_23$values)
sprc_23$tre<-as.factor(sprc_23$tre)
oneway_test(values~tre,data=sprc_23)

#Test Spread A treatment 1 v. 2 
sprc_23<-as.data.frame(cbind(c(dw$sp1[dw$tre==2],dw$sp1[dw$tre==1]),c(rep("one",5),rep("two",5))))
names(sprc_23)<-c("values","tre")
sprc_23$values<-as.numeric(sprc_23$values)
sprc_23$tre<-as.factor(sprc_23$tre)
oneway_test(values~tre,data=sprc_23)

#Test Spread B treatment 1 v. 2 
sprc_23<-as.data.frame(cbind(c(dw$sp2[dw$tre==2],dw$sp2[dw$tre==1]),c(rep("one",5),rep("two",5))))
names(sprc_23)<-c("values","tre")
sprc_23$values<-as.numeric(sprc_23$values)
sprc_23$tre<-as.factor(sprc_23$tre)
oneway_test(values~tre,data=sprc_23)


##
#Test Spread ETF treatment 2 v. 3 
spre_23<-as.data.frame(cbind(c(dw$sp4[dw$tre==2],dw$sp4[dw$tre==3]),c(rep("one",5),rep("two",5))))
names(spre_23)<-c("values","tre")
spre_23$values<-as.numeric(spre_23$values)
spre_23$tre<-as.factor(spre_23$tre)
oneway_test(values~tre,data=spre_23)

#Test Spread ETF treatment 2 v. 3 
sprb_23<-as.data.frame(cbind(c(dw$sp2[dw$tre==2],dw$sp2[dw$tre==3]),c(rep("one",5),rep("two",5))))
names(sprb_23)<-c("values","tre")
sprb_23$values<-as.numeric(sprb_23$values)
sprb_23$tre<-as.factor(sprb_23$tre)
oneway_test(values~tre,data=sprb_23)

#i==67 To replicate D.2  use i==67 
dd<- d %>% filter(r>0, i==67) %>% group_by(sess,activo, tre)  %>%
  summarize(z = mean(z), bb=mean(bb,na.rm=T), bo=mean(bo,na.rm=T))


etf12<-as.data.frame(cbind(c(rep(0,5),dd$bo[dd$activo==3 & dd$tre==3]-dd$bo[dd$activo==2 & dd$tre==3]),c(rep("one",5),rep("two",5))))
names(etf12)<-c("values","tre")
etf12$values<-as.numeric(etf12$values)
etf12$tre<-as.factor(etf12$tre)
oneway_test(values~tre,data=etf12)


t1<-dd$z[dd$activo==3 & dd$tre==1]-dd$z[dd$activo==2 & dd$tre==1]
t2<-dd$z[dd$activo==3 & dd$tre==2]-dd$z[dd$activo==2 & dd$tre==2]
t3<-dd$z[dd$activo==3 & dd$tre==3]-dd$z[dd$activo==2 & dd$tre==3]

t1<-dd$z[dd$activo==4 & dd$tre==1]
t2<-dd$z[dd$activo==4 & dd$tre==2]


#Test order imbalance 2 v. 1 
etf12<-as.data.frame(cbind(c(t2,t1),c(rep("one",5),rep("two",5))))
names(etf12)<-c("values","tre")
etf12$values<-as.numeric(etf12$values)
etf12$tre<-as.factor(etf12$tre)
oneway_test(values~tre,data=etf12)

#Test order imbalance 3 v. 1 
z31<-as.data.frame(cbind(c(t3,t1),c(rep("one",5),rep("two",5))))
names(z31)<-c("values","tre")
z31$values<-as.numeric(z31$values)
z31$tre<-as.factor(z31$tre)
oneway_test(values~tre,data=z31)


#Test order imbalance ETF
tetf1<-as.data.frame(cbind(c(dd$z[dd$activo==4], rep(0,15)),c(rep("one",15),rep("two",15))))
names(tetf1)<-c("values","tre")
tetf1$values<-as.numeric(tetf1$values)
tetf1$tre<-as.factor(tetf1$tre)
oneway_test(values~tre,data=tetf1)

#Test order imbalance A
ta<-as.data.frame(cbind(c(dd$z[dd$activo==1], rep(0,15)),c(rep("one",15),rep("two",15))))
names(ta)<-c("values","tre")
ta$values<-as.numeric(ta$values)
ta$tre<-as.factor(ta$tre)
oneway_test(values~tre,data=ta)

#Test order imbalance B
tb<-as.data.frame(cbind(c(dd$z[dd$activo==2], rep(0,15)),c(rep("one",15),rep("two",15))))
names(tb)<-c("values","tre")
tb$values<-as.numeric(tb$values)
tb$tre<-as.factor(tb$tre)
oneway_test(values~tre,data=tb)

#Test order imbalance C
tc<-as.data.frame(cbind(c(dd$z[dd$activo==3], rep(0,15)),c(rep("one",15),rep("two",15))))
names(tc)<-c("values","tre")
tc$values<-as.numeric(tc$values)
tc$tre<-as.factor(tc$tre)
oneway_test(values~tre,data=tc)


z1<-tapply(d$z[d$activo==1],d$tre[d$activo==1],mean,na.rm=T)
z2<-tapply(d$z[d$activo==2],d$tre[d$activo==2],mean,na.rm=T)
z3<-tapply(d$z[d$activo==3],d$tre[d$activo==3],mean,na.rm=T)
z4<-tapply(d$z[d$activo==4],d$tre[d$activo==4],mean,na.rm=T)

#Create Table D.2
bb1<-tapply(dd$bb[dd$activo==1],dd$tre[dd$activo==1],mean,na.rm=T)
bo1<-tapply(dd$bo[dd$activo==1],dd$tre[dd$activo==1],mean,na.rm=T)
bb2<-tapply(dd$bb[dd$activo==2],dd$tre[dd$activo==2],mean,na.rm=T)
bo2<-tapply(dd$bo[dd$activo==2],dd$tre[dd$activo==2],mean,na.rm=T)
bb3<-tapply(dd$bb[dd$activo==3],dd$tre[dd$activo==3],mean,na.rm=T)
bo3<-tapply(dd$bo[dd$activo==3],dd$tre[dd$activo==3],mean,na.rm=T)
bb4<-tapply(dd$bb[dd$activo==4],dd$tre[dd$activo==4],mean,na.rm=T)
bo4<-tapply(dd$bo[dd$activo==4],dd$tre[dd$activo==4],mean,na.rm=T)

xtable(round(rbind(cbind(t(bo1),t(bb1)),cbind(t(bo2),t(bb2)),cbind(t(bo3),t(bb3)),cbind(t(bo4),t(bb4)),0)))

#test
tb<-as.data.frame(cbind(c(dd$bb[dd$tre==1 & dd$activo==3]-dd$bo[dd$tre==1 & dd$activo==2 ], rep(0,5)),c(rep("one",5),rep("two",5))))
names(tb)<-c("values","tre")
tb$values<-as.numeric(tb$values)
tb$tre<-as.factor(tb$tre)
oneway_test(values~tre,data=tb)

d$spre<- d$bo-d$bb

spre1<-tapply(d$spre[d$activo==1],d$tre[d$activo==1],mean,na.rm=T)
spre2<-tapply(d$spre[d$activo==2],d$tre[d$activo==2],mean,na.rm=T)
spre3<-tapply(d$spre[d$activo==3],d$tre[d$activo==3],mean,na.rm=T)
spre4<-tapply(d$spre[d$activo==4],d$tre[d$activo==4],mean,na.rm=T)

##
##CReate Table 5 
xtable(round(rbind(z1,z2,z3,z4),2))
xtable(round(rbind(spre1,spre2,spre3,spre4),2))
##


spreads<-NA
spreads2<-NA
bb_2<-NA
bb_3<-NA

ba_2<-NA
ba_3<-NA

for(t in c(67:67)){
  d_bids <- d %>% filter(i==t)%>%
    select(r,sess,tre,bb,activo)
  
  d_wb = d_bids %>% 
    spread(activo, bb)
  
  names(d_wb)[names(d_wb) == "1"] <- "bb_1"
  names(d_wb)[names(d_wb) == "2"] <- "bb_2"
  names(d_wb)[names(d_wb) == "3"] <- "bb_3"
  names(d_wb)[names(d_wb) == "4"] <- "bb_4"
  
  d_asks <- d %>% filter(i==t)%>%
    select(r,sess,tre,bo,activo)
  
  d_wa = d_asks %>% 
    spread(activo, bo)
  
  names(d_wa)[names(d_wa) == "1"] <- "bo_1"
  names(d_wa)[names(d_wa) == "2"] <- "bo_2"
  names(d_wa)[names(d_wa) == "3"] <- "bo_3"
  names(d_wa)[names(d_wa) == "4"] <- "bo_4"
  
  d_bo = inner_join(d_wb,d_wa ,by.x=c("r", "sess","tre"),by.y=c("r", "sess","tre"))

  d_bo$b3a2 <-d_bo$bb_3-d_bo$bo_2
  d_bo$a3b2 <-d_bo$bo_3-d_bo$bb_2
  bb_2 = rbind(bb_2,tapply(d_bo$bb_2,d_bo$tre,mean,na.rm=T))
  bb_3 = rbind(bb_3,tapply(d_bo$bb_3,d_bo$tre,mean,na.rm=T))
  ba_2 = rbind(ba_2,tapply(d_bo$bo_2,d_bo$tre,mean,na.rm=T))
  ba_3 = rbind(ba_3,tapply(d_bo$bo_3,d_bo$tre,mean,na.rm=T))
  
  spreads = rbind(spreads,tapply(d_bo$b3a2,d_bo$tre,mean,na.rm=T))
  spreads2 = rbind(spreads2,tapply(d_bo$a3b2,d_bo$tre,mean,na.rm=T))
  
}

table_spr<-rbind(ba_2,ba_3,bb_2,bb_3)
table_spr<-table_spr[!is.na(table_spr[,1]),]
table_spr<-rbind(table_spr,table_spr[4,]-table_spr[1,])

xtable(table_spr)

table_vender <- apply(spreads,2,mean,na.rm=T)
xtable(as.data.frame(t(table_vender)))


table_vender2 <- apply(spreads2,2,mean,na.rm=T)
xtable(as.data.frame(t(table_vender2)))


t1 = tapply(d_bo$b3a2[d_bo$tre==1],d_bo$sess[d_bo$tre==1],mean,na.rm=T)
t2 = tapply(d_bo$b3a2[d_bo$tre==2],d_bo$sess[d_bo$tre==2],mean,na.rm=T)
t3 = tapply(d_bo$b3a2[d_bo$tre==3],d_bo$sess[d_bo$tre==3],mean,na.rm=T)

tt1 = tapply(d_bo$bo_3[d_bo$tre==1]-d_bo$bo_2[d_bo$tre==1],d_bo$sess[d_bo$tre==1],mean,na.rm=T)
tt2 = tapply(d_bo$bo_3[d_bo$tre==2]-d_bo$bo_2[d_bo$tre==2],d_bo$sess[d_bo$tre==2],mean,na.rm=T)
tt3 = tapply(d_bo$bo_3[d_bo$tre==3]-d_bo$bo_2[d_bo$tre==3],d_bo$sess[d_bo$tre==3],mean,na.rm=T)

tt2a= tapply(d_bo$bo_2[d_bo$tre==2],d_bo$sess[d_bo$tre==2],mean,na.rm=T)
tt3a= tapply(d_bo$bo_3[d_bo$tre==2],d_bo$sess[d_bo$tre==2],mean,na.rm=T)

ttt2a= tapply(d_bo$bo_2[d_bo$tre==3],d_bo$sess[d_bo$tre==3],mean,na.rm=T)
ttt3a= tapply(d_bo$bo_3[d_bo$tre==3],d_bo$sess[d_bo$tre==3],mean,na.rm=T)


