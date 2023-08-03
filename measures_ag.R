rm(list = ls())
library(dplyr)
library(tidyverse)
library(xtable)
library(here)
library(coin)

load("~/Desktop/jotarepos/capmout/clean/alltrades.Rda")

load(here("holdings/holdings.Rda"))
load(here("spreads_session.Rda"))

dd[["isBot"]] <- grepl("Bot", dd[["make_pcode"]]) | grepl("Bot", dd[["take_pcode"]])
dd[["maker_isBot"]] <- grepl("Bot", dd[["make_pcode"]])
dd[["taker_isBot"]] <- grepl("Bot", dd[["take_pcode"]])

write.csv(dd,file="dtrades.csv",row.names=FALSE)


d<- dd %>% filter(round>0) %>% group_by(session,asset, tre)  %>%
  summarize(precio = mean(ex_price))

db<- dd %>% filter(round>0)  %>%  group_by(session,asset, tre,isBot) %>% 
  summarize(precio = mean(ex_price))

dp<-d %>%
  pivot_wider(names_from = asset, values_from = c(precio))
names(dp)<-c("sess","tre","p1","p2","p3","p4")

da<-left_join(dp, dw, by = c("sess","tre"))

cor.test(da$p3[da$tre==2]-da$p2[da$tre==2], da$sp3[da$tre==2], method = 'spearman')

t1<-d$precio[d$asset==3 & d$tre==1]-d$precio[d$asset==2 & d$tre==1]
t2<-d$precio[d$asset==3 & d$tre==2]-d$precio[d$asset==2 & d$tre==2]
t3<-d$precio[d$asset==3 & d$tre==3]-d$precio[d$asset==2 & d$tre==3]

etf_1<-d$precio[d$asset==4 & d$tre==1]
etf_2<-d$precio[d$asset==4 & d$tre==2]
etf_3<-d$precio[d$asset==4 & d$tre==3]

etf12<-as.data.frame(cbind(c(etf_1,etf_2),c(rep("one",5),rep("two",5))))
#etf12<-as.data.frame(cbind(c(etf_3,rep(200,5)),c(rep("one",5),rep("two",5))))

# Index premium test 1 v. 2 
rho12<-as.data.frame(cbind(c(t2,t1),c(rep("one",5),rep("two",5))))
names(rho12)<-c("values","tre")
rho12$values<-as.numeric(rho12$values)
rho12$tre<-as.factor(rho12$tre)
oneway_test(values~tre,data=rho12)

# Index premium test 1 v. 3
rho13<-as.data.frame(cbind(c(t3,t1),c(rep("one",5),rep("two",5))))
names(rho13)<-c("values","tre")
rho13$values<-as.numeric(rho13$values)
rho13$tre<-as.factor(rho13$tre)
oneway_test(values~tre,data=rho13)

# price of a in treatment 1 v. 3
a13<-as.data.frame(cbind(c(d$precio[d$asset==1 & d$tre==3],d$precio[d$asset==1 & d$tre==1]),c(rep("one",5),rep("two",5))))
names(a13)<-c("values","tre")
a13$values<-as.numeric(a13$values)
a13$tre<-as.factor(a13$tre)
oneway_test(values~tre,data=a13)


# price of a in treatment 1 v. 2
a12<-as.data.frame(cbind(c(d$precio[d$asset==1 & d$tre==2],d$precio[d$asset==1 & d$tre==1]),c(rep("one",5),rep("two",5))))
names(a12)<-c("values","tre")
a12$values<-as.numeric(a12$values)
a12$tre<-as.factor(a12$tre)
oneway_test(values~tre,data=a12)

# price of a in treatment 3 v. 2
a23<-as.data.frame(cbind(c(d$precio[d$asset==1 & d$tre==2],d$precio[d$asset==1 & d$tre==3]),c(rep("one",5),rep("two",5))))
names(a23)<-c("values","tre")
a23$values<-as.numeric(a23$values)
a23$tre<-as.factor(a23$tre)
oneway_test(values~tre,data=a23)

# price of etf in treatment 3 v. 2
etf23<-as.data.frame(cbind(c(d$precio[d$asset==4 & d$tre==2],d$precio[d$asset==4 & d$tre==3]),c(rep("one",5),rep("two",5))))
names(etf23)<-c("values","tre")
etf23$values<-as.numeric(etf23$values)
etf23$tre<-as.factor(etf23$tre)
oneway_test(values~tre,data=etf23)

etf12<-as.data.frame(cbind(c(d$precio[d$asset==4 & d$tre==2],d$precio[d$asset==4 & d$tre==1]),c(rep("one",5),rep("two",5))))
names(etf12)<-c("values","tre")
etf12$values<-as.numeric(etf12$values)
etf12$tre<-as.factor(etf12$tre)
oneway_test(values~tre,data=etf12)



# price of etf against FV
etf2<-as.data.frame(cbind(c(d$precio[d$asset==4 & d$tre==2],rep(200,5)),c(rep("one",5),rep("two",5))))
names(etf2)<-c("values","tre")
etf2$values<-as.numeric(etf2$values)
etf2$tre<-as.factor(etf2$tre)
oneway_test(values~tre,data=etf2)

etf1<-as.data.frame(cbind(c(d$precio[d$asset==4 & d$tre==1],rep(200,5)),c(rep("one",5),rep("two",5))))
names(etf1)<-c("values","tre")
etf1$values<-as.numeric(etf1$values)
etf1$tre<-as.factor(etf1$tre)
oneway_test(values~tre,data=etf1)

etf3<-as.data.frame(cbind(c(d$precio[d$asset==4 & d$tre==3],rep(200,5)),c(rep("one",5),rep("two",5))))
names(etf3)<-c("values","tre")
etf3$values<-as.numeric(etf3$values)
etf3$tre<-as.factor(etf3$tre)
oneway_test(values~tre,data=etf3)

nav_1<-db$precio[db$asset==1 & db$tre==1 & db$isBot==FALSE]+db$precio[db$asset==2 & db$tre==1 & db$isBot==FALSE]+db$precio[db$asset==3 & db$tre==1 & db$isBot==FALSE]
nav_2<-db$precio[db$asset==1 & db$tre==2& db$isBot==FALSE]+2*db$precio[db$asset==3 & db$tre==2& db$isBot==FALSE]
nav_3<-db$precio[db$asset==1 & db$tre==3& db$isBot==FALSE]+2*db$precio[db$asset==3 & db$tre==3& db$isBot==FALSE]

#Test ETF vs. NAV in secondary markets 
tnav3<-as.data.frame(cbind(c(db$precio[db$asset==4 & db$tre==3 & db$isBot==TRUE]-nav_3,rep(0,5)),c(rep("one",5),rep("two",5))))
names(tnav3)<-c("values","tre")
tnav3$values<-as.numeric(tnav3$values)
tnav3$tre<-as.factor(tnav3$tre)
oneway_test(values~tre,data=tnav3)

tnav2<-as.data.frame(cbind(c(db$precio[db$asset==4 & db$tre==2 & db$isBot==TRUE]-nav_2,rep(0,5)),c(rep("one",5),rep("two",5))))
names(tnav2)<-c("values","tre")
tnav2$values<-as.numeric(tnav2$values)
tnav2$tre<-as.factor(tnav2$tre)
oneway_test(values~tre,data=tnav2)

tnav1<-as.data.frame(cbind(c(db$precio[db$asset==4 & db$tre==1 & db$isBot==TRUE]-nav_1,rep(0,5)),c(rep("one",5),rep("two",5))))
names(tnav1)<-c("values","tre")
tnav1$values<-as.numeric(tnav1$values)
tnav1$tre<-as.factor(tnav1$tre)
oneway_test(values~tre,data=tnav1)


dd$unidad<-1
p1<-tapply(dd$ex_price[dd$asset==1],dd$tre[dd$asset==1],mean,na.rm=T)
p2<-tapply(dd$ex_price[dd$asset==2],dd$tre[dd$asset==2],mean,na.rm=T)
p3<-tapply(dd$ex_price[dd$asset==3],dd$tre[dd$asset==3],mean,na.rm=T)
p4<-tapply(dd$ex_price[dd$asset==4],dd$tre[dd$asset==4],mean,na.rm=T)

p1sd<-tapply(dd$ex_price[dd$asset==1],dd$tre[dd$asset==1],sd,na.rm=T)
p2sd<-tapply(dd$ex_price[dd$asset==2],dd$tre[dd$asset==2],sd,na.rm=T)
p3sd<-tapply(dd$ex_price[dd$asset==3],dd$tre[dd$asset==3],sd,na.rm=T)
p4sd<-tapply(dd$ex_price[dd$asset==4],dd$tre[dd$asset==4],sd,na.rm=T)

p1_bot<-tapply(dd$ex_price[dd$asset==1 & dd$isBot==1],dd$tre[dd$asset==1& dd$isBot==1],mean,na.rm=T)
p2_bot<-tapply(dd$ex_price[dd$asset==2& dd$isBot==1],dd$tre[dd$asset==2& dd$isBot==1],mean,na.rm=T)
p3_bot<-tapply(dd$ex_price[dd$asset==3& dd$isBot==1],dd$tre[dd$asset==3& dd$isBot==1],mean,na.rm=T)
p4_bot<-tapply(dd$ex_price[dd$asset==4& dd$isBot==1],dd$tre[dd$asset==4& dd$isBot==1],mean,na.rm=T)

p1_h<-tapply(dd$ex_price[dd$asset==1 & dd$isBot==0],dd$tre[dd$asset==1& dd$isBot==0],mean,na.rm=T)
p2_h<-tapply(dd$ex_price[dd$asset==2& dd$isBot==0],dd$tre[dd$asset==2& dd$isBot==0],mean,na.rm=T)
p3_h<-tapply(dd$ex_price[dd$asset==3& dd$isBot==0],dd$tre[dd$asset==3& dd$isBot==0],mean,na.rm=T)
p4_h<-tapply(dd$ex_price[dd$asset==4& dd$isBot==0],dd$tre[dd$asset==4& dd$isBot==0],mean,na.rm=T)

############
#Creates Price data in Table 4
xtable(round(rbind(rbind(p1,p2,p3,p4),rbind(p1_bot,p2_bot,p3_bot,p4_bot),rbind(p1_h,p2_h,p3_h,p4_h)),0),digits=0)
############

#Create Table D.1
table(dd$session[dd$asset==4 & dd$tre==1 & dd$isBot==TRUE])
table(dd$session[dd$asset==4 & dd$tre==1 & dd$isBot==FALSE])
table(dd$session[dd$asset==4 & dd$tre==2 & dd$isBot==TRUE])
table(dd$session[dd$asset==4 & dd$tre==2 & dd$isBot==FALSE])
table(dd$session[dd$asset==4 & dd$tre==3 & dd$isBot==TRUE])
table(dd$session[dd$asset==4 & dd$tre==3 & dd$isBot==FALSE])


q1<-table(dd$tre[dd$asset==1])/(7*5)
q2<-table(dd$tre[dd$asset==2])/(7*5)
q3<-table(dd$tre[dd$asset==3])/(7*5)
q4<-table(dd$tre[dd$asset==4])/(7*5)

q1_bot<-table(dd$tre[dd$asset==1& dd$isBot==1])/(7*5)
q2_bot<-table(dd$tre[dd$asset==2& dd$isBot==1])/(7*5)
q3_bot<-table(dd$tre[dd$asset==3& dd$isBot==1])/(7*5)
q4_bot<-table(dd$tre[dd$asset==4& dd$isBot==1])/(7*5)

q1_h<-table(dd$tre[dd$asset==1& dd$isBot==0])/(7*5)
q2_h<-table(dd$tre[dd$asset==2& dd$isBot==0])/(7*5)
q3_h<-table(dd$tre[dd$asset==3& dd$isBot==0])/(7*5)
q4_h<-table(dd$tre[dd$asset==4& dd$isBot==0])/(7*5)

############
#Creates Turnover data in Table 4
xtable(round(rbind(rbind(q1,q2,q3,q4),rbind(q1_bot,q2_bot,q3_bot,q4_bot),rbind(q1_h,q2_h,q3_h,q4_h)),0),digits=0)
############

table(dd$asset,dd$isBot)

#Create Table D.4 First 3 columns, Change tre=1,2,3 for the different tows
bot_d <- filter(dd, asset==4 & tre==3)
bot_u <- filter(dd, asset!=4 & tre==3)

bot_bought_d <- sum(bot_d  %>% filter(maker_isBot==TRUE & make_isbid ==TRUE ) %>% select(ex_price))
bot_bought_d2 <- sum(bot_d  %>% filter(taker_isBot==TRUE & make_isbid ==FALSE ) %>% select(ex_price))

bot_sold_d <- sum(bot_d  %>% filter(maker_isBot==TRUE & make_isbid ==FALSE ) %>% select(ex_price))
bot_sold_d2 <- sum(bot_d  %>% filter(taker_isBot==TRUE & make_isbid ==TRUE ) %>% select(ex_price))

bot_bought_u <- sum(bot_u  %>% filter(maker_isBot==TRUE & make_isbid ==TRUE ) %>% select(ex_price))
bot_bought_u2 <- sum(bot_u  %>% filter(taker_isBot==TRUE & make_isbid ==FALSE ) %>% select(ex_price))

bot_sold_u <- sum(bot_u  %>% filter(maker_isBot==TRUE & make_isbid ==FALSE ) %>% select(ex_price))
bot_sold_u2 <- sum(bot_u %>% filter(taker_isBot==TRUE & make_isbid ==TRUE ) %>% select(ex_price))

profits_bot <- (bot_sold_d+bot_sold_d2-bot_bought_u-bot_bought_u2)+bot_sold_u+bot_sold_u2-bot_bought_d-bot_bought_d2
profits_bot/(5*7)

qq<-sum(dd$ex_price[dd$asset==4 & dd$isBot==TRUE & dd$tre==3]>0 )/(5*7)
print(c(profits_bot/(5*7),qq,profits_bot/(5*7)/qq))


s1<-tapply(full_data$participant.id_in_session[full_data$tre==1],full_data$session.code[full_data$tre==1],max,na.rm=T)
s2<-tapply(full_data$participant.id_in_session[full_data$tre==2],full_data$session.code[full_data$tre==2],max,na.rm=T)
s3<-tapply(full_data$participant.id_in_session[full_data$tre==3],full_data$session.code[full_data$tre==3],max,na.rm=T)


q1_1<-c(table(dd$sess[dd$asset==1 &dd$tre==1]))
q2_1<-c(table(dd$sess[dd$asset==2 &dd$tre==1]))
q3_1<-c(table(dd$sess[dd$asset==3 &dd$tre==1]))
q4_1<-c(table(dd$sess[dd$asset==4 &dd$tre==1]))

q1_2<-c(table(dd$sess[dd$asset==1 &dd$tre==2]))
q2_2<-c(table(dd$sess[dd$asset==2 &dd$tre==2]))
q3_2<-c(table(dd$sess[dd$asset==3 &dd$tre==2]))
q4_2<-c(table(dd$sess[dd$asset==4 &dd$tre==2]))

q1_3<-c(table(dd$sess[dd$asset==1 &dd$tre==3]))
q2_3<-c(table(dd$sess[dd$asset==2 &dd$tre==3]))
q3_3<-c(table(dd$sess[dd$asset==3 &dd$tre==3]))
q4_3<-c(table(dd$sess[dd$asset==4 &dd$tre==3]))

#Test turnover C in 2 v 1
etf12<-as.data.frame(cbind(c(q3_2/s2,q3_1/s1),c(rep("one",5),rep("two",5))))
names(etf12)<-c("values","tre")
etf12$values<-as.numeric(etf12$values)
etf12$tre<-as.factor(etf12$tre)
oneway_test(values~tre,data=etf12)

#Test turnover B in 2 v 1
etf12<-as.data.frame(cbind(c(q2_2/s2,q2_1/s1),c(rep("one",5),rep("two",5))))
names(etf12)<-c("values","tre")
etf12$values<-as.numeric(etf12$values)
etf12$tre<-as.factor(etf12$tre)
oneway_test(values~tre,data=etf12)

#Test turnover A in 2 v 1
etf12<-as.data.frame(cbind(c(q1_2/s2,q1_1/s1),c(rep("one",5),rep("two",5))))
names(etf12)<-c("values","tre")
etf12$values<-as.numeric(etf12$values)
etf12$tre<-as.factor(etf12$tre)
oneway_test(values~tre,data=etf12)


t1 = c(mean(q2_1/q1_1),mean(q3_1/q1_1),mean(q4_1/q1_1))
t2 = c(mean(q2_2/q1_2),mean(q3_2/q1_2),mean(q4_2/q1_2))
t3 = c(mean(q2_3/q1_3),mean(q3_3/q1_3),mean(q4_2/q1_3))
rbind(t1,t2,t3)


t1c = c(mean(q1_1/q3_1),mean(q2_1/q1_1),mean(q4_1/q3_1))
t2c = c(mean(q1_2/q3_2),mean(q2_2/q3_2),mean(q4_2/q3_2))
t3c = c(mean(q1_3/q3_3),mean(q2_3/q3_3),mean(q4_2/q3_3))
rbind(t1c,t2c,t3c)


mean(q1_1/q3_1)


##Count how many transactions are in parity for last column of Table D.4 

spread<-matrix(NA,3,5)
qETF<-matrix(NA,3,5)
pETF<-matrix(NA,3,5)
for(t in 1:3){
  list_s <-unique(dd$session[dd$tre==t])
  
  for(s in 1:length(list_s)){
    bot_etf <- filter(dd, asset==4 & tre==t  & session==list_s[s] & isBot==T)
    bot_und <- filter(dd, asset!=4 & tre==t  & session==list_s[s] & isBot==T)
    
    bot_etf <- bot_etf %>% arrange(tiempo)
    bot_und <- bot_und %>% arrange(tiempo)
    
    bot_etf<- mutate(bot_etf, id = row_number())
    bot_und<- mutate(bot_und, id = ceiling(row_number()/3))
    et<- tapply(bot_etf$ex_price,bot_etf$id,sum,na.rm=T)
    ud<-tapply(bot_und$ex_price,bot_und$id,sum,na.rm=T)
    
    spread[t,s]<- sum(et-ud)
    qETF[t,s]<- length(et)
    pETF[t,s]<- sum(et==ud)
  }
}  
print(apply(pETF/qETF,1,mean))
