#rm(list = ls())
library(dplyr)
library(here)
library(xtable)

load(here("holdings/holdings.Rda"))
d<-full_data

ds<- full_data %>% group_by(player.id_in_group,session.code,tre) %>% 
  summarize(q_a = median(a), q_b = median(b),q_c = median(c),q_etf =median(etf), cash=median(player.settled_cash))

ds$imba<-abs((2*ds$q_a-(ds$q_b+ds$q_c))/(ds$q_a+ds$q_b+ds$q_c+3*ds$q_etf))

#ds$imba<-abs((2*ds$q_a-(ds$q_b+ds$q_c))/(ds$q_a+ds$q_b+ds$q_c+3*ds$q_etf+ds$cash))

t1<-ds$imba[ds$tre<3 & !!(ds$player.id_in_group %% 2) & ds$q_etf>0]
t2<-ds$imba[ds$tre<3 & !!(ds$player.id_in_group %% 2)& ds$q_etf==0]

#Test for type 1
etf12<-as.data.frame(cbind(c(t1,t2),c(rep("one",length(t1)),rep("two",length(t2)))))
names(etf12)<-c("values","tipo")
etf12$values<-as.numeric(etf12$values)
etf12$tipo<-as.factor(etf12$tipo)
oneway_test(values~tipo,data=etf12)

#Test for type 2 
t3<-ds$imba[ds$tre<3 & !(ds$player.id_in_group %% 2) & ds$q_etf>0]
t4<-ds$imba[ds$tre<3 & !(ds$player.id_in_group %% 2)& ds$q_etf==0]

etf12<-as.data.frame(cbind(c(t3,t4),c(rep("one",length(t1)),rep("two",length(t2)))))
names(etf12)<-c("values","tipo")
etf12$values<-as.numeric(etf12$values)
etf12$tipo<-as.factor(etf12$tipo)
oneway_test(values~tipo,data=etf12)

#Figure 5 
png(here("outputs/imbalance_etfs_1.png"), width = 500, height = 500)
plot(ecdf(ds$imba[ds$tre<3 & !!(ds$player.id_in_group %% 2) & ds$q_etf>0]),verticals=T,cex=0,lwd=2,lty=2,xlab="value",ylab="CDF",main="Type I",xlim=c(0,0.8),cex.lab=1.4, cex.main=2)
lines(ecdf(ds$imba[ds$tre<3 & !!(ds$player.id_in_group %% 2)& ds$q_etf==0]),verticals=T,cex=0,lwd=2,lty=2,col="gray")
abline(v=abs((6-2)/(3+1+1)),lty=2)
text(x=abs((6-2)/(3+1+1))-.13,.1,"Type I initial imbalance",cex=1,lwd=2)
legend(0.01,.95,c("With ETF","Without ETF"),col=c("black", "gray"),lty=c(2,2),cex=1.2,bty = "n")
dev.off()

png(here("outputs/imbalance_etfs_2.png"), width = 500, height = 500)
plot(ecdf(ds$imba[ds$tre<3 & !(ds$player.id_in_group %% 2) & ds$q_etf>0]),verticals=T,cex=0,lwd=2,lty=2,xlab="value",ylab="CDF",main="Type II",cex.lab=1.4,cex.main=2)
lines(ecdf(ds$imba[ds$tre<3 & !(ds$player.id_in_group %% 2)& ds$q_etf==0]),verticals=T,cex=0,lwd=2,lty=2,col="gray")
abline(v=abs((2-6)/(1+3+3)),col="black",lty=2)
text(x=abs((2-6)/(1+3+3))-.12,.1,"Type II initial imbalance",cex=1,col="black",lwd=2)
legend(0.01,.95,c("With ETF","Without ETF"),col=c("black", "gray"),lty=c(2,2),cex=1.2,bty = "n")
dev.off()

mean(d$player.payoff[d$etf>0 & d$tre==1])
mean(d$player.payoff[d$etf==0 & d$tre==1])

mean(d$player.payoff[d$etf>0 & d$tre==2])
mean(d$player.payoff[d$etf==0 & d$tre==2])

mean(d$player.payoff[d$etf>0 & d$tre==3])
mean(d$player.payoff[d$etf==0 & d$tre==3])

sd(d$player.payoff[d$etf>0 & d$tre==1])
sd(d$player.payoff[d$etf==0 & d$tre==1])

sd(d$player.payoff[d$etf>0 & d$tre==2])
sd(d$player.payoff[d$etf==0 & d$tre==2])

sd(d$player.payoff[d$etf>0 & d$tre==3])
sd(d$player.payoff[d$etf==0 & d$tre==3])



d$player.odd<- d$player.id_in_group %% 2
tapply(d$a[d$etf>0 & d$tre==1],d$player.odd[d$etf>0& d$tre==1],mean)
tapply(d$a[d$etf==0 & d$tre==1],d$player.odd[d$etf==0& d$tre==1],mean)

tapply(d$a[d$etf>0 & d$tre==2],d$player.odd[d$etf>0& d$tre==2],mean)
tapply(d$a[d$etf==0 & d$tre==2],d$player.odd[d$etf==0& d$tre==2],mean)

tapply(d$b[d$etf>0 & d$tre==1],d$player.odd[d$etf>0& d$tre==1],mean)
tapply(d$b[d$etf==0 & d$tre==1],d$player.odd[d$etf==0& d$tre==1],mean)

tapply(d$b[d$etf>0 & d$tre==2],d$player.odd[d$etf>0& d$tre==2],mean)
tapply(d$b[d$etf==0 & d$tre==2],d$player.odd[d$etf==0& d$tre==2],mean)

tapply(d$b[d$etf>0 & d$tre==3],d$player.odd[d$etf>0& d$tre==3],mean)
tapply(d$b[d$etf==0 & d$tre==3],d$player.odd[d$etf==0& d$tre==3],mean)

tapply(d$c[d$etf>0 & d$tre==1],d$player.odd[d$etf>0& d$tre==1],mean)
tapply(d$c[d$etf==0 & d$tre==1],d$player.odd[d$etf==0& d$tre==1],mean)

tapply(d$c[d$etf>0 & d$tre==2],d$player.odd[d$etf>0& d$tre==2],mean)
tapply(d$c[d$etf==0 & d$tre==2],d$player.odd[d$etf==0& d$tre==2],mean)



