library(dplyr)
library(tidyverse)
library(xtable)
library(here)
load(here("clean/alldata.Rda"))

d <- arrange(d, d$session, d$round, d$tiempo)

d$time_inactive[d$time_inactive>86400 & !is.na(d$time_inactive)]<-d$time_inactive[d$time_inactive>86400  & !is.na(d$time_inactive)]-86400
#d$time_inactive[d$time_inactive>86400 & !is.na(d$time_inactive)]<-d$time_inactive[d$time_inactive>86400  & !is.na(d$time_inactive)]-86400

#d$ex_price[!is.na(d$ex_price) & (d$time_inactive-d$tiempo>0.8)]<-NA

d$bb<-NA
d$bo<-NA

#d$ex_price[!is.na(d$ex_price) & (d$time_inactive-d$tiempo>0.8)]<-NA
for(sess in unique(d$session)){
  for(activo in c(1:4)){
    for(r in c(1:7)){
      books<- d %>% filter(round==r &session==sess & asset==activo) %>% select(price, tiempo, time_inactive, bid, ex_price, status)
      books$bb<-NA
      books$bo<-NA
      books$price[!is.na(books$ex_price)]<-books$ex_price[!is.na(books$ex_price)]
      books$price[books$bid==FALSE]<-books$price[books$bid==FALSE]*-1
      books$time_inactive[is.na(books$time_inactive)]<-max(books$tiempo)+10
      books$time_inactive[books$status=="ACCEPTED_TAKER"]<-books$tiempo[books$status=="ACCEPTED_TAKER"]
      for(l in c(1:dim(books)[1])){
        t<-books$tiempo[l]
        books$bb[l]<-max(books$price[books$tiempo<=t & books$time_inactive>t])
      }
      
      books$price[books$bid==FALSE]<-books$price[books$bid==FALSE]*-1
      books$price[books$bid==TRUE]<-10000
      
      for(l in c(1:dim(books)[1])){
        t<-books$tiempo[l]
        books$bo[l]<-min(books$price[books$tiempo<=t & books$time_inactive>t],na.rm=T)
      }
      
      d$bb[d$round==r &d$session==sess & d$asset==activo]<- books$bb
      d$bo[d$round==r &d$session==sess & d$asset==activo]<- books$bo
      
    }
  }
}

d<-as.data.frame(d)
d$bb<-as.numeric(paste(d$bb))
d$bo<-as.numeric(paste(d$bo))
d$ex_price<-as.numeric(paste(d$ex_price))
d$price<-as.numeric(paste(d$price))

d$bb[d$bb=="-Inf"]<-NA
d$bo[d$bo=="Inf"]<-NA
d$bb[d$bb<0]<-NA
d$bo[d$bo==10000]<-NA
d$spd<-(d$bo-d$bb)
d$spd_z<-d$spd
d$spd_z[is.na(d$spd_z)]<-0

rango<- 4
descuento<- 0.99
data<-NULL

for(sess in unique(d$session)){
  tre<-max(d$tre[d$session==sess])
  precio<-NA
  for(r in c(1:7)){
    t_0<-min(d$tiempo[d$round==r & d$session==sess])
    t_max<-max(d$tiempo[d$round==r & d$session==sess])
    precio<-NA
    dete<-0
    dete_a<-0
    for(activo in c(1:4)){
      i<-0
      precio<-NA
      dete<-0
      dete_a<-0
      books<- d %>% filter(round==r &session==sess & asset==activo) %>% select(price, tiempo, time_inactive, bid, ex_price, status)
      books$price[!is.na(books$ex_price)]<-books$ex_price[!is.na(books$ex_price)]
      books$time_inactive[is.na(books$time_inactive)]<-max(books$tiempo)+10
      books$time_inactive[books$status=="ACCEPTED_TAKER"]<-books$tiempo[books$status=="ACCEPTED_TAKER"]
      l<-t_0+rango
      while (l<=t_max){
        i<-i+1
        descuento_compra<-0
        ordenes_compra<-0
        ordenes_venta<-0
        dete<-0
        precio<-NA
        spr<-0
        bb<-NA
        bo<-NA
        libro<- books %>% filter(tiempo<=l & time_inactive>l)
        libro_p<- books %>% filter(tiempo<=l & time_inactive>l-rango)
        
        if(dim(libro)[1]>0){
          libro_compra<- libro %>% filter(bid==TRUE)
          libro_venta<-  libro %>% filter(bid==FALSE)
          if(dim(libro_compra)[1]>0){
            bb<-max(libro_compra$price)
          }
          if(dim(libro_venta)[1]>0){
            bo<-min(libro_venta$price)
          }
          spr<-mean(c(bo,bb),na.rm = T)
          
          if(dim(libro_compra)[1]>0){
            ordenes_compra<-sum(descuento^(abs(libro_compra$price-spr)),na.rm=T)
          }
          
          if(dim(libro_venta)[1]>0){
            ordenes_venta<-sum(descuento^(abs(libro_venta$price-spr)),na.rm=T)
          }
          precio<-mean(libro_p$ex_price[(libro_p$time_inactive-libro_p$tiempo<1)],na.rm = T)
          if (!is.na(precio)){
            dete<-length(libro_p$ex_price[libro_p$time_inactive-libro_p$tiempo<1 & libro_p$bid==TRUE])- length(libro_p$ex_price[libro_p$time_inactive-libro_p$tiempo<1 & libro_p$bid!=TRUE])
            dete_a<-dete_a+dete
          }
          data<-rbind(data,cbind(sess,tre,r,activo,i,ordenes_compra,ordenes_venta,bb,bo,precio,dete,dete_a))
        }
        
        l<-l+rango
      }
    }
  }
}    
data<-as.data.frame(data)
data$bb<-as.numeric(paste(data$bb))
data$bo<-as.numeric(paste(data$bo))
data$dete<-as.numeric(paste(data$dete))
data$dete_a<-as.numeric(paste(data$dete_a))
data$i<-as.numeric(paste(data$i))
data$activo<-as.numeric(paste(data$activo))
data$r<-as.numeric(paste(data$r))
data$tre<-as.numeric(paste(data$tre))
data$ordenes_compra<-as.numeric(paste(data$ordenes_compra))
data$ordenes_venta<-as.numeric(paste(data$ordenes_venta))
data$z<-data$ordenes_compra-data$ordenes_venta
data$precio<-as.numeric(paste(data$precio))

data <- arrange(data, data$sess, data$activo, data$r, data$i)
data$dt[2:length(data$r)]<-data$r[2:length(data$r)]-data$r[1:length(data$r)-1]
data$dt[1]<-0

write.csv(data,file="detf_new.csv",row.names=FALSE)