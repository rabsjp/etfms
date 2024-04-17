rm(list = ls())
library("rjson")
library(ggplot2)
library(dplyr)
library(tidyverse)
library(xtable)
library(here)

setwd(here("abc"))
# Do this for each folder abc, a2c, a2c_ss

files = list.files(pattern="*json")
protect_against_null <- function( x ) {
  if( is.null(x) )
    return( NA ) # Replace with whatever string you'd like.
  else 
    return( x )
}

df<-NULL
dt<-NULL
nombre<-files[4]
d <- fromJSON(file = nombre)
#round time_active time_inactive pcode id status price
for (round in c(4:length(d))) {
  for (asset in seq(4)){
    quantos<-length(d[[round]][["exchange_data"]][[asset]][['orders']])
    for (i in c(1:quantos)){
      esta<-d[[round]][["exchange_data"]][[asset]][['orders']][[i]]
      df<-rbind(df,c(round,esta[["time_entered"]],asset,esta[['pcode']],esta[['price']],esta[['is_bid']],esta[['id']],protect_against_null(esta[["time_inactive"]]),protect_against_null(esta[['status']])))#esta[["time_inactive"]],esta[['id']]))
    }
    market<-length(d[[round]][["exchange_data"]][[asset]][['trades']])
    if(market==0) next
    for (j in c(1:market)){
      estm<-d[[round]][["exchange_data"]][[asset]][['trades']][[j]]
      dt<-rbind(dt,c(round,estm[["timestamp"]],asset,estm[['taking_order_id']],estm[['making_order_ids']]))
    }
  }
}


df<-as.data.frame(df)
dt<-as.data.frame(dt)
names(df)<-c("round","time_entered","asset","pcode","price","bid","id","time_inactive","status")
names(dt)<-c("round","time_entered","asset","take_id","make_id")
df$round<-as.numeric(as.character(df$round))
df$time_entered<-as.character(df$time_entered)
df$time_inactive<-as.character(df$time_inactive)
df$status<-as.character(df$status)
df$time_entered<-as.numeric(paste(df$time_entered))
df$time_inactive<-as.numeric(paste(df$time_inactive))

df$asset<-as.numeric(as.character(df$asset))
df$price<-as.numeric(as.character(df$price))
df$pcode<-as.character(df$pcode)
df$id<-as.numeric(as.character(df$id))

dt<-as.data.frame(dt)
dt$round<-as.numeric(as.character(dt$round))
dt$asset<-as.numeric(as.character(dt$asset))
dt$take_id<-as.numeric(as.character(dt$take_id))
dt$make_id<-as.numeric(as.character(dt$make_id))
dt$time_entered<-as.numeric(as.character(dt$time_entered))

df$round<-df$round-3
dt$round<-dt$round-3

df= arrange(df, round, time_entered)
df$tiempo<-df$time_entered
d_tempora<- df %>% select(id,price)
dt$tiempo<-as.numeric(dt$time_entered)

dt<- dt %>% inner_join(d_tempora, by = c("make_id" = "id"))
colnames(dt)[7]<-"ex_price"
makers<- dt %>% select(make_id,ex_price)
takers<- dt %>% select(take_id,ex_price)
colnames(takers)<-colnames(makers)
makers<-rbind(makers,takers)

df<- df  %>% left_join(makers, by = c("id"= "make_id"))

#df$time_inactive[df$status=="CANCELED"]<-df$time_inactive[df$status=="CANCELED"]+7200
df$tre<-1 #1 refers to ABC 2 to A2B, 3 to A2B_SS
df$session<-unlist(strsplit(nombre, split='.', fixed=TRUE))[1]

for(r in 2:length(unique(df$round))){
  df$tiempo[df$round==r]<-df$tiempo[df$round==r]+max(df$tiempo[df$round==r-1])
  dt$tiempo[dt$round==r]<-dt$tiempo[dt$round==r]+max(df$tiempo[df$round==r-1])
  #df$time_inactive[df$round==r]<-df$time_inactive[df$round==r]+max(df$tiempo[df$round==r-1])
}

for(r in 2:length(unique(df$round))){
  df$time_inactive[df$round==r]<-df$time_inactive[df$round==r]+max(df$tiempo[df$round==r-1])
}


dt$tre<-1 #1 refers to ABC 2 to A2B, 3 to A2B_SS
dt$session<-unlist(strsplit(nombre, split='.', fixed=TRUE))[1]

d_pcodes<- df %>% select(id,pcode)

dt<- dt %>% inner_join(d_pcodes, by = c("make_id" = "id"))
colnames(dt)[10] = "make_pcode"

dt<- dt %>% inner_join(d_pcodes, by = c("take_id" = "id"))
colnames(dt)[11] = "take_pcode"

d_bid<- df %>% select(id,bid)

dt<- dt %>% inner_join(d_bid, by = c("make_id" = "id"))
colnames(dt)[12] = "make_isbid"

save(df,file=paste("data_",df$session[1],".Rda",sep=""))
save(dt,file=paste("datatrades_",dt$session[1],".Rda",sep=""))
