rm(list = ls())
library(dplyr)
library(here)

library(xtable)
#setwd("/cloud/project/data/abcone")
rm(list = ls())

# add paths to files
your_path<-here("holdings")
path = list.files(path=your_path, pattern="*.csv")
# import data with no regret columns
full_data<-NULL
for(i in seq(along=path)){
  d<-read.csv(paste(your_path, '/', path[i],sep=""),header=T, stringsAsFactors = FALSE,sep=";")
  d<-d[d$subsession.round_number<11,]
  d$player.settled_assets <- gsub("QUUX", "'", gsub("'", '"', gsub('"', "QUUX", d$player.settled_assets)))
  d$a<-NA
  d$b<-NA
  d$c<-NA
  d$etf<-NA
  
  for(j in seq(1:dim(d)[1])){
    d$a[j]<-fromJSON(d$player.settled_assets[j])[["A"]]
    d$b[j]<-fromJSON(d$player.settled_assets[j])[["B"]]
    d$c[j]<-fromJSON(d$player.settled_assets[j])[["C"]]
    d$etf[j]<-fromJSON(d$player.settled_assets[j])[["D"]]
  }
  full_data<-rbind(full_data,d)
}
full_data$tre<-1

save(full_data,file="holdings_1.Rda")

your_path<-here("holdings/2")
path = list.files(path=your_path, pattern="*.csv")
# import data with no regret columns
full_data_2<-NULL
for(i in seq(along=path)){
  d<-read.csv(paste(your_path, '/', path[i],sep=""),header=T, stringsAsFactors = FALSE,sep=";")
  d<-d[d$subsession.round_number<11,]
  d$player.settled_assets <- gsub("QUUX", "'", gsub("'", '"', gsub('"', "QUUX", d$player.settled_assets)))
  d$a<-NA
  d$b<-NA
  d$c<-NA
  d$etf<-NA
  
  for(j in seq(1:dim(d)[1])){
    d$a[j]<-fromJSON(d$player.settled_assets[j])[["A"]]
    d$b[j]<-fromJSON(d$player.settled_assets[j])[["B"]]
    d$c[j]<-fromJSON(d$player.settled_assets[j])[["C"]]
    d$etf[j]<-fromJSON(d$player.settled_assets[j])[["D"]]
  }
  full_data_2<-rbind(full_data_2,d)
}
full_data_2$tre<-2
save(full_data_2,file="holdings_2.Rda")

your_path<-here("holdings/3")
path = list.files(path=your_path, pattern="*.csv")
# import data with no regret columns
full_data_3<-NULL
for(i in seq(along=path)){
  d<-read.csv(paste(your_path, '/', path[i],sep=""),header=T, stringsAsFactors = FALSE,sep=";")
  d<-d[d$subsession.round_number<11,]
  d$player.settled_assets <- gsub("QUUX", "'", gsub("'", '"', gsub('"', "QUUX", d$player.settled_assets)))
  d$a<-NA
  d$b<-NA
  d$c<-NA
  d$etf<-NA
  
  for(j in seq(1:dim(d)[1])){
    d$a[j]<-fromJSON(d$player.settled_assets[j])[["A"]]
    d$b[j]<-fromJSON(d$player.settled_assets[j])[["B"]]
    d$c[j]<-fromJSON(d$player.settled_assets[j])[["C"]]
    d$etf[j]<-fromJSON(d$player.settled_assets[j])[["D"]]
  }
  full_data_3<-rbind(full_data_3,d)
}
full_data_3$tre<-3
save(full_data_3,file="holdings_3.Rda")

full_data<-rbind(full_data,full_data_2,full_data_3)
save(full_data,file="holdings.Rda")



