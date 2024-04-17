## Put all data together using clean folder
rm(list = ls())
library(here)
setwd(here("clean"))
files<-list.files(pattern="data_")
d<-NULL
for (f in files){
  load(f)
  d<-rbind(d,df)
}

files_t<-list.files(pattern="datatrades*")
dd<-NULL
for (f in files_t){
  load(f)
  dd<-rbind(dd,dt)
}

save(d,file="alldata.Rda")
save(dd,file="alltrades.Rda")

