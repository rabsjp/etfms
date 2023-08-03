rm(list = ls())
library(dplyr)
library(tidyverse)
library(xtable)

load("~/Desktop/jotarepos/capmout/clean/alldata.Rda")

d <- arrange(d, d$session, d$round, d$tiempo)

d[["isBot"]] <- grepl("Bot", d[["pcode"]])

d_human= d %>% filter(isBot==FALSE) 

d_cancel= d %>% filter(status=="CANCELED", isBot==FALSE) 

table(d_cancel$tre,d_cancel$asset)
table(d_human$tre,d_human$asset)
