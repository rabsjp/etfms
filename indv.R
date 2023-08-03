rm(list = ls())
library(dplyr)
library(tidyverse)
library(xtable)
library(here)
load("~/Desktop/jotarepos/etfms/clean/alltrades.Rda")
players<-read.csv("~/Desktop/jotarepos/etfms/clean/total_players.csv",sep=";",header=T, stringsAsFactors = FALSE)

###Table of Buy-Hold ETFs
dw = inner_join(dd, players, by="session")

dw[["isBot"]] <- grepl("Bot", dw[["make_pcode"]]) | grepl("Bot", dw[["take_pcode"]])
dw[["maker_isBot"]] <- grepl("Bot", dw[["make_pcode"]])
dw[["taker_isBot"]] <- grepl("Bot", dw[["take_pcode"]])

ta = table(dw$isBot[dw$asset==4],dw$tre[dw$asset==4])[c(2:1),]
ta = rbind(ta,apply(ta,2,sum))

t(ta)/apply(ta,2,sum)
xtable(ta)

#check redemptions

bot_buy_D_maker <- dw %>% filter(asset == 4 & make_isbid==T & maker_isBot==T)  
bot_buy_D_taker <- dw %>% filter(asset == 4 & make_isbid==F & taker_isBot==T)  


t1 = table(dw$isBot[dw$asset==4 & dw$tre==1],dw$session[dw$asset==4& dw$tre==1]) 

t2 = table(dw$isBot[dw$asset==4 & dw$tre==2],dw$session[dw$asset==4& dw$tre==2]) 

t3 = table(dw$isBot[dw$asset==4 & dw$tre==3],dw$session[dw$asset==4& dw$tre==3]) 

bh = rbind(sort(t1[2,]/apply(t1,2,sum)), sort(t2[2,]/apply(t2,2,sum)), sort(t3[2,]/apply(t3,2,sum)))
xtable(bh)
xtable(rbind(t1[c(2:1),],t2[c(2:1),],t3[c(2:1),]))

###Table of Buy-Sell C and B. 

buy_b <- dw %>% filter(asset == 2 & make_isbid==T)  %>%
  select(round,session,tre,tiempo,make_pcode,ex_price,asset)

names(buy_b)[names(buy_b) == "make_pcode"] <- "p_code"

buy_b_takers <- dw %>% filter(asset == 2 & make_isbid==F)  %>%
  select(round,session,tre,tiempo,take_pcode,ex_price,asset)

names(buy_b_takers)[names(buy_b_takers) == "take_pcode"] <- "p_code"

buy_b <-rbind(buy_b,buy_b_takers)

sell_c <- dw %>% filter(asset == 3 & make_isbid==F)  %>%
  select(round,session,tre,tiempo,make_pcode,ex_price,asset)

names(sell_c)[names(sell_c) == "make_pcode"] <- "p_code"

sell_c_takers <- dw %>% filter(asset == 3 & make_isbid==T )  %>%
  select(round,session,tre,tiempo,take_pcode,ex_price,asset)

names(sell_c_takers)[names(sell_c_takers) == "take_pcode"] <- "p_code"

sell_c <-rbind(sell_c,sell_c_takers)

solo_buy_b <- buy_b %>% 
  group_by(round,tre) %>% 
  distinct(p_code)

solo_sell_c <- sell_c %>% 
  group_by(round,tre) %>% 
  distinct(p_code)

buy_b_sell_c <- inner_join(solo_buy_b,solo_sell_c ,by.x=c("round", "tre","p_code"), by.y=c("round", "tre","p_code"))

###Table of buy-sell C and B. 

sell_b <- dw %>% filter(asset == 2 & make_isbid==F )  %>%
  select(round,session,tre,tiempo,make_pcode,ex_price,asset)

names(sell_b)[names(sell_b) == "make_pcode"] <- "p_code"

sell_b_takers <- dw %>% filter(asset == 2 & make_isbid==T)  %>%
  select(round,session,tre,tiempo,take_pcode,ex_price,asset)

names(sell_b_takers)[names(sell_b_takers) == "take_pcode"] <- "p_code"

sell_b <-rbind(sell_b,sell_b_takers)

buy_c <- dw %>% filter(asset == 3 & make_isbid==T)  %>%
  select(round,session,tre,tiempo,make_pcode,ex_price,asset)

names(buy_c)[names(buy_c) == "make_pcode"] <- "p_code"

buy_c_takers <- dw %>% filter(asset == 3 & make_isbid==F)  %>%
  select(round,session,tre,tiempo,take_pcode,ex_price,asset)

names(buy_c_takers)[names(buy_c_takers) == "take_pcode"] <- "p_code"

buy_c <-rbind(buy_c,buy_c_takers)

solo_sell_b <- sell_b %>% 
  group_by(round,tre) %>% 
  distinct(p_code)

solo_buy_c <- buy_c %>% 
  group_by(round,tre) %>% 
  distinct(p_code)

buy_c_sell_b <- inner_join(solo_buy_c,solo_sell_b ,by.x=c("round", "tre","p_code"), by.y=c("round", "tre","p_code"))

bs1 <- c(table(buy_b_sell_c$tre)/7)/c(58,62,62)
bs2 <- c(table(buy_c_sell_b$tre)/7)/c(58,62,62)

xtable(rbind(bs1,bs2))


