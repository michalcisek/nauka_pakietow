rm(list=ls())
# install.packages("sqldf")
library(sqldf)
library(lubridate)
tran<-read.csv2("gral_transakcje.csv",header=F,dec = ".")
tran<-tran[,-1]
colnames(tran)<-c("time","type","no_order","size","price","sl","tp","profit","balance")
tran$time<-as.POSIXct(tran$time,format="%Y.%m.%d %H:%M")

attach(tran)

modify<-tran[which(type=="modify"),]
no_modify<-sqldf("select no_order, count(*) sum from modify group by no_order")

zlecenia<-tran[which(type %in% c("buy","sell")),]
zlecenia<-zlecenia[,-8]
colnames(zlecenia)<-c("open_time","open_type","no_order","size","open_price","sl","tp","open_balance")

close<-tran[which(type %in% c("s/l","t/p","close at stop")),]


zlecenia<-sqldf("select a.*, b.time close_time, b.type close_type, b.profit, b.balance close_balance, c.sum no_modify
                  from zlecenia a
                  left join
                  close b
                  on a.no_order=b.no_order
                  left join
                  no_modify c
                  on a.no_order=c.no_order")
zlecenia$close_time<-as.POSIXct(zlecenia$close_time,origin="1970-01-01")
zlecenia<-zlecenia[,c(3,1,9,2,10,4,5,6,7,8,12,11,13)]
zlecenia[is.na(zlecenia$no_modify),13] <- 0
zlecenia$duration<-difftime(zlecenia$close_time, zlecenia$open_time, units = "mins")
zlecenia$wday<-wday(zlecenia$open_time, label=T)


library(ggplot2)
ggplot(zlecenia,aes(x=profit))+
  geom_histogram(binwidth = 1)+
  facet_grid(wday ~ .)+
  xlim(c(0,250))+
  theme_bw()

ggplot(zlecenia,aes(x=profit, y=duration))+
  geom_bin2d()+
  theme_bw()+
  facet_grid(wday ~ .)

ggplot(zlecenia,aes(x=profit, y=no_modify))+
  geom_bin2d()+
  theme_bw()
ggplot(zlecenia,aes(x=profit, y=no_modify))+
  geom_jitter()+
  theme_bw()+
  facet_grid(wday ~ .)

ggplot(zlecenia,aes(x=close_time,y=close_balance))+
  geom_line()
