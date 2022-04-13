# Homework-1
# Doða Durmuþ
# 2018402015
library(data.table)
library(lubridate)
library(ggplot2)
library(zoo)
library(ggcorrplot)
library(GGally)

#before reading the data, all data is gathered in one sheet and column names are changed to simpler names to achieve a better readibility 
datapath='C:/Users/dogad/Desktop/IE360/alldata.csv'
alldata=fread(datapath)
str(alldata)
alldata$date=dmy(alldata$date)
str(alldata)

# Monthly USD/TRY Exchange Rate Line Graph
ggpexc <- ggplot(data = alldata,aes(x = date,y = rate))
ggpexc + geom_line(color ="black") + labs(title = "USD/TRY Exchange Rates vs. Time",x = "Time", y = "USD Exchange Rate" ) +scale_x_date(date_breaks = "3 month",date_minor_breaks = "1 month")+
  theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +
  scale_y_continuous(breaks = seq(from = 0,to = 16,by = 1))

# General Consumer Price Index Line Graph
ggpcpi <- ggplot(data = alldata,aes(x = date,y = cpi))
ggpcpi + geom_line(color ="black") + labs(title = "Consumer Price Index vs. Time",x = "Time", y = "Consumer Price Index" )+scale_x_date(date_breaks = "3 month",date_minor_breaks = "1 month")+
  theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +
  scale_y_continuous(breaks = seq(from = 300,to = 800,by = 25))

# Average Monthly Gold Price Line Graph
ggpgold <- ggplot(data = alldata,aes(x = date,y = gold))
ggpgold + geom_line(color ="black") + labs(title = "Gold Price vs. Time",x = "Time", y = "Gold Price(TRY) " )+scale_x_date(date_breaks = "3 month",date_minor_breaks = "1 month") +
  theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +
  scale_y_continuous(breaks = seq(from = 1000,to = 6500,by = 250))

# All 3 Line Graphs
alldata[,exchange_normalized:=(alldata$rate-min(alldata$rate))/(max(alldata$rate)-min(alldata$rate))]
alldata[,cpi_normalized:=(alldata$cpi-min(alldata$cpi))/(max(alldata$cpi)-min(alldata$cpi))]
alldata[,gold_normalized:=(alldata$gold-min(alldata$gold))/(max(alldata$gold)-min(alldata$gold))]

gg_all<-ggplot(data=alldata,aes(x=date,y=exchange_normalized))
gg_all + geom_line(color="black") + geom_line(data=alldata,aes(x=date,y=cpi_normalized),color="red")+geom_line(data=alldata,aes(x=date,y=gold_normalized),color="blue")+ labs(title = "Normalized Data vs Time")

# Google Trends Analysis
gt_dolar<-read.csv("gt dolar.csv")
gt_dolarkactl<-read.csv("gt dolar kaç tl.csv")
gt_dolarkuru<-read.csv("dolar kuru.csv")

gt_zam<-read.csv("zam.csv")
gt_zamgeldi<-read.csv("zam geldi.csv")

gt_altin<-read.csv("altýn.csv")
gt_altinzam<-read.csv("altýn zam.csv")
gt_altinfiyati<-read.csv("altýn fiyatý.csv")

gg_d_1<-ggplot(data=gt_dolar,aes(x=Ay,y=dolar,group=1))+geom_line()+theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4))
gg_d_1
gg_d_2<-ggplot(data=gt_dolarkactl,aes(x=Ay,y=dolar.kac.tl,group=1))+geom_line()+theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4))
gg_d_2
gg_d_3<-ggplot(data=gt_dolarkuru,aes(x=Ay,y=dolar.kuru,group=1))+geom_line()+theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4))
gg_d_3

gg_z_1<-ggplot(data=gt_zam,aes(x=Ay,y=zam,group=1))+geom_line()+theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4))
gg_z_1
gg_z_2<-ggplot(data=gt_zamgeldi,aes(x=Ay,y=zam.geldi,group=1))+geom_line()+theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4))
gg_z_2

gg_a_1<-ggplot(data=gt_altin,aes(x=Ay,y=altin,group=1))+geom_line()+theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4))
gg_a_1
gg_a_2<-ggplot(data=gt_altinzam,aes(x=Ay,y=altin.zam,group=1))+geom_line()+theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4))
gg_a_2
gg_a_3<-ggplot(data=gt_altinfiyati,aes(x=Ay,y=altin.fiyatý,group=1))+geom_line()+theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4))
gg_a_3


# Correlation Analysis
for_corr=alldata[,c("rate","cpi","gold")]
corr_info=cor(for_corr)
corr_info
ggcorrplot(corr_info)
ggpairs(for_corr)
