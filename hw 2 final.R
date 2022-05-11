setwd("C:/Users/dogad/Desktop/IE360")
require(data.table)
require(lubridate)
require(zoo)
require(ggplot2)
require(RcppRoll)
require(GGally)
require(forecast)
petroldata<-fread("IE360_Spring22_HW2_data.csv")
str(petroldata)

petroldata$`Unleaded Gasoline Sale (UGS)`=as.numeric(gsub(" ", "", petroldata$`Unleaded Gasoline Sale (UGS)`))
petroldata$`# LPG Vehicles (NLPG)`=as.numeric(gsub(" ", "",petroldata$`# LPG Vehicles (NLPG)` ))
petroldata$`# Unleaded Gasoline Vehicles (NUGV)`=as.numeric(gsub(" ", "", petroldata$`# Unleaded Gasoline Vehicles (NUGV)`))
petroldata$`GNP Agriculture`=as.numeric(gsub(" ", "", petroldata$`GNP Agriculture`))
petroldata$`GNP Commerce`=as.numeric(gsub(" ", "", petroldata$`GNP Commerce`))
petroldata$`GNP Total`=as.numeric(gsub(" ", "", petroldata$`GNP Total`))

petroldata$Quarter= as.Date(as.yearqtr(petroldata$Quarter,format="%Y_Q%q"))

str(petroldata)

setnames(petroldata,old = c("Unleaded Gasoline Sale (UGS)"  ,"RNUV", "# LPG Vehicles (NLPG)"               
                            ,"Price of Unleaded Gasoline (PU)"      ,"Price of Diesel Gasoline (PG)"       
                            ,"# Unleaded Gasoline Vehicles (NUGV)"  ,"# of Diesel Gasoline Vehicles (NDGV)"
                            ,"GNP Agriculture"                      ,"GNP Commerce"                        
                            ,"GNP Total"        ),new= c("ugs","rnuv","nlpg","pu","pg","nugv","ndgv","gnpa","gnpc","gnpt"))

str(petroldata)


ggpugs <- ggplot(data = petroldata,aes(x = Quarter,y =ugs))
ggpugs + geom_line(color ="black")

mean_series=roll_mean(petroldata$ugs,4,align='left')
var_series=roll_var(petroldata$ugs,4,align='left') 

plot(mean_series,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Rolling Mean",
     main = "Mean series")

plot(var_series,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Rolling Variance",
     main = "Variance series")


acf(petroldata$ugs,lag.max=8,na.action = na.pass)

ggpairs(petroldata)

lm1 =lm(ugs~.,data=petroldata)
summary(lm1)
checkresiduals(lm1$residuals)

petroldata$qrtr<-as.factor(quarter(petroldata$Quarter))
lm2 =lm(ugs~qrtr,data=petroldata)
summary(lm2)
checkresiduals(lm2$residuals)

petroldata[,trnd:=1:.N]
lm3 =lm(ugs~qrtr+trnd,data=petroldata)
summary(lm3)
checkresiduals(lm3$residuals)

ggpairs(petroldata)

res=petroldata[-(29:32)]
res[,residuals:=lm3$residuals]
res
lmres=lm(residuals~rnuv+nlpg+pu+pg+nugv+ndgv+gnpa+gnpc+gnpt,data=res)
summary(lmres)
checkresiduals(lmres)

lm7 =lm(ugs~qrtr+trnd+ndgv,data=petroldata) #ndgv nugv highly correlated.
summary(lm7)
checkresiduals(lm7$residuals)

lm8 =lm(ugs~.,data=petroldata)
summary(lm8)
checkresiduals(lm8$residuals)

tmp=copy(petroldata)
tmp[,actual:=ugs]
tmp[,predicted_trend:=predict(lm3,tmp)]
tmp[,residual_trend:=actual-predicted_trend]
#head(tmp)
ggplot(tmp ,aes(x=Quarter)) +
  geom_line(aes(y=actual,color='real')) + 
  geom_line(aes(y=predicted_trend,color='predicted'))

pr1=predict(lm3,newdata = petroldata[29:32])
pr1

petroldata[,ugslag4:=c(rep(NA,4),petroldata$ugs[-(29:32)])]
lm4 =lm(ugs~ugslag4,data=petroldata[(5:28)])
summary(lm4)
checkresiduals(lm4$residuals)

lm10 =lm(ugs~ugslag4+trnd+qrtr,data=petroldata[(5:28)])
summary(lm10)
checkresiduals(lm10$residuals)

