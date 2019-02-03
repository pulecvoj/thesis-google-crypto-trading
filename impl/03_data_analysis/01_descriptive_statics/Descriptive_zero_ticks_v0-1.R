rm(list=ls())
zero_ticks <- as.data.frame(matrix(data = 1,nrow = 6,ncol = 4))
colnames(zero_ticks) <- c("XBT", "ETH", "LTC", "XMR")

zero_search<- as.data.frame(matrix(data = 1,nrow = 6,ncol = 4))
colnames(zero_search) <- c("XBT", "ETH", "LTC", "XMR")

# 1 ) *************** XBT ******************************************

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd) 


load(file="XBT_1minute.Rdata")
load(file="XBT_5minute_returns.Rdata")
load(file="XBT_15minute_returns.Rdata")
load(file="XBT_30minute_returns.Rdata")
load(file="XBT_60minute_returns.Rdata")
load(file="XBT_day_returns.Rdata")

zero_ticks$XBT[1] <- length(which(minute_data$logreturn_price_all==0))/nrow(minute_data)
zero_ticks$XBT[2] <- length(which(five_minute_data$logreturn_price_all==0))/nrow(five_minute_data)
zero_ticks$XBT[3] <- length(which(fifteen_minute_data$logreturn_price_all==0))/nrow(fifteen_minute_data)
zero_ticks$XBT[4] <- length(which(thirty_minute_data$logreturn_price_all==0))/nrow(thirty_minute_data)
zero_ticks$XBT[5] <- length(which(hour_data$logreturn_price_all==0))/nrow(hour_data)
zero_ticks$XBT[6] <- length(which(day_data$logreturn_price_all==0))/nrow(day_data)

zero_search$XBT[1] <- length(which(minute_data$SVI==0))/nrow(minute_data)
zero_search$XBT[2] <- length(which(five_minute_data$SVI==0))/nrow(five_minute_data)
zero_search$XBT[3] <- length(which(fifteen_minute_data$SVI==0))/nrow(fifteen_minute_data)
zero_search$XBT[4] <- length(which(thirty_minute_data$SVI==0))/nrow(thirty_minute_data)
zero_search$XBT[5] <- length(which(hour_data$SVI==0))/nrow(hour_data)
zero_search$XBT[6] <- length(which(day_data$SVI==0))/nrow(day_data)

# 2 ) *************** ETH ******************************************

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
setwd(wd) 

load(file="ETH_1minute.Rdata")
load(file="ETH_5minute_returns.Rdata")
load(file="ETH_15minute_returns.Rdata")
load(file="ETH_30minute_returns.Rdata")
load(file="ETH_60minute_returns.Rdata")
load(file="ETH_day_returns.Rdata")

zero_ticks$ETH[1] <- length(which(minute_data$logreturn_price_all==0))/nrow(minute_data)
zero_ticks$ETH[2] <- length(which(five_minute_data$logreturn_price_all==0))/nrow(five_minute_data)
zero_ticks$ETH[3] <- length(which(fifteen_minute_data$logreturn_price_all==0))/nrow(fifteen_minute_data)
zero_ticks$ETH[4] <- length(which(thirty_minute_data$logreturn_price_all==0))/nrow(thirty_minute_data)
zero_ticks$ETH[5] <- length(which(hour_data$logreturn_price_all==0))/nrow(hour_data)
zero_ticks$ETH[6] <- length(which(day_data$logreturn_price_all==0))/nrow(day_data)

zero_search$ETH[1] <- length(which(minute_data$SVI==0))/nrow(minute_data)
zero_search$ETH[2] <- length(which(five_minute_data$SVI==0))/nrow(five_minute_data)
zero_search$ETH[3] <- length(which(fifteen_minute_data$SVI==0))/nrow(fifteen_minute_data)
zero_search$ETH[4] <- length(which(thirty_minute_data$SVI==0))/nrow(thirty_minute_data)
zero_search$ETH[5] <- length(which(hour_data$SVI==0))/nrow(hour_data)
zero_search$ETH[6] <- length(which(day_data$SVI==0))/nrow(day_data)


# 3 ) *************** LTC ******************************************

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
setwd(wd) 

load(file="LTC_1minute.Rdata")
load(file="LTC_5minute_returns.Rdata")
load(file="LTC_15minute_returns.Rdata")
load(file="LTC_30minute_returns.Rdata")
load(file="LTC_60minute_returns.Rdata")
load(file="LTC_day_returns.Rdata")

zero_ticks$LTC[1] <- length(which(minute_data$logreturn_price_all==0))/nrow(minute_data)
zero_ticks$LTC[2] <- length(which(five_minute_data$logreturn_price_all==0))/nrow(five_minute_data)
zero_ticks$LTC[3] <- length(which(fifteen_minute_data$logreturn_price_all==0))/nrow(fifteen_minute_data)
zero_ticks$LTC[4] <- length(which(thirty_minute_data$logreturn_price_all==0))/nrow(thirty_minute_data)
zero_ticks$LTC[5] <- length(which(hour_data$logreturn_price_all==0))/nrow(hour_data)
zero_ticks$LTC[6] <- length(which(day_data$logreturn_price_all==0))/nrow(day_data)

zero_search$LTC[1] <- length(which(minute_data$SVI==0))/nrow(minute_data)
zero_search$LTC[2] <- length(which(five_minute_data$SVI==0))/nrow(five_minute_data)
zero_search$LTC[3] <- length(which(fifteen_minute_data$SVI==0))/nrow(fifteen_minute_data)
zero_search$LTC[4] <- length(which(thirty_minute_data$SVI==0))/nrow(thirty_minute_data)
zero_search$LTC[5] <- length(which(hour_data$SVI==0))/nrow(hour_data)
zero_search$LTC[6] <- length(which(day_data$SVI==0))/nrow(day_data)

# 4 ) *************** XMR ******************************************

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
setwd(wd) 

load(file="XMR_1minute.Rdata")
load(file="XMR_5minute_returns.Rdata")
load(file="XMR_15minute_returns.Rdata")
load(file="XMR_30minute_returns.Rdata")
load(file="XMR_60minute_returns.Rdata")
load(file="XMR_day_returns.Rdata")

zero_ticks$XMR[1] <- length(which(minute_data$logreturn_price_all==0))/nrow(minute_data)
zero_ticks$XMR[2] <- length(which(five_minute_data$logreturn_price_all==0))/nrow(five_minute_data)
zero_ticks$XMR[3] <- length(which(fifteen_minute_data$logreturn_price_all==0))/nrow(fifteen_minute_data)
zero_ticks$XMR[4] <- length(which(thirty_minute_data$logreturn_price_all==0))/nrow(thirty_minute_data)
zero_ticks$XMR[5] <- length(which(hour_data$logreturn_price_all==0))/nrow(hour_data)
zero_ticks$XMR[6] <- length(which(day_data$logreturn_price_all==0))/nrow(day_data)

zero_search$XMR[1] <- length(which(minute_data$SVI==0))/nrow(minute_data)
zero_search$XMR[2] <- length(which(five_minute_data$SVI==0))/nrow(five_minute_data)
zero_search$XMR[3] <- length(which(fifteen_minute_data$SVI==0))/nrow(fifteen_minute_data)
zero_search$XMR[4] <- length(which(thirty_minute_data$SVI==0))/nrow(thirty_minute_data)
zero_search$XMR[5] <- length(which(hour_data$SVI==0))/nrow(hour_data)
zero_search$XMR[6] <- length(which(day_data$SVI==0))/nrow(day_data)

# 5) ******* plotting ***********************************************
par(mfrow=c(2,2))
plot(zero_ticks$XBT,xaxt = "n", xlab='Granularity' , ylab = "Share of zero ticks", main="XBT", ylim = c(0,0.6), type = "p", col = "black", pch = 19, cex = 1.5, bty="n")
axis(1,at = 1:6, labels = c("1 min","5 mins","15 mins","30 mins","hour","day"))

plot(zero_ticks$ETH,xaxt = "n", xlab='Granularity' , ylab = "Share of zero ticks", main="ETH", ylim = c(0,0.6), type = "p", col = "black", pch = 19, cex = 1.5, bty="n")
axis(1,at = 1:6, labels = c("1 min","5 mins","15 mins","30 mins","hour","day"))

plot(zero_ticks$LTC,xaxt = "n", xlab='Granularity' , ylab = "Share of zero ticks", main="LTC", ylim = c(0,0.6), type = "p", col = "black", pch = 19, cex = 1.5, bty="n")
axis(1,at = 1:6, labels = c("1 min","5 mins","15 mins","30 mins","hour","day"))

plot(zero_ticks$XMR,xaxt = "n", xlab='Granularity' , ylab = "Share of zero ticks", main="XMR", ylim = c(0,0.6), type = "p", col = "black", pch = 19, cex = 1.5, bty="n")
axis(1,at = 1:6, labels = c("1 min","5 mins","15 mins","30 mins","hour","day"))

par(mfrow=c(2,2))
plot(zero_search$XBT,xaxt = "n", xlab='Granularity' , ylab = "Share of zero search", main="bitcoin", ylim = c(0,0.02), type = "p", col = "black", pch = 19, cex = 1.5, bty="n")
axis(1,at = 1:6, labels = c("1 min","5 mins","15 mins","30 mins","hour","day"))

plot(zero_search$ETH,xaxt = "n", xlab='Granularity' , ylab = "Share of zero search", main="ethereum", ylim = c(0,0.02), type = "p", col = "black", pch = 19, cex = 1.5, bty="n")
axis(1,at = 1:6, labels = c("1 min","5 mins","15 mins","30 mins","hour","day"))

plot(zero_search$LTC,xaxt = "n", xlab='Granularity' , ylab = "Share of zero search", main="litecoin", ylim = c(0,0.02), type = "p", col = "black", pch = 19, cex = 1.5, bty="n")
axis(1,at = 1:6, labels = c("1 min","5 mins","15 mins","30 mins","hour","day"))

plot(zero_search$XMR,xaxt = "n", xlab='Granularity' , ylab = "Share of zero search", main="monero", ylim = c(0,0.02), type = "p", col = "black", pch = 19, cex = 1.5, bty="n")
axis(1,at = 1:6, labels = c("1 min","5 mins","15 mins","30 mins","hour","day"))