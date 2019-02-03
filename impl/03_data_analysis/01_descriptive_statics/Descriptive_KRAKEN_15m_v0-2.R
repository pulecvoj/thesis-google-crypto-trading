setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd) 
rm(list=ls())

library(tseries)
library(moments)
library(ggplot2)

# 1) *** loading data ****************************
load(file="XBT_15minute_returns.Rdata")

# 2)***** stats ***************
nrow(fifteen_minute_data$logreturn_price_all)
mean(fifteen_minute_data$logreturn_price_all)
min(fifteen_minute_data$logreturn_price_all)
max(fifteen_minute_data$logreturn_price_all)
kurtosis(fifteen_minute_data$logreturn_price_all)
skewness(fifteen_minute_data$logreturn_price_all)
jarque.bera.test(fifteen_minute_data$logreturn_price_all)

histogram <- as.data.frame(matrix(data = 1, nrow = nrow(fifteen_minute_data), ncol = 4))
histogram <- colnames(c("XBT", "ETH", "LTC", "XMR"))                        
histogram$XBT <- fifteen_minute_data$logreturn_price_all

# 3) ******* going to eth ****************
setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
setwd(wd) 

load(file="ETH_15minute_returns.Rdata")

# 4)***** stats ***************
nrow(fifteen_minute_data$logreturn_price_all)
mean(fifteen_minute_data$logreturn_price_all)
min(fifteen_minute_data$logreturn_price_all)
max(fifteen_minute_data$logreturn_price_all)
kurtosis(fifteen_minute_data$logreturn_price_all)
skewness(fifteen_minute_data$logreturn_price_all)
jarque.bera.test(fifteen_minute_data$logreturn_price_all)

histogram$ETH <- fifteen_minute_data$logreturn_price_all

# 5) ******* going to eth ****************
setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
setwd(wd) 

load(file="LTC_15minute_returns.Rdata")

# 6)***** stats ***************
nrow(fifteen_minute_data$logreturn_price_all)
mean(fifteen_minute_data$logreturn_price_all)
min(fifteen_minute_data$logreturn_price_all)
max(fifteen_minute_data$logreturn_price_all)
kurtosis(fifteen_minute_data$logreturn_price_all)
skewness(fifteen_minute_data$logreturn_price_all)
jarque.bera.test(fifteen_minute_data$logreturn_price_all)

histogram$LTC <- fifteen_minute_data$logreturn_price_all

# 7) ******* going to eth ****************
setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
setwd(wd) 

load(file="XMR_15minute_returns.Rdata")

# 6)***** stats ***************
nrow(fifteen_minute_data$logreturn_price_all)
mean(fifteen_minute_data$logreturn_price_all)
min(fifteen_minute_data$logreturn_price_all)
max(fifteen_minute_data$logreturn_price_all)
kurtosis(fifteen_minute_data$logreturn_price_all)
skewness(fifteen_minute_data$logreturn_price_all)
jarque.bera.test(fifteen_minute_data$logreturn_price_all)

histogram$XMR <- fifteen_minute_data$logreturn_price_all

histogram <- as.data.frame(histogram)

par(mfrow=c(2,2))
hist(histogram$XBT,
     main="XBT", 
     xlab="15 minutes returns", 
     border="black", 
     col="gray",
     xlim=c(-0.02,0.02),
     ylim = c(0,5000),
     las=1, 
     breaks=200)
hist(histogram$ETH,
     main="ETH", 
     xlab="15 minutes returns", 
     border="black", 
     col="gray",
     xlim=c(-0.02,0.02),
     ylim = c(0,5000),
     las=1, 
     breaks=300)
hist(histogram$LTC,
     main="LTC", 
     xlab="15 minutes returns", 
     border="black", 
     col="gray",
     xlim=c(-0.02,0.02),
     ylim = c(0,5000),
     las=1, 
     breaks=300)
hist(histogram$XMR,
     main="XMR", 
     xlab="15 minutes returns", 
     border="black", 
     col="gray",
     xlim=c(-0.02,0.02),
     ylim = c(0,5000),
     las=1, 
     breaks=300)

