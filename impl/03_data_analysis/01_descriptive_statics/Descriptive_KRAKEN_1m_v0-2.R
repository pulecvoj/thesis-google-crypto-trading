setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd) 
rm(list=ls())

library(tseries)
library(moments)
library(ggplot2)

# 1) *** loading data ****************************
load(file="XBT_1minute_returns.Rdata")

# 2)***** stats ***************
nrow(minute_data$logreturn_price_all)
mean(minute_data$logreturn_price_all)
min(minute_data$logreturn_price_all)
max(minute_data$logreturn_price_all)
kurtosis(minute_data$logreturn_price_all)
skewness(minute_data$logreturn_price_all)
jarque.bera.test(minute_data$logreturn_price_all)

histogram <- as.data.frame(matrix(data = 1, nrow = nrow(minute_data), ncol = 4))
histogram <- colnames(c("XBT", "ETH", "LTC", "XMR"))                        
histogram$XBT <- minute_data$logreturn_price_all

# 3) ******* going to eth ****************
setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
setwd(wd) 

load(file="ETH_1minute_returns.Rdata")

# 4)***** stats ***************
nrow(minute_data$logreturn_price_all)
mean(minute_data$logreturn_price_all)
min(minute_data$logreturn_price_all)
max(minute_data$logreturn_price_all)
kurtosis(minute_data$logreturn_price_all)
skewness(minute_data$logreturn_price_all)
jarque.bera.test(minute_data$logreturn_price_all)

histogram$ETH <- minute_data$logreturn_price_all

# 5) ******* going to eth ****************
setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
setwd(wd) 

load(file="LTC_1minute_returns.Rdata")

# 6)***** stats ***************
nrow(minute_data$logreturn_price_all)
mean(minute_data$logreturn_price_all)
min(minute_data$logreturn_price_all)
max(minute_data$logreturn_price_all)
kurtosis(minute_data$logreturn_price_all)
skewness(minute_data$logreturn_price_all)
jarque.bera.test(minute_data$logreturn_price_all)

histogram$LTC <- minute_data$logreturn_price_all

# 7) ******* going to eth ****************
setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
setwd(wd) 

load(file="XMR_1minute_returns.Rdata")

# 6)***** stats ***************
nrow(minute_data$logreturn_price_all)
mean(minute_data$logreturn_price_all, na.rm = TRUE)
min(minute_data$logreturn_price_all, na.rm = TRUE)
max(minute_data$logreturn_price_all, na.rm = TRUE)
kurtosis(minute_data$logreturn_price_all, na.rm = TRUE)
skewness(minute_data$logreturn_price_all, na.rm = TRUE)
temp <- minute_data$logreturn_price_all[!is.na(minute_data$logreturn_price_all)]
jarque.bera.test(temp)

histogram$XMR <- minute_data$logreturn_price_all

histogram <- as.data.frame(histogram)

par(mfrow=c(2,2))
hist(histogram$XBT,
     main="XBT", 
     xlab="1 minute returns", 
     ylab = "",
     border="black", 
     col="gray",
     xlim=c(-0.005,0.005),
     ylim = c(0,175000),
     las=1, 
     breaks=2500)
hist(histogram$ETH,
     main="ETH", 
     xlab="1 minute returns", 
     ylab = "",
     border="black", 
     col="gray",
     xlim=c(-0.005,0.005),
     ylim = c(0,175000),
     las=1, 
     breaks= 1000)
hist(histogram$LTC,
     main="LTC", 
     xlab="1 minute returns", 
     ylab = "",
     border="black", 
     col="gray",
     xlim=c(-0.005,0.005),
     ylim = c(0,175000),
     las=1, 
     breaks=500)
hist(histogram$XMR,
     main="XMR", 
     xlab="1 minute returns", 
     ylab = "",
     border="black", 
     col="gray",
     xlim=c(-0.005,0.005),
     ylim = c(0,175000),
     las=1, 
     breaks=500)

