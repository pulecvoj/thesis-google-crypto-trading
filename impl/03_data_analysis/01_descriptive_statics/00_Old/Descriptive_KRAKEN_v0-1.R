setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd) 
rm(list=ls())

library(tseries)
library(moments)

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

# 2)***** stats ***************
nrow(fifteen_minute_data$logreturn_price_all)
mean(fifteen_minute_data$logreturn_price_all)
min(fifteen_minute_data$logreturn_price_all)
max(fifteen_minute_data$logreturn_price_all)
kurtosis(fifteen_minute_data$logreturn_price_all)
skewness(fifteen_minute_data$logreturn_price_all)
jarque.bera.test(fifteen_minute_data$logreturn_price_all)