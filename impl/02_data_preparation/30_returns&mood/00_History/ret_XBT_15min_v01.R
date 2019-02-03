setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd) 
rm(list=ls())

library("tseries")
library("zoo")
library("forecast")
library("ggplot2")
library('stats')
library("gridExtra")
library("reshape2")
library("tcltk")
library("sarima")


# *** loading data ****************************
load(file = "XBT_15minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(fifteen_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(fifteen_minute_data)[i]
  nr_nas_1[i,2] <- nrow(fifteen_minute_data)
  nr_nas_1[i,3] <- sum(is.na(fifteen_minute_data[,i])) + sum(fifteen_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(fifteen_minute_data) <- c("time", "SVI", "price_all",
                                   "price_ba","price_bm", "price_bl",
                                   "price_sa","price_sm", "price_sl",
                                   "volume_all",
                                   "volume_ba","volume_bm","volume_bl",
                                   "volume_sa","volume_sm","volume_sl")

fifteen_minute_data <- na.locf(fifteen_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
fifteen_minute_data$return_price_all <-NA
fifteen_minute_data$logreturn_price_all <- NA
fifteen_minute_data$SVI_diff <- NA
fifteen_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$return_price_all[i] <- (fifteen_minute_data$price_all[i] -  fifteen_minute_data$price_all[i-1])/fifteen_minute_data$price_all[i-1]
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$logreturn_price_all[i] <- log(fifteen_minute_data$price_all[i] / fifteen_minute_data$price_all[i-1])
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$SVI_diff[i] <- (fifteen_minute_data$SVI[i] -  fifteen_minute_data$SVI[i-1])/fifteen_minute_data$SVI[i-1]
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$SVI_log_diff[i] <- log(fifteen_minute_data$SVI[i] / fifteen_minute_data$SVI[i-1])
}


# subsetting
fifteen_minute_data <- fifteen_minute_data[-1,]


save(fifteen_minute_data,file="XBT_15minute_returns.Rdata")