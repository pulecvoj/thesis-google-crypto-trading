setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
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
load(file = "ETH_60minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(hour_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(hour_data)[i]
  nr_nas_1[i,2] <- nrow(hour_data)
  nr_nas_1[i,3] <- sum(is.na(hour_data[,i])) + sum(hour_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(hour_data) <- c("time", "SVI", "price_all",
                         "price_ba","price_bm", "price_bl",
                         "price_sa","price_sm", "price_sl",
                         "volume_all",
                         "volume_ba","volume_bm","volume_bl",
                         "volume_sa","volume_sm","volume_sl")

hour_data <- na.locf(hour_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
hour_data$return_price_all <-NA
hour_data$logreturn_price_all <- NA
hour_data$SVI_diff <- NA
hour_data$SVI_log_diff <- NA

for (i in 2:nrow(hour_data)){
  hour_data$return_price_all[i] <- (hour_data$price_all[i] -  hour_data$price_all[i-1])/hour_data$price_all[i-1]
}

for (i in 2:nrow(hour_data)){
  hour_data$logreturn_price_all[i] <- log(hour_data$price_all[i] / hour_data$price_all[i-1])
}

for (i in 2:nrow(hour_data)){
  hour_data$SVI_diff[i] <- (hour_data$SVI[i] -  hour_data$SVI[i-1])/hour_data$SVI[i-1]
}

for (i in 2:nrow(hour_data)){
  hour_data$SVI_log_diff[i] <- log(hour_data$SVI[i] / hour_data$SVI[i-1])
}


# subsetting
hour_data <- hour_data[-1,]


save(hour_data,file="ETH_60minute_returns.Rdata")