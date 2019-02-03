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
library("dplyr")
library("imputeTS")


# *** loading data ****************************
load(file = "ETH_15minute_CRIX.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(fifteen_minute_data), ncol = 3))
for(i in 1:17){
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
                                   "volume_sa","volume_sm","volume_sl","crix")

fifteen_minute_data$crix <- na.interpolation(fifteen_minute_data$crix, option = "linear")

fifteen_minute_data <- na.locf(fifteen_minute_data)

#2 ) creating returns and log returns and their squares
fifteen_minute_data$return_price_all <-NA
fifteen_minute_data$logreturn_price_all <- NA
fifteen_minute_data$SVI_diff <- NA
fifteen_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$return_price_all[i] <- (fifteen_minute_data$price_all[i] -  fifteen_minute_data$price_all[i-1])/fifteen_minute_data$price_all[i-1]
  print(paste(i, "return_price_all"))
  }

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$logreturn_price_all[i] <- log(fifteen_minute_data$price_all[i] / fifteen_minute_data$price_all[i-1])
  print(paste(i, "log_return_price_all"))
  }

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$SVI_diff[i] <- (fifteen_minute_data$SVI[i] -  fifteen_minute_data$SVI[i-1])/fifteen_minute_data$SVI[i-1]
  print(paste(i, "SVI_diff"))
  }

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$SVI_log_diff[i] <- log(fifteen_minute_data$SVI[i] / fifteen_minute_data$SVI[i-1])
  print(paste(i, "SVI_log_dif"))
  }


# subsetting
fifteen_minute_data <- fifteen_minute_data[-1,]


save(fifteen_minute_data,file="ETH_15minute_returns.Rdata")