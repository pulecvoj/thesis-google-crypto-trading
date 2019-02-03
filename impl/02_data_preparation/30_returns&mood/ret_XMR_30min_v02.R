setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
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
load(file = "XMR_30minute_CRIX.RData")

#1) filling NAs - taking last valid price - zoo na.locf and linear interpolation for CRIX
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(thirty_minute_data), ncol = 3))
for(i in 1:17){
  nr_nas_1[i,1] <- colnames(thirty_minute_data)[i]
  nr_nas_1[i,2] <- nrow(thirty_minute_data)
  nr_nas_1[i,3] <- sum(is.na(thirty_minute_data[,i])) + sum(thirty_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(thirty_minute_data) <- c("time", "SVI", "price_all",
                                   "price_ba","price_bm", "price_bl",
                                   "price_sa","price_sm", "price_sl",
                                   "volume_all",
                                   "volume_ba","volume_bm","volume_bl",
                                   "volume_sa","volume_sm","volume_sl", "crix")

thirty_minute_data$crix <- na.interpolation(thirty_minute_data$crix, option = "linear")

thirty_minute_data <- na.locf(thirty_minute_data)

#2 ) creating returns and log returns and their squares
thirty_minute_data$return_price_all <-NA
thirty_minute_data$logreturn_price_all <- NA
thirty_minute_data$SVI_diff <- NA
thirty_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$return_price_all[i] <- (thirty_minute_data$price_all[i] -  thirty_minute_data$price_all[i-1])/thirty_minute_data$price_all[i-1]
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$logreturn_price_all[i] <- log(thirty_minute_data$price_all[i] / thirty_minute_data$price_all[i-1])
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$SVI_diff[i] <- (thirty_minute_data$SVI[i] -  thirty_minute_data$SVI[i-1])/thirty_minute_data$SVI[i-1]
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$SVI_log_diff[i] <- log(thirty_minute_data$SVI[i] / thirty_minute_data$SVI[i-1])
}


# subsetting
thirty_minute_data <- thirty_minute_data[-1,]


save(thirty_minute_data,file="XMR_30minute_returns.Rdata")