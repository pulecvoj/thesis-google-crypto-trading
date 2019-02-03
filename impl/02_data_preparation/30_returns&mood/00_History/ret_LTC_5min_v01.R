setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
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
load(file = "LTC_5minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(five_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(five_minute_data)[i]
  nr_nas_1[i,2] <- nrow(five_minute_data)
  nr_nas_1[i,3] <- sum(is.na(five_minute_data[,i])) + sum(five_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(five_minute_data) <- c("time", "SVI", "price_all",
                                "price_ba","price_bm", "price_bl",
                                "price_sa","price_sm", "price_sl",
                                "volume_all",
                                "volume_ba","volume_bm","volume_bl",
                                "volume_sa","volume_sm","volume_sl")

five_minute_data <- na.locf(five_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
five_minute_data$return_price_all <-NA
five_minute_data$logreturn_price_all <- NA
five_minute_data$SVI_diff <- NA
five_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(five_minute_data)){
  five_minute_data$return_price_all[i] <- (five_minute_data$price_all[i] -  five_minute_data$price_all[i-1])/five_minute_data$price_all[i-1]
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$logreturn_price_all[i] <- log(five_minute_data$price_all[i] / five_minute_data$price_all[i-1])
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$SVI_diff[i] <- (five_minute_data$SVI[i] -  five_minute_data$SVI[i-1])/five_minute_data$SVI[i-1]
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$SVI_log_diff[i] <- log(five_minute_data$SVI[i] / five_minute_data$SVI[i-1])
}


# subsetting
five_minute_data <- five_minute_data[-1,]


save(five_minute_data,file="LTC_5minute_returns.Rdata")