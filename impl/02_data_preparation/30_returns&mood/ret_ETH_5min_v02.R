setwd("C:/Users/Vojt�ch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
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
load(file = "ETH_5minute_CRIX.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(five_minute_data), ncol = 3))
for(i in 1:17){
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
                                "volume_sa","volume_sm","volume_sl", "crix")

five_minute_data$crix <- na.interpolation(five_minute_data$crix, option = "linear")

five_minute_data <- na.locf(five_minute_data)

#2 ) creating returns and log returns and their squares
five_minute_data$return_price_all <-NA
five_minute_data$logreturn_price_all <- NA
five_minute_data$SVI_diff <- NA
five_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(five_minute_data)){
  five_minute_data$return_price_all[i] <- (five_minute_data$price_all[i] -  five_minute_data$price_all[i-1])/five_minute_data$price_all[i-1]
  print(paste(i, "return_price_all"))
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$logreturn_price_all[i] <- log(five_minute_data$price_all[i] / five_minute_data$price_all[i-1])
  print(paste(i, "log_return_price_all"))
  }

for (i in 2:nrow(five_minute_data)){
  five_minute_data$SVI_diff[i] <- (five_minute_data$SVI[i] -  five_minute_data$SVI[i-1])/five_minute_data$SVI[i-1]
  print(paste(i, "SVI_diff"))
  }

for (i in 2:nrow(five_minute_data)){
  five_minute_data$SVI_log_diff[i] <- log(five_minute_data$SVI[i] / five_minute_data$SVI[i-1])
  print(paste(i, "SVI_log_diff"))
  }


# subsetting
five_minute_data <- five_minute_data[-1,]


save(five_minute_data,file="ETH_5minute_returns.Rdata")