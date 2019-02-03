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
library("dplyr")
library("imputeTS")


# *** loading data ****************************
load(file = "XBT_day_CRIX.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(day_data), ncol = 3))
for(i in 1:17){
  nr_nas_1[i,1] <- colnames(day_data)[i]
  nr_nas_1[i,2] <- nrow(day_data)
  nr_nas_1[i,3] <- sum(is.na(day_data[,i])) + sum(day_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(day_data) <- c("time", "SVI", "price_all",
                        "price_ba","price_bm", "price_bl",
                        "price_sa","price_sm", "price_sl",
                        "volume_all",
                        "volume_ba","volume_bm","volume_bl",
                        "volume_sa","volume_sm","volume_sl", "crix")

day_data$crix <- na.interpolation(day_data$crix, option = "linear")

day_data <- na.locf(day_data)

#2 ) creating returns and log returns and their squares
day_data$return_price_all <-NA
day_data$logreturn_price_all <- NA
day_data$SVI_diff <- NA
day_data$SVI_log_diff <- NA

for (i in 2:nrow(day_data)){
  day_data$return_price_all[i] <- (day_data$price_all[i] -  day_data$price_all[i-1])/day_data$price_all[i-1]
}

for (i in 2:nrow(day_data)){
  day_data$logreturn_price_all[i] <- log(day_data$price_all[i] / day_data$price_all[i-1])
}

for (i in 2:nrow(day_data)){
  day_data$SVI_diff[i] <- (day_data$SVI[i] -  day_data$SVI[i-1])/day_data$SVI[i-1]
}

for (i in 2:nrow(day_data)){
  day_data$SVI_log_diff[i] <- log(day_data$SVI[i] / day_data$SVI[i-1])
}


# subsetting
day_data <- day_data[-1,]


save(day_data,file="XBT_day_returns.Rdata")