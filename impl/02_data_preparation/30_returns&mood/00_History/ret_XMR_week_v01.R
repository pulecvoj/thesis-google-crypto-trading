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


# *** loading data ****************************
load(file = "XMR_week.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(week_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(week_data)[i]
  nr_nas_1[i,2] <- nrow(week_data)
  nr_nas_1[i,3] <- sum(is.na(week_data[,i])) + sum(week_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(week_data) <- c("time", "SVI", "price_all",
                        "price_ba","price_bm", "price_bl",
                        "price_sa","price_sm", "price_sl",
                        "volume_all",
                        "volume_ba","volume_bm","volume_bl",
                        "volume_sa","volume_sm","volume_sl")

week_data <- na.locf(week_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
week_data$return_price_all <-NA
week_data$logreturn_price_all <- NA
week_data$SVI_diff <- NA
week_data$SVI_log_diff <- NA

for (i in 2:nrow(week_data)){
  week_data$return_price_all[i] <- (week_data$price_all[i] -  week_data$price_all[i-1])/week_data$price_all[i-1]
}

for (i in 2:nrow(week_data)){
  week_data$logreturn_price_all[i] <- log(week_data$price_all[i] / week_data$price_all[i-1])
}

for (i in 2:nrow(week_data)){
  week_data$SVI_diff[i] <- (week_data$SVI[i] -  week_data$SVI[i-1])/week_data$SVI[i-1]
}

for (i in 2:nrow(week_data)){
  week_data$SVI_log_diff[i] <- log(week_data$SVI[i] / week_data$SVI[i-1])
}


# subsetting
week_data <- week_data[-1,]


save(week_data,file="XMR_week_returns.Rdata")