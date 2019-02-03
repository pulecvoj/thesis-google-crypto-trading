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
library(sparsevar)
library(vars)


# *** loading data ****************************
load(file="XBT_15minute_returns.Rdata")

# 1) Splitting the attention
fifteen_minute_data$Positive <- 0
fifteen_minute_data$Negative <- 0
fifteen_minute_data$Mixed <- 0


# *** adding XY trend ***********************************************
#generating the data structure
trend <- as.data.frame(matrix(1, nrow = nrow(fifteen_minute_data), ncol = 2))
colnames(trend) <- c("time", "ret")

#getting trend (daily)
for(i in 96:nrow(fifteen_minute_data)){
  trend$time[i] <- fifteen_minute_data$time[i]
  trend$ret[i] <- log(fifteen_minute_data$price_all[i]/fifteen_minute_data$price_all[i-60])
  print(paste(i, "getting_ret"))
}

sum_ret <- summary(trend$ret, na.rm = TRUE)
lower_bound <- as.numeric(sum_ret[2])
upper_bound <- as.numeric(sum_ret[5])

#trend deciding mechanism
for(i in 1:nrow(fifteen_minute_data)){
  if(trend$ret[i] < lower_bound)
  {trend$trend[i] <- -1}
  else if (trend$ret[i] > upper_bound) 
  {trend$trend[i] <- 1}
  else 
  {trend$trend[i] <- 0}
  print(paste(i, "getting_trend"))
}


#looping to assign trend to time stamp
for (i in 1:nrow(fifteen_minute_data)) {
  fifteen_minute_data$trend[i] <- trend$trend[match(fifteen_minute_data$time[i], trend$time)]
  print(paste(i, "assigning"))
}

#splitting search into positive and negative
for (i in 96:nrow(fifteen_minute_data)) {
  if(fifteen_minute_data$trend[i] < 0)
  {fifteen_minute_data$Negative[i] <- fifteen_minute_data$SVI_log_diff[i]}
  else
  {fifteen_minute_data$Negative[i] <- 0}
  
  if(fifteen_minute_data$trend[i]> 0) 
  {fifteen_minute_data$Positive[i] <- fifteen_minute_data$SVI_log_diff[i]}
  else
  {fifteen_minute_data$Positive[i] <-0}
  
  if(fifteen_minute_data$trend[i] == 0) 
  {fifteen_minute_data$Mixed[i] <- fifteen_minute_data$SVI_log_diff[i]}
  else
  {fifteen_minute_data$Mixed[i] <-0}
  print(paste(i, "splitting"))
}
save(trend,file="XBT_15minute_trend_daily_mix_quartiles.Rdata")
save(fifteen_minute_data,file="XBT_15minute_returns_daily_mix_quartiles.Rdata")

