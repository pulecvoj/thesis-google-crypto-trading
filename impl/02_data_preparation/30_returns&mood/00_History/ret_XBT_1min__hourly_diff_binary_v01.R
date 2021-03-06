setwd("C:/Users/Vojt�ch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
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
load(file="XBT_1minute_returns.Rdata")

# 1) Splitting the attention
minute_data$Positive <- 0
minute_data$Negative <- 0
minute_data$Mixed <- 0


# *** adding XY trend ***********************************************
#generating the data structure
trend <- as.data.frame(matrix(1, nrow = nrow(minute_data), ncol = 2))
colnames(trend) <- c("time", "ret")

#getting trend (hourly)
for(i in 61:nrow(minute_data)){
  trend$time[i] <- minute_data$time[i]
  trend$ret[i] <- log(minute_data$price_all[i]/minute_data$price_all[i-60])
  print(paste(i, "getting_ret"))
}

# sum_ret <- summary(trend$ret, na.rm = TRUE)
# lower_bound <- as.numeric(sum_ret[2])
# upper_bound <- as.numeric(sum_ret[5])

#trend deciding mechanism
for(i in 1:nrow(minute_data)){
  if(trend$ret[i] < 0)
  {trend$trend[i] <- -1}
  else 
  {trend$trend[i] <- 1}
  print(paste(i, "getting_trend"))
}

#looping to assign trend to time stamp
for (i in 1:nrow(minute_data)) {
  minute_data$trend[i] <- trend$trend[match(minute_data$time[i], trend$time)]
  print(paste(i, "assigning"))
}

#splitting search into positive and negative
for (i in 61:nrow(minute_data)) {
  if(minute_data$trend[i] < 0)
  {minute_data$Negative[i] <- minute_data$SVI_log_diff[i]}
  else
  {minute_data$Negative[i] <- 0}
  
  if(minute_data$trend[i]> 0) 
  {minute_data$Positive[i] <- minute_data$SVI_log_diff[i]}
  else
  {minute_data$Positive[i] <-0}
  
  if(minute_data$trend[i] == 0) 
  {minute_data$Mixed[i] <- minute_data$SVI_log_diff[i]}
  else
  {minute_data$Mixed[i] <-0}
  print(paste(i, "splitting"))
}
save(trend,file="XBT_1minute_trend_hourly_diff_binary.Rdata")
save(minute_data,file="XBT_1minute_returns_hourly_diff_binary.Rdata")

sbst <- as.matrix(minute_data[ , c(2,3)])
summary(ca.jo(sbst))

