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
load(file="ETH_5minute_returns.Rdata")

# 1) Splitting the attention
five_minute_data$Positive_Dum <- 0
five_minute_data$Negative_Dum <- 0
five_minute_data$Mixed_Dum <- 0


# *** adding XY trend ***********************************************
#generating the data structure
trend <- as.data.frame(matrix(1, nrow = nrow(five_minute_data), ncol = 2))
colnames(trend) <- c("time", "ret")

#getting trend (daily)
for(i in 289:nrow(five_minute_data)){
  trend$time[i] <- five_minute_data$time[i]
  trend$ret[i] <- log(five_minute_data$crix[i]/five_minute_data$crix[i-288])
  print(paste(i, "getting_ret"))
}

sum_ret <- summary(trend$ret, na.rm = TRUE)
lower_bound <- as.numeric(sum_ret[2])
upper_bound <- as.numeric(sum_ret[5])

for(i in 1:nrow(five_minute_data)){
  if(trend$ret[i] < lower_bound)
  {trend$trend[i] <- -1}
  else if (trend$ret[i] > upper_bound) 
  {trend$trend[i] <- 1}
  else 
  {trend$trend[i] <- 0}
  print(paste(i, "getting_trend"))
}

#looping to assign trend to time stamp
for (i in 1:nrow(five_minute_data)) {
  five_minute_data$trend[i] <- trend$trend[match(five_minute_data$time[i], trend$time)]
  print(paste(i, "assigning"))
}

#structure for splitting
five_minute_data$search_negative <- NA
five_minute_data$search_positive <- NA
five_minute_data$search_mixed <- NA

#splitting search into positive and negative
for (i in 61:nrow(five_minute_data)) {
  if(five_minute_data$trend[i] < 0)
  {five_minute_data$search_negative[i] <- five_minute_data$SVI_log_diff[i]}
  else
  {five_minute_data$search_negative[i] <- 0}
  
  if(five_minute_data$trend[i]> 0) 
  {five_minute_data$search_positive[i] <- five_minute_data$SVI_log_diff[i]}
  else
  {five_minute_data$search_positive[i] <-0}
  
  if(five_minute_data$trend[i] == 0) 
  {five_minute_data$search_mixed[i] <- five_minute_data$SVI_log_diff[i]}
  else
  {five_minute_data$search_mixed[i] <-0}
  print(paste(i, "splitting"))
}

save(five_minute_data,file="ETH_1five_minute_returns_hourly_diff.Rdata")