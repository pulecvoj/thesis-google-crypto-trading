rm(list=ls())
clist <- c("XBT", "ETH", "XMR", "LTC")
for(w in clist){
setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/", w, sep = "")
setwd(wd) 


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
load(file=paste(w,"_60minute_returns.Rdata",sep = ""))

# 1) Splitting the attention
hour_data$positive_Dum <- 0
hour_data$negative_Dum <- 0
hour_data$mixed_Dum <- 0


# *** adding XY trend ***********************************************
#generating the data structure
trend <- as.data.frame(matrix(0, nrow = nrow(hour_data), ncol = 2))
colnames(trend) <- c("time", "ret")

#getting trend (daily)
for(i in 25:nrow(hour_data)){
  trend$time[i] <- hour_data$time[i]
  trend$ret[i] <- log(hour_data$crix[i]/hour_data$crix[i-24])
  print(paste(i, "getting_ret", w))
}

sum_ret <- summary(trend$ret, na.rm = TRUE)
lower_bound <- as.numeric(sum_ret[2])
upper_bound <- as.numeric(sum_ret[5])

for(i in 1:nrow(hour_data)){
  if(trend$ret[i] <= 0)
  {trend$trend[i] <- -1}
  else if (trend$ret[i] > 0) 
  {trend$trend[i] <- 1}
  else 
  {trend$trend[i] <- 0}
  print(paste(i, "getting_trend", w))
}

#looping to assign trend to time stamp
for (i in 1:nrow(hour_data)) {
  hour_data$trend[i] <- trend$trend[match(hour_data$time[i], trend$time)]
  print(paste(i, "assigning", w))
}

#splitting search into positive and negative
for (i in 25:nrow(hour_data)) {
  if(hour_data$trend[i] < 0)
  {hour_data$negative_Dum[i] <- 1
  }
  else
  {hour_data$negative_Dum[i] <- 0}
  
  if(hour_data$trend[i]> 0) 
  {hour_data$positive_Dum[i] <- 1}
  else
  {hour_data$positive_Dum[i] <-0}
  
  if(hour_data$trend[i] == 0) 
  {hour_data$mixed_Dum[i] <- 1}
  else
  {hour_data$mixed_Dum[i] <-0}
  print(paste(i, "splitting", w))
}

hour_data$search_negative <- hour_data$negative_Dum*hour_data$SVI_log_diff
hour_data$search_positive <- hour_data$positive_Dum*hour_data$SVI_log_diff
hour_data$search_mixed <- hour_data$mixed_Dum*hour_data$SVI_log_diff

save(hour_data,file=paste(w,"_60minute_returns_bin.Rdata",sep = ""))
}