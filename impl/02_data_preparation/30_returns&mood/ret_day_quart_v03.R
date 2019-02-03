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
load(file=paste(w,"_day_returns.Rdata",sep = ""))

# 1) Splitting the attention
day_data$positive_Dum <- 0
day_data$negative_Dum <- 0
day_data$mixed_Dum <- 0


# *** adding XY trend ***********************************************
#generating the data structure
trend <- as.data.frame(matrix(0, nrow = nrow(day_data), ncol = 2))
colnames(trend) <- c("time", "ret")

#getting trend (daily)
for(i in 8:nrow(day_data)){
  trend$time[i] <- day_data$time[i]
  trend$ret[i] <- log(day_data$crix[i]/day_data$crix[i-7])
  print(paste(i, "getting_ret", w))
}

sum_ret <- summary(trend$ret, na.rm = TRUE)
lower_bound <- as.numeric(sum_ret[2])
upper_bound <- as.numeric(sum_ret[5])

for(i in 1:nrow(day_data)){
  if(trend$ret[i] < lower_bound)
  {trend$trend[i] <- -1}
  else if (trend$ret[i] > upper_bound) 
  {trend$trend[i] <- 1}
  else 
  {trend$trend[i] <- 0}
  print(paste(i, "getting_trend", w))
}

#looping to assign trend to time stamp
for (i in 1:nrow(day_data)) {
  day_data$trend[i] <- trend$trend[match(day_data$time[i], trend$time)]
  print(paste(i, "assigning", w))
}

#splitting search into positive and negative
for (i in 8:nrow(day_data)) {
  if(day_data$trend[i] < 0)
  {day_data$negative_Dum[i] <- 1
  }
  else
  {day_data$negative_Dum[i] <- 0}
  
  if(day_data$trend[i]> 0) 
  {day_data$positive_Dum[i] <-1}
  else
  {day_data$positive_Dum[i] <-0}
  
  if(day_data$trend[i] == 0) 
  {day_data$mixed_Dum[i] <- 1}
  else
  {day_data$mixed_Dum[i] <-0}
  print(paste(i, "splitting",w))
}

day_data$search_negative <- day_data$negative_Dum*day_data$SVI_log_diff
day_data$search_positive <- day_data$positive_Dum*day_data$SVI_log_diff
day_data$search_mixed <- day_data$mixed_Dum*day_data$SVI_log_diff

save(day_data,file=paste(w,"_day_returns_quart.Rdata",sep = ""))
}