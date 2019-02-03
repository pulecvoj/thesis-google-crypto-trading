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
load(file=paste(w,"_30minute_returns.Rdata",sep = ""))

# 1) Splitting the attention
thirty_minute_data$positive_Dum <- 0
thirty_minute_data$negative_Dum <- 0
thirty_minute_data$mixed_Dum <- 0


# *** adding XY trend ***********************************************
#generating the data structure
trend <- as.data.frame(matrix(0, nrow = nrow(thirty_minute_data), ncol = 2))
colnames(trend) <- c("time", "ret")

#getting trend (daily)
for(i in 49:nrow(thirty_minute_data)){
  trend$time[i] <- thirty_minute_data$time[i]
  trend$ret[i] <- log(thirty_minute_data$crix[i]/thirty_minute_data$crix[i-48])
  print(paste(i, "getting_ret", w))
}

sum_ret <- summary(trend$ret, na.rm = TRUE)
lower_bound <- as.numeric(sum_ret[2])
upper_bound <- as.numeric(sum_ret[5])

for(i in 1:nrow(thirty_minute_data)){
  if(trend$ret[i] <= 0)
  {trend$trend[i] <- -1}
  else if (trend$ret[i] > 0) 
  {trend$trend[i] <- 1}
  else 
  {trend$trend[i] <- 0}
  print(paste(i, "getting_trend", w))
}

#looping to assign trend to time stamp
for (i in 1:nrow(thirty_minute_data)) {
  thirty_minute_data$trend[i] <- trend$trend[match(thirty_minute_data$time[i], trend$time)]
  print(paste(i, "assigning", w))
}

#splitting search into positive and negative
for (i in 49:nrow(thirty_minute_data)) {
  if(thirty_minute_data$trend[i] < 0)
  {thirty_minute_data$negative_Dum[i] <- 1
  }
  else
  {thirty_minute_data$negative_Dum[i] <- 0}
  
  if(thirty_minute_data$trend[i]> 0) 
  {thirty_minute_data$positive_Dum[i] <- 1}
  else
  {thirty_minute_data$positive_Dum[i] <-0}
  
  if(thirty_minute_data$trend[i] == 0) 
  {thirty_minute_data$mixed_Dum[i] <- 1}
  else
  {thirty_minute_data$mixed_Dum[i] <-0}
  print(paste(i, "splitting", w))
}

thirty_minute_data$search_negative <- thirty_minute_data$negative_Dum*thirty_minute_data$SVI_log_diff
thirty_minute_data$search_positive <- thirty_minute_data$positive_Dum*thirty_minute_data$SVI_log_diff
thirty_minute_data$search_mixed <- thirty_minute_data$mixed_Dum*thirty_minute_data$SVI_log_diff

save(thirty_minute_data,file=paste(w,"_30minute_returns_bin.Rdata",sep = ""))
}