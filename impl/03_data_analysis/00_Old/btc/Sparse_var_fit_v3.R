#Prepsat to na skalovatelnost trendu

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/test_sample/bitcoin", sep = "")
setwd(wd) 
rm(list=ls())

#install.packages("sparsevar")


library(ggplot2)
library(tcltk)
library(forecast)
library(sparsevar)

# *** loading data ****************************
load(file = "five_minute_data_ret_bc.RData")

# *** adding hourly / daily trend ***********************************************
#generating the data structure
hourly_trend <- as.data.frame(matrix(1, nrow = nrow(five_minute_data), ncol = 2))
colnames(hourly_trend) <- c("time_min", "hourly_ret")

daily_trend <- as.data.frame(matrix(1, nrow = nrow(five_minute_data), ncol = 2))
colnames(daily_trend) <- c("time_min", "daily_ret")

#getting hourly trend
for(i in 13:nrow(five_minute_data)){
  hourly_trend$time_min[i] <- five_minute_data$time_min[i]
  hourly_trend$hourly_ret[i] <- log(five_minute_data$price_all[i]/five_minute_data$price_all[i-12])
}

for(i in 1:nrow(five_minute_data)){
  hourly_trend$hourly_trend[i] <- sign(hourly_trend$hourly_ret[i])
}

# getting daily trend
for(i in 289:nrow(five_minute_data)){
  daily_trend$time_min[i] <- five_minute_data$time_min[i]
  daily_trend$daily_ret[i] <- log(five_minute_data$price_all[i]/five_minute_data$price_all[i-288])
}

for(i in 1:nrow(five_minute_data)){
  daily_trend$daily_trend[i] <- sign(daily_trend$daily_ret[i])
}

#looping to assign trend to time stamp
for (i in 1:nrow(five_minute_data)) {
  five_minute_data$daily_trend[i] <- daily_trend$daily_trend[match(five_minute_data$time_min[i], daily_trend$time_min)]
  five_minute_data$hourly_trend[i] <- hourly_trend$hourly_trend[match(five_minute_data$time_min[i], hourly_trend$time_min)]
}

#structure for splitting
five_minute_data$search_negative_hourly <- NA
five_minute_data$search_positive_hourly <- NA
five_minute_data$search_negative_daily <- NA
five_minute_data$search_positive_daily <- NA

#splitting search into positive and negative
for (i in 13:nrow(five_minute_data)) {
  if(five_minute_data$hourly_trend[i] < 0)
  {five_minute_data$search_negative_hourly[i] <- five_minute_data$search[i]}
  else
  {five_minute_data$search_negative_hourly[i] <- 0}
  
  if(five_minute_data$hourly_trend[i] > 0) 
  {five_minute_data$search_positive_hourly[i] <- five_minute_data$search[i]}
  else
  {five_minute_data$search_positive_hourly[i] <-0}
}
for (i in 289:nrow(five_minute_data)) {
  if(five_minute_data$daily_trend[i] < 0) 
    {five_minute_data$search_negative_daily[i] <- five_minute_data$search[i]}
  if(five_minute_data$daily_trend[i] > 0) {five_minute_data$search_positive_daily[i] <- five_minute_data$search[i]}

}


sbst <- as.matrix(five_minute_data[c(13:5999), c(18,20,9,16)])

# **** fitting s var ******************************

fit <- fitVAR(sbst, p = 25, penalty = "ENET", alpha = 0)

#sparsevar::plotVAR(fit)
fit$A


#computeForecasts(fit, 10)

irf <- impulseResponse(fit,25)
eb <- errorBandsIRF(fit, irf)


plotIRFGrid(irf, eb, c(3,4))