#Prepsat to na skalovatelnost trendu

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken", sep = "")
setwd(wd) 
rm(list=ls())

#install.packages("sparsevar")


library(ggplot2)
library(tcltk)
library(forecast)
library(sparsevar)
library(vars)

# *** loading data ****************************
load(file="XBT_5minute_returns.Rdata")

# *** adding hourly / daily trend ***********************************************
#generating the data structure
hourly_trend <- as.data.frame(matrix(1, nrow = nrow(five_minute_data), ncol = 2))
colnames(hourly_trend) <- c("time", "hourly_ret")

daily_trend <- as.data.frame(matrix(1, nrow = nrow(five_minute_data), ncol = 2))
colnames(daily_trend) <- c("time", "daily_ret")

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
  if(hourly_trend[i,3] < 0)
  {five_minute_data$search_negative_hourly[i] <- five_minute_data$SVI[i]}
  else
  {five_minute_data$search_negative_hourly[i] <- 0}
  
  if(hourly_trend[i,3] > 0) 
  {five_minute_data$search_positive_hourly[i] <- five_minute_data$SVI[i]}
  else
  {five_minute_data$search_positive_hourly[i] <-0}
}
for (i in 289:nrow(five_minute_data)) {
  if(daily_trend[i,3] < 0)
  {five_minute_data$search_negative_daily[i] <- five_minute_data$SVI[i]}
  else
  {five_minute_data$search_negative_daily[i] <- 0}
  
  if(daily_trend[i,3] > 0) 
  {five_minute_data$search_positive_daily[i] <- five_minute_data$SVI[i]}
  else
  {five_minute_data$search_positive_daily[i] <-0}
}

#setting up parametr of the window
window_start <- 50000 #any number from number of lags to nrow-1
window_end <- 55036 #any number from start to nrow
lookback <- 72

sbst <- as.matrix(na.omit(five_minute_data[c((window_start) :window_end), c(18,21)]))

fit <- VAR(sbst, p = 1, type = "none")
irf.var.neg <- irf(fit, impulse = "search_negative_hourly", response = "logreturn_price_all",  n.ahead = 25, ci = 0.9)


sbst <- as.matrix(na.omit(five_minute_data[c((window_start) :window_end), c(22,18)]))

fit <- VAR(sbst, p = 1, type = "none")
irf.var.pos <- irf(fit, response = "search_positive_hourly", impulse = "logreturn_price_all",  n.ahead = 25, ci = 0.9)

sbst <- as.matrix(na.omit(five_minute_data[c((window_start) :window_end), c(2,10)]))

fit <- VAR(sbst, p = 1, type = "none")
irf.var.neu <- irf(fit, impulse = "SVI", response = "volume_all",  n.ahead = 25, ci = 0.9)

par(mfrow=c(2,1))
plot(irf.var.pos)
plot(irf.var.neg)
plot(irf.var.neu)

# **** fitting s var ******************************

fit <- fitVAR(sbst, p = 20, penalty = "SCAD", alpha = 1)

#sparsevar::plotVAR(fit)
fit$A


#computeForecasts(fit, 10)

irf <- impulseResponse(fit,20)
eb <- errorBandsIRF(fit, irf)


plotIRFGrid(irf, eb, c(1,2,3))