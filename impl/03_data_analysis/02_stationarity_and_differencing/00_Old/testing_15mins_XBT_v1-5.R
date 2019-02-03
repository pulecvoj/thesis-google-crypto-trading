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
library("urca")
library("lmtest")


# *** loading data ****************************
load(file = "XBT_15minute_returns.Rdata")

#1) testing stationarity
#price
kpss.test(fifteen_minute_data$price_all, null = "Level", lshort = TRUE)
adf.test(fifteen_minute_data$price_all)



#testing - returns
kpss.test(fifteen_minute_data$return_price_all, null = "Level",lshort = TRUE)
adf.test(fifteen_minute_data$return_price_all)

#testing - log returns
kpss.test(fifteen_minute_data$logreturn_price_all, null = "Level",lshort = TRUE)
adf.test(fifteen_minute_data$logreturn_price_all)

#SVI
kpss.test(fifteen_minute_data$SVI, null = "Level", lshort = TRUE)
adf.test(fifteen_minute_data$SVI)

#SVI diff
kpss.test(fifteen_minute_data$SVI_diff, null = "Level", lshort = TRUE)
adf.test(fifteen_minute_data$SVI_diff)

#SVI log diff
kpss.test(fifteen_minute_data$SVI_log_diff, null = "Level", lshort = TRUE)
adf.test(fifteen_minute_data$SVI_log_diff)


# 2) testing cointegration
sbst <- fifteen_minute_data[,c(2,3)]
summary(ca.jo(sbst))

# 3) testing granger causality
summary(grangertest(fifteen_minute_data$SVI,fifteen_minute_data$logreturn_price_all, order = 2)) #logreturn~SVI
summary(grangertest(fifteen_minute_data$logreturn_price_all, fifteen_minute_data$SVI,order = 2)) #SVI~logreturn

# 4) plain correlation
cor(sbst)

# 5) white noise testing
x.acf <- autocorrelations(fifteen_minute_data$logreturn_price_all)
whiteNoiseTest(x.acf, h0 = "iid", nlags = c(5,10,20))

x.acf <- autocorrelations(fifteen_minute_data$SVI_log_diff)
whiteNoiseTest(x.acf, h0 = "iid", nlags = c(5,10,20))

#************ ACF & PACF ***************************************
acf <- acf(fifteen_minute_data$logreturn_price_all)
pacf <- pacf(fifteen_minute_data$logreturn_price_all)


#*************************************** Rolling PCF **********************************************************
#setting up parametr of the window
window_start <- 73 #any number from number of lags to nrow-1
window_end <- nrow(fifteen_minute_data) #any number from start to nrow
lookback <- 72

# *** task bar
pb <- tkProgressBar(title = "progress bar", min = window_start, max = window_end, width = 300)

#loop itself
time_pacf <- data.frame(Time=double(),
                        Lag1=double(),
                        Lag2=double(),
                        Lag3=double(),
                        Lag4=double(),
                        Lag5=double(),
                        Lag6=double(),
                        Lag7=double())

for (i in window_start:window_end){
  pacf <- pacf(fifteen_minute_data$logreturn_price_all[(i-lookback):i], lag.max = 7)
  time_pacf[i,1] <- fifteen_minute_data$time[i]
  for(z in 2:8){
    time_pacf[i,z] <- pacf$acf[z-1]
  }
  setTkProgressBar(pb, i, label=paste( round((i - window_start)/(window_end-window_start), 2)*100,"% done"))
}

time_pacf <- time_pacf[-c(1:window_start),]

plot(time_pacf$Lag1)
plot(time_pacf$Lag2)

#************************************** Rolling ACF  **************************************************

time_acf <- data.frame(Time=double(),
                       Lag1=double(),
                       Lag2=double(),
                       Lag3=double(),
                       Lag4=double(),
                       Lag5=double(),
                       Lag6=double(),
                       Lag7=double())

# *** task bar
pb <- tkProgressBar(title = "progress bar", min = window_start, max = window_end, width = 300)

for (i in window_start:window_end){
  acf <- acf(fifteen_minute_data$logreturn_price_all[(i-lookback):i], lag.max = 7)
  time_acf[i,1] <- fifteen_minute_data$time[i]
  for(z in 2:8){
    time_acf[i,z] <- acf$acf[z-1]
  }
  setTkProgressBar(pb, i, label=paste( round((i - window_start)/(window_end-window_start), 2)*100,"% done"))
}

time_acf <- time_acf[-c(1:window_start),]


plot(time_acf$Lag2)
plot(time_acf$Lag3)


