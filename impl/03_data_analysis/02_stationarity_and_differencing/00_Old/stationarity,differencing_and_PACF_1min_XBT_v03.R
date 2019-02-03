setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken", sep = "")
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
load(file = "XBT_1minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(minute_data)[i]
  nr_nas_1[i,2] <- nrow(minute_data)
  nr_nas_1[i,3] <- sum(is.na(minute_data[,i])) + sum(minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(minute_data) <- c("time", "SVI", "price_all",
                                "price_ba","price_bm", "price_bl",
                                "price_sa","price_sm", "price_sl",
                                "volume_all",
                                "volume_ba","volume_bm","volume_bl",
                                "volume_sa","volume_sm","volume_sl")

minute_data <- na.locf(minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
minute_data$return_price_all <-NA
minute_data$logreturn_price_all <- NA
minute_data$SVI_diff <- NA
minute_data$SVI_log_diff <- NA

for (i in 2:nrow(minute_data)){
  minute_data$return_price_all[i] <- (minute_data$price_all[i] -  minute_data$price_all[i-1])/minute_data$price_all[i-1]
  }

for (i in 2:nrow(minute_data)){
  minute_data$logreturn_price_all[i] <- log(minute_data$price_all[i] / minute_data$price_all[i-1])
}

for (i in 2:nrow(minute_data)){
  minute_data$SVI_diff[i] <- (minute_data$SVI[i] -  minute_data$SVI[i-1])/minute_data$SVI[i-1]
}

for (i in 2:nrow(minute_data)){
  minute_data$SVI_log_diff[i] <- log(minute_data$SVI[i] / minute_data$SVI[i-1])
}


# subsetting
minute_data <- minute_data[-1,]


save(minute_data,file="XBT_1minute_returns.Rdata")

#3) testing stationarity
#price
kpss.test(minute_data$price_all, null = "Level", lshort = TRUE)
adf.test(minute_data$price_all)



#testing - returns
kpss.test(minute_data$return_price_all, null = "Level",lshort = TRUE)
adf.test(minute_data$return_price_all)



#testing - log returns
kpss.test(minute_data$logreturn_price_all, null = "Level",lshort = TRUE)
adf.test(minute_data$logreturn_price_all)

#SVI
kpss.test(minute_data$SVI, null = "Level", lshort = TRUE)
adf.test(minute_data$SVI)

#SVI diff
kpss.test(minute_data$SVI_diff, null = "Level", lshort = TRUE)
adf.test(minute_data$SVI_diff)

#SVI log diff
kpss.test(minute_data$SVI_log_diff, null = "Level", lshort = TRUE)
adf.test(minute_data$SVI_log_diff)


#************ ACF & PACF ***************************************
acf <- acf(minute_data$logreturn_price_all)
pacf <- pacf(minute_data$logreturn_price_all)


#************ Rolling ACF & PACF ***************************************

#setting up parametr of the window
window_start <- 31200 #any number from number of lags to nrow-1
window_end <- 33200 #any number from start to nrow
lookback <- 72

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
  acf <- acf(minute_data$logreturn_price_all[(i-lookback):i], lag.max = 7)
  time_acf[i,1] <- minute_data$time[i]
  for(z in 2:8){
    time_acf[i,z] <- acf$acf[z-1]
  }
  setTkProgressBar(pb, i, label=paste( round((i - window_start)/(window_end-window_start), 2)*100,"% done"))
}

time_acf <- time_acf[-c(1:window_start),]


plot(time_acf$Lag2)
plot(time_acf$Lag3)

#****** PCF *******

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
  pacf <- pacf(minute_data$logreturn_price_all[(i-lookback):i], lag.max = 7)
  time_pacf[i,1] <- minute_data$time[i]
  for(z in 2:8){
    time_pacf[i,z] <- pacf$acf[z-1]
  }
  setTkProgressBar(pb, i, label=paste( round((i - window_start)/(window_end-window_start), 2)*100,"% done"))
}

time_pacf <- time_pacf[-c(1:window_start),]

plot(time_pacf$Lag1)
plot(time_pacf$Lag2)

load(file="XBT_1minute_returns.Rdata")

# general white noise testing
x.acf <- autocorrelations(minute_data$logreturn_price_all)
whiteNoiseTest(x.acf, h0 = "iid", nlags = c(5,10,20))

x.acf <- autocorrelations(minute_data$SVI_log_diff)
whiteNoiseTest(x.acf, h0 = "iid", nlags = c(5,10,20))

# testing for corresponding window
x.acf <- autocorrelations(minute_data$logreturn_price_all[c(window_start:window_end)])
whiteNoiseTest(x.acf, h0 = "iid", nlags = c(5,10,20))
