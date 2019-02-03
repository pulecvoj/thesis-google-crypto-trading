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
load(file = "XBT_5minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_5 <- as.data.frame(matrix(1, nrow = ncol(five_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_5[i,1] <- colnames(five_minute_data)[i]
  nr_nas_5[i,2] <- nrow(five_minute_data)
  nr_nas_5[i,3] <- sum(is.na(five_minute_data[,i])) + sum(five_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_5) <- c("variable", "#observations", "#NAs")

five_minute_data <- na.locf(five_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
five_minute_data$return_price_all <-NA
five_minute_data$logreturn_price_all <- NA

for (i in 2:nrow(five_minute_data)){
  five_minute_data$return_price_all[i] <- (five_minute_data$price_all[i] -  five_minute_data$price_all[i-1])/five_minute_data$price_all[i-1]
  }

for (i in 2:nrow(five_minute_data)){
  five_minute_data$logreturn_price_all[i] <- log(five_minute_data$price_all[i] / five_minute_data$price_all[i-1])
}

# subsetting
five_minute_data <- five_minute_data[-1,]

for (i in 1:nrow(five_minute_data)){
  five_minute_data$return_squared_price_all[i] <- (five_minute_data$return_price_all[i])^2
}

for (i in 1:nrow(five_minute_data)){
  five_minute_data$logreturn_squared_price_all[i] <- (five_minute_data$logreturn_price_all[i])^2
}



save(five_minute_data,file="XBT_5minute_returns.Rdata")

#3) testing stationarity
#price
kpss.test(five_minute_data$price_all, null = "Level", lshort = TRUE)
adf.test(five_minute_data$price_all)



#testing - returns
kpss.test(five_minute_data$return_price_all, null = "Level",lshort = TRUE)
adf.test(five_minute_data$return_price_all)



#testing - log returns
kpss.test(five_minute_data$logreturn_price_all, null = "Level",lshort = TRUE)
adf.test(five_minute_data$logreturn_price_all)

#SVI
kpss.test(five_minute_data$SVI, null = "Level", lshort = TRUE)
adf.test(five_minute_data$SVI)

#SVI diff
kpss.test(five_minute_data$SVI_diff, null = "Level", lshort = TRUE)
adf.test(five_minute_data$SVI_diff)

#SVI log diff
kpss.test(five_minute_data$SVI_log_diff, null = "Level", lshort = TRUE)
adf.test(five_minute_data$SVI_log_diff)


#************ ACF & PACF ***************************************
acf <- acf(five_minute_data$logreturn_price_all)
pacf <- pacf(five_minute_data$logreturn_price_all)


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
  acf <- acf(five_minute_data$logreturn_price_all[(i-lookback):i], lag.max = 7)
  time_acf[i,1] <- five_minute_data$time[i]
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
  pacf <- pacf(five_minute_data$logreturn_price_all[(i-lookback):i], lag.max = 7)
  time_pacf[i,1] <- five_minute_data$time[i]
  for(z in 2:8){
    time_pacf[i,z] <- pacf$acf[z-1]
  }
  setTkProgressBar(pb, i, label=paste( round((i - window_start)/(window_end-window_start), 2)*100,"% done"))
}

time_pacf <- time_pacf[-c(1:window_start),]

plot(time_pacf$Lag1)
plot(time_pacf$Lag2)

load(file="XBT_5minute_returns.Rdata")

# general white noise testing
x.acf <- autocorrelations(five_minute_data$logreturn_price_all)
whiteNoiseTest(x.acf, h0 = "iid", nlags = c(5,10,20))

# testing for corresponding window
x.acf <- autocorrelations(five_minute_data$logreturn_price_all[c(window_start:window_end)])
whiteNoiseTest(x.acf, h0 = "iid", nlags = c(5,10,20))
