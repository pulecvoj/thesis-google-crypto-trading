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
load(file = "XBT_week.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_5 <- as.data.frame(matrix(1, nrow = ncol(week_data), ncol = 3))
for(i in 1:16){
  nr_nas_5[i,1] <- colnames(week_data)[i]
  nr_nas_5[i,2] <- nrow(week_data)
  nr_nas_5[i,3] <- sum(is.na(week_data[,i])) + sum(week_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_5) <- c("variable", "#observations", "#NAs")

week_data <- na.locf(week_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
week_data$return_price_all <-NA
week_data$logreturn_price_all <- NA
week_data$SVI_diff <- NA
week_data$SVI_log_diff <- NA

for (i in 2:nrow(week_data)){
  week_data$return_price_all[i] <- (week_data$price_all[i] -  week_data$price_all[i-1])/week_data$price_all[i-1]
  }

for (i in 2:nrow(week_data)){
  week_data$logreturn_price_all[i] <- log(week_data$price_all[i] / week_data$price_all[i-1])
}

for (i in 2:nrow(week_data)){
  week_data$SVI_diff[i] <- (week_data$SVI[i] -  week_data$SVI[i-1])/week_data$SVI[i-1]
}

for (i in 2:nrow(week_data)){
  week_data$SVI_log_diff[i] <- log(week_data$SVI[i] / week_data$SVI[i-1])
}

# subsetting
week_data <- week_data[-1,]

save(week_data,file="XBT_week_returns.Rdata")

#3) testing stationarity
#price
kpss.test(week_data$price_all, null = "Level", lshort = TRUE)
adf.test(week_data$price_all)



#testing - returns
kpss.test(week_data$return_price_all, null = "Level",lshort = TRUE)
adf.test(week_data$return_price_all)



#testing - log returns
kpss.test(week_data$logreturn_price_all, null = "Level",lshort = TRUE)
adf.test(week_data$logreturn_price_all)

#SVI
kpss.test(week_data$SVI, null = "Level", lshort = TRUE)
adf.test(week_data$SVI)

#SVI diff
kpss.test(week_data$SVI_diff, null = "Level", lshort = TRUE)
adf.test(week_data$SVI_diff)

#SVI log diff
kpss.test(week_data$SVI_log_diff, null = "Level", lshort = TRUE)
adf.test(week_data$SVI_log_diff)



#************ ACF & PACF ***************************************
acf <- acf(week_data$logreturn_price_all)
pacf <- pacf(week_data$logreturn_price_all)


#************ Rolling ACF & PACF ***************************************

#setting up parametr of the window
window_start <- 10 #any number from number of lags to nrow-1
window_end <- 54 #any number from start to nrow
lookback <- 9

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
  acf <- acf(week_data$logreturn_price_all[(i-lookback):i], lag.max = 7)
  time_acf[i,1] <- week_data$time[i]
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
  pacf <- pacf(week_data$logreturn_price_all[(i-lookback):i], lag.max = 7)
  time_pacf[i,1] <- week_data$time[i]
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
x.acf <- autocorrelations(week_data$logreturn_price_all)
whiteNoiseTest(x.acf, h0 = "iid", nlags = c(5,10,20))

# testing for corresponding window
x.acf <- autocorrelations(week_data$logreturn_price_all[c(window_start:window_end)])
whiteNoiseTest(x.acf, h0 = "iid", nlags = c(5,10,20))
