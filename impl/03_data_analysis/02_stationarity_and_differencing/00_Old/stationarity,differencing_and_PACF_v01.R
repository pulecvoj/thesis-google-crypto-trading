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


# *** loading data ****************************
load(file = "five_minute_data_bc.RData")

#1) filling NAs - taking last valit price - zoo na.locf
nr_nas_5 <- as.data.frame(matrix(1, nrow = ncol(five_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_5[i,1] <- colnames(five_minute_data)[i]
  nr_nas_5[i,2] <- nrow(five_minute_data)
  nr_nas_5[i,3] <- sum(is.na(five_minute_data[,i])) + sum(five_minute_data[,i] == 0, na.rm = TRUE)
}

five_minute_data <- na.locf(five_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
for (i in 2:nrow(five_minute_data)){
  five_minute_data$return_price_all[i] <- (five_minute_data$price_all[i] -  five_minute_data$price_all[i-1])/five_minute_data$price_all[i-1]
  }

for (i in 2:nrow(five_minute_data)){
  five_minute_data$logreturn_price_all[i] <- log(five_minute_data$price_all[i] / five_minute_data$price_all[i-1])
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$return_squared_price_all[i] <- (five_minute_data$return_price_all[i])^2
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$logreturn_squared_price_all[i] <- (five_minute_data$logreturn_price_all[i])^2
}

# subsetting
five_minute_data <- five_minute_data[-1,]

save(five_minute_data,file="five_minute_data_ret_bc.Rdata")

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

#************ ACF ***************************************
time_acf <- data.frame(Time=double(),
                       Lag1=double(),
                       Lag2=double(),
                       Lag3=double(),
                       Lag4=double(),
                       Lag5=double(),
                       Lag6=double(),
                       Lag7=double())

# *** task bar
pb <- tkProgressBar(title = "progress bar", min = 0, max = nrow(five_minute_data), width = 300)

for (i in 1:(nrow(five_minute_data)-288)){
  acf <- acf(five_minute_data$logreturn_price_all[i:(i+288)], lag.max = 5)
  time_acf[i,1] <- five_minute_data$time_min[i+288]
  for(z in 2:6){
    time_acf[i,z] <- acf$acf[z-1]
  }
  setTkProgressBar(pb, i, label=paste( round(i/nrow(five_minute_data)*100 , 0),"% done"))
}

save(time_acf,file="acf_1day_5mi_bc.Rdata")
plot(time_acf$Lag2)
plot(time_acf$Lag3)
#load(file = "acf.Rdata")

#************ PCF ***************************************

time_pacf <- data.frame(Time=double(),
                       Lag1=double(),
                       Lag2=double(),
                       Lag3=double(),
                       Lag4=double(),
                       Lag5=double(),
                       Lag6=double(),
                       Lag7=double())

pb <- tkProgressBar(title = "progress bar", min = 0, max = nrow(five_minute_data), width = 300)

for (i in 1:(nrow(five_minute_data)-36)){
  pcf <- pacf(five_minute_data$logreturn_price_all[i:(i+36)], lag.max = 5)
  time_pacf[i,1] <- five_minute_data$time_min[i+36]
  for(z in 2:6){
    time_pacf[i,z] <- pcf$acf[z-1]
  }
  setTkProgressBar(pb, i, label=paste( round(i/nrow(five_minute_data)*100 , 0),"% done"))
}


save(time_pacf,file="pacf_1day_5mi_bc.Rdata")
#load(file = "pacf.Rdata")

plot(time_pacf$Lag1)
plot(time_pacf$Lag2)
