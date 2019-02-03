setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd) 
rm(list=ls())

library(ggplot2)
library(tcltk)
library(forecast)
library(sparsevar)
library(vars)

# ***************** benchmarking to ARIMA **********************************************************************************

load(file=paste(window_start,"_", window_end,"_", lookback,"_time_vector_one_minut_ARIMA_fit_XBT.Rdata", sep = "")) #vector of non white noise observation from ARIMA
load(file=paste(window_start,"_", window_end,"_", lookback,"_time_vector_one_minut_ARIMA_fit_XBT.Rdata", sep = "")) #load the results 

forecasts <- fifteen_minute_data[which(fifteen_minute_data$time %in% time_vector),]

#actual returns share
returns_benchmark <- fifteen_minute_data[which(fifteen_minute_data$time %in% time_vector),]
returns_benchmark$one  <- nrow(returns_benchmark[which(returns_benchmark$R_1 > 1),])/(nrow(returns_benchmark)-sum(is.na(fifteen_minute_data$A_m1)))*100
returns_benchmark$three <- nrow(returns_benchmark[which(returns_benchmark$R_3 > 1),])/(nrow(returns_benchmark)-sum(is.na(fifteen_minute_data$A_m3)))*100
returns_benchmark$five <- nrow(returns_benchmark[which(returns_benchmark$R_5 > 1),])/(nrow(returns_benchmark)-sum(is.na(fifteen_minute_data$A_m5)))*100
returns_benchmark$ten <- nrow(returns_benchmark[which(returns_benchmark$R_10 > 1),])/(nrow(returns_benchmark)-sum(is.na(fifteen_minute_data$A_m10)))*100
returns_benchmark$fifteen  <- nrow(returns_benchmark[which(returns_benchmark$R_15 > 1),])/(nrow(returns_benchmark)-sum(is.na(fifteen_minute_data$A_m15)))*100
returns_benchmark <- returns_benchmark[1,-c(1:50)]

#MDA
returns_benchmark[2,1] <- sum(forecasts$A_DA_1,na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_1)))*100
returns_benchmark[2,2] <- sum(forecasts$A_DA_3, na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_3)))*100
returns_benchmark[2,3] <- sum(forecasts$A_DA_5,na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_5)))*100
returns_benchmark[2,4] <- sum(forecasts$A_DA_10,na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_10)))*100
returns_benchmark[2,5] <- sum(forecasts$A_DA_15, na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_15)))*100

#actual change in %
temp <- as.data.frame(matrix(1, ncol = 5, nrow = nrow(forecasts)))
for(i in 1:nrow(forecasts)){
  temp[i,1] <- (forecasts$R_1[i] - 1)*100
  temp[i,2] <- (forecasts$R_3[i] -1)*100
  temp[i,3] <- (forecasts$R_5[i] -1)*100
  temp[i,4] <- (forecasts$R_10[i] -1)*100
  temp[i,5] <- (forecasts$R_15[i] -1)*100
}

returns_benchmark[3,1] <- mean(temp[,1],na.rm = TRUE)
returns_benchmark[3,2] <- mean(temp[,2],na.rm = TRUE)
returns_benchmark[3,3] <- mean(temp[,3],na.rm = TRUE)
returns_benchmark[3,4] <- mean(temp[,4],na.rm = TRUE)
returns_benchmark[3,5] <- mean(temp[,5],na.rm = TRUE)

#Actual preditcion
temp <- as.data.frame(matrix(1, ncol = 5, nrow = nrow(forecasts)))
for(i in 1:nrow(forecasts)){
  temp[i,1] <- ((forecasts$A_m1[i]) - 1)*100
  temp[i,2] <- ((forecasts$A_m3[i]) -1)*100
  temp[i,3] <- ((forecasts$A_m5[i]) -1)*100
  temp[i,4] <- ((forecasts$A_m10[i]) -1)*100
  temp[i,5] <- ((forecasts$A_m15[i]) -1)*100
}

returns_benchmark[4,1] <- mean(temp[,1],na.rm = TRUE)
returns_benchmark[4,2] <- mean(temp[,2],na.rm = TRUE)
returns_benchmark[4,3] <- mean(temp[,3],na.rm = TRUE)
returns_benchmark[4,4] <- mean(temp[,4],na.rm = TRUE)
returns_benchmark[4,5] <- mean(temp[,5],na.rm = TRUE)

# MSE of the prediction
temp <- as.data.frame(matrix(1, ncol = 5, nrow = nrow(forecasts)))
for(i in 1:nrow(forecasts)){
  temp[i,1] <- ((forecasts$A_m1[i] - forecasts$R_1[i])^2)
  temp[i,2] <- ((forecasts$A_m3[i] - forecasts$R_3[i])^2)
  temp[i,3] <- ((forecasts$A_m5[i] - forecasts$R_5[i])^2)
  temp[i,4] <- ((forecasts$A_m10[i] - forecasts$R_10[i])^2)
  temp[i,5] <- ((forecasts$A_m15[i] - forecasts$R_15[i])^2)
}

returns_benchmark[5,1] <- mean(temp[,1],na.rm = TRUE)
returns_benchmark[5,2] <- mean(temp[,2],na.rm = TRUE)
returns_benchmark[5,3] <- mean(temp[,3],na.rm = TRUE)
returns_benchmark[5,4] <- mean(temp[,4],na.rm = TRUE)
returns_benchmark[5,5] <- mean(temp[,5],na.rm = TRUE)


save(returns_benchmark,file=paste(window_start,"_", window_end,"_", lookback,"_15minut_VAR_no_diff_benchmark_XBT.Rdata", sep = ""))


# ************** analysis of the results (VAR solely) ********************************

forecasts <- fifteen_minute_data[which(fifteen_minute_data$A_m1 !=1),]

#actual returns share
returns_benchmark <- fifteen_minute_data[which(fifteen_minute_data$A_m1 !=1),]
returns_benchmark$one  <- nrow(returns_benchmark[which(returns_benchmark$R_1 > 1),])/(nrow(returns_benchmark)-sum(is.na(fifteen_minute_data$A_m1)))*100
returns_benchmark$three <- nrow(returns_benchmark[which(returns_benchmark$R_3 > 1),])/(nrow(returns_benchmark)-sum(is.na(fifteen_minute_data$A_m3)))*100
returns_benchmark$five <- nrow(returns_benchmark[which(returns_benchmark$R_5 > 1),])/(nrow(returns_benchmark)-sum(is.na(fifteen_minute_data$A_m5)))*100
returns_benchmark$ten <- nrow(returns_benchmark[which(returns_benchmark$R_10 > 1),])/(nrow(returns_benchmark)-sum(is.na(fifteen_minute_data$A_m10)))*100
returns_benchmark$fifteen  <- nrow(returns_benchmark[which(returns_benchmark$R_15 > 1),])/(nrow(returns_benchmark)-sum(is.na(fifteen_minute_data$A_m15)))*100
returns_benchmark <- returns_benchmark[1,-c(1:50)]

#MDA
returns_benchmark[2,1] <- sum(forecasts$A_DA_1,na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_1)))*100
returns_benchmark[2,2] <- sum(forecasts$A_DA_3, na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_3)))*100
returns_benchmark[2,3] <- sum(forecasts$A_DA_5,na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_5)))*100
returns_benchmark[2,4] <- sum(forecasts$A_DA_10,na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_10)))*100
returns_benchmark[2,5] <- sum(forecasts$A_DA_15, na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_15)))*100

#actual change in %
temp <- as.data.frame(matrix(1, ncol = 5, nrow = nrow(forecasts)))
for(i in 1:nrow(forecasts)){
  temp[i,1] <- (forecasts$R_1[i] - 1)*100
  temp[i,2] <- (forecasts$R_3[i] -1)*100
  temp[i,3] <- (forecasts$R_5[i] -1)*100
  temp[i,4] <- (forecasts$R_10[i] -1)*100
  temp[i,5] <- (forecasts$R_15[i] -1)*100
}

returns_benchmark[3,1] <- mean(temp[,1],na.rm = TRUE)
returns_benchmark[3,2] <- mean(temp[,2],na.rm = TRUE)
returns_benchmark[3,3] <- mean(temp[,3],na.rm = TRUE)
returns_benchmark[3,4] <- mean(temp[,4],na.rm = TRUE)
returns_benchmark[3,5] <- mean(temp[,5],na.rm = TRUE)

#Actual preditcion
temp <- as.data.frame(matrix(1, ncol = 5, nrow = nrow(forecasts)))
for(i in 1:nrow(forecasts)){
  temp[i,1] <- ((forecasts$A_m1[i]) - 1)*100
  temp[i,2] <- ((forecasts$A_m3[i]) -1)*100
  temp[i,3] <- ((forecasts$A_m5[i]) -1)*100
  temp[i,4] <- ((forecasts$A_m10[i]) -1)*100
  temp[i,5] <- ((forecasts$A_m15[i]) -1)*100
}

returns_benchmark[4,1] <- mean(temp[,1],na.rm = TRUE)
returns_benchmark[4,2] <- mean(temp[,2],na.rm = TRUE)
returns_benchmark[4,3] <- mean(temp[,3],na.rm = TRUE)
returns_benchmark[4,4] <- mean(temp[,4],na.rm = TRUE)
returns_benchmark[4,5] <- mean(temp[,5],na.rm = TRUE)

# MSE of the prediction
temp <- as.data.frame(matrix(1, ncol = 5, nrow = nrow(forecasts)))
for(i in 1:nrow(forecasts)){
  temp[i,1] <- ((forecasts$A_m1[i] - forecasts$R_1[i])^2)
  temp[i,2] <- ((forecasts$A_m3[i] - forecasts$R_3[i])^2)
  temp[i,3] <- ((forecasts$A_m5[i] - forecasts$R_5[i])^2)
  temp[i,4] <- ((forecasts$A_m10[i] - forecasts$R_10[i])^2)
  temp[i,5] <- ((forecasts$A_m15[i] - forecasts$R_15[i])^2)
}

returns_benchmark[5,1] <- mean(temp[,1],na.rm = TRUE)
returns_benchmark[5,2] <- mean(temp[,2],na.rm = TRUE)
returns_benchmark[5,3] <- mean(temp[,3],na.rm = TRUE)
returns_benchmark[5,4] <- mean(temp[,4],na.rm = TRUE)
returns_benchmark[5,5] <- mean(temp[,5],na.rm = TRUE)


save(returns_benchmark,file=paste(window_start,"_", window_end,"_", lookback,"_minut_VAR_no_diff_solely_benchmark_XBT.Rdata", sep = ""))

