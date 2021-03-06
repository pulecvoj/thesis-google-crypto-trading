setwd("C:/Users/Vojt�ch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd) 
rm(list=ls())

library(ggplot2)
library(tcltk)
library(forecast)
library(sparsevar)
library(vars)

#loop for multiple rolling window lenghts
for(q in c(60,120,240,360)){
  print(q)

# *** loading data ****************************
  load(file="XMR_1minute_returns.Rdata")
  
 # *** adding XY trend ***********************************************
  #generating the data structure
  trend <- as.data.frame(matrix(1, nrow = nrow(minute_data), ncol = 2))
  colnames(trend) <- c("time", "ret")
  
  #getting trend (hourly)
  for(i in 60:nrow(minute_data)){
    trend$time[i] <- minute_data$time[i]
    trend$ret[i] <- log(minute_data$price_all[i]/minute_data$price_all[i-60])
  }
  
  for(i in 1:nrow(five_minute_data)){
    trend$trend[i] <- sign(trend$ret[i])
  }
  
#looping to assign trend to time stamp
  for (i in 1:nrow(minute_data)) {
    minute_data$trend[i] <- trend$trend[match(minute_data$time[i], trend$time)]
  }
  
#structure for splitting
  minute_data$search_negative <- NA
  minute_data$search_positive <- NA
  minute_data$search_mixed <- NA
  
#splitting search into positive and negative
  for (i in 60:nrow(minute_data)) {
    if(minute_data$trend[i] < 0)
    {minute_data$search_negative[i] <- minute_data$SVI[i]}
    else
    {minute_data$search_negative[i] <- 0}
    
    if(minute_data$trend[i]> 0) 
    {minute_data$search_positive[i] <- minute_data$SVI[i]}
    else
    {minute_data$search_positive[i] <-0}
    
    if(minute_data$trend[i] = 0) 
    {minute_data$search_mixed[i] <- minute_data$SVI[i]}
    else
    {minute_data$search_mixed[i] <-0}
  }

  # 2) *********** preparing structure for fit ******************************
  
  #setting up parametr of the window
  
  
  window_start <- q+1 #any number from number of lags to nrow-1
  window_end <- nrow(minute_data) #any number from start to nrow
  lookback <- q
  
  minute_data <- minute_data[c((window_start-lookback):window_end),]
  

  # Progress bar
  # before loop pb <- tkProgressBar(title = "progress bar", min = 0, max = nrow(OM_uni), width = 1000)
  # into the loop setTkProgressBar(pb, i, label=paste( round(i/(nrow(OM_uni))*1000, 0),"� done"))
  
  #preparing struncture - 23
  minute_data[,23:54] <- 1
  colnames(minute_data)[c(23,26,29,32,35)] <- c("A_m1","A_m3","A_m5","A_m10", "A_m15")                             #mean for x minutes forward estimate
  colnames(minute_data)[c(24,27,30,33,36)] <- c("A_u1(95%)", "A_u3(95%)", "A_u5(95%)", "A_u10(95%)","A_u15(95%)")  #upper bound for x minutes forward estimate
  colnames(minute_data)[c(25,228,31,34,37)] <- c("A_l1(95%)" ,"A_l3(95%)", "A_l5(95%)", "A_l10(95%)", "A_l15(95%)")  # lower bound for x minutes forward estimate
  colnames(minute_data)[c(38,39)] <- c("A_BIC", "A_AICC")                                                            # AIC a BIC criteria for choosing proper order
  colnames(minute_data)[c(40:41)] <- c("R_1","R_3","R_5","R_10", "R_15")                                          #real values
  colnames(minute_data)[c(44:49)] <- c("A_SQe_1","A_SQe_3","A_SQe_5","A_SQe_10", "A_SQe_15")                      #squared errors of estimate 
  colnames(minute_data)[c(50:54)] <- c("A_DA_1","A_DA_3","A_DA_5","A_DA_10", "A_DA_15")                           #directional accuracy


minute_data <- minute_data[c((window_start-lookback):window_end),]


# **** fitting var ******************************
timeframe <- lookback #reference period
shift <- timeframe - 1 # last block of time frame it starts forecasting 5/10/15/30/60 mins

pb <- tkProgressBar(title = "progress bar", min = 0, max = (nrow(minute_data)- timeframe), width = 1000)

for(i in 1:(nrow(minute_data)-timeframe)){
  sbst <- as.matrix(minute_data[c(i:(i+shift)), c(18,20,21,22)])
  
  fit <- VAR(sbst, lag.max = 4, ic = "AIC", type = "none")
  forecast60 <- predict(fit, n.ahead = 15, ic = "AIC", ci = 0.95)
  forecast60 <- forecast60$fcst$logreturn_price_all 
  
  # fitting 1 minutes ahead
  minute_data[i+shift,23] <- forecast60[1,1] + 1
  minute_data[i+shift,24] <- forecast60[1,3] + 1
  minute_data[i+shift,25] <- forecast60[1,2] + 1
  
  
  # fitting 3 minute ahead
  for(z in 1:3){
    minute_data[i+shift,26] <- (forecast60[z,1] + 1) *  minute_data[i+shift,26]
    minute_data[i+shift,27] <- (forecast60[z,3] + 1) * minute_data[i+shift,27]
    minute_data[i+shift,28] <- (forecast60[z,2] + 1) * minute_data[i+shift,28]
  }
  
  # fitting 5 minute ahead
  for(z in 1:5){
    minute_data[i+shift,29] <- (forecast60[z,1] + 1) *  minute_data[i+shift,29]
    minute_data[i+shift,30] <- (forecast60[z,3] + 1) * minute_data[i+shift,30]
    minute_data[i+shift,31] <- (forecast60[z,2] + 1) * minute_data[i+shift,31]
  }
  
  # fitting 10 minute ahead
  for(z in 1:10){
    minute_data[i+shift,32] <- (forecast60[z,1] + 1) *  minute_data[i+shift,32]
    minute_data[i+shift,33] <- (forecast60[z,3] + 1) * minute_data[i+shift,33]
    minute_data[i+shift,34] <- (forecast60[z,2] + 1) * minute_data[i+shift,34]
  }
  
  # fitting 15 minute ahead
  for(z in 1:15){
    minute_data[i+shift,35] <- (forecast60[z,1] + 1) *  minute_data[i+shift,35]
    minute_data[i+shift,36] <- (forecast60[z,3] + 1) * minute_data[i+shift,36]
    minute_data[i+shift,37] <- (forecast60[z,2] + 1) * minute_data[i+shift,37]
  }
  
  #getting AIC
  minute_data[i+shift,38] <- forecast60$model$bic
  minute_data[i+shift,39] <- forecast60$model$aicc
  
  # gettting real returns
  minute_data[i+shift,40] <- log(minute_data[i+shift+1,3]/minute_data[i+shift,3]) + 1
  minute_data[i+shift,41] <- log(minute_data[i+shift+2,3]/minute_data[i+shift,3]) + 1
  minute_data[i+shift,42] <- log(minute_data[i+shift+3,3]/minute_data[i+shift,3]) + 1
  minute_data[i+shift,43] <- log(minute_data[i+shift+6,3]/minute_data[i+shift,3]) + 1
  minute_data[i+shift,44] <- log(minute_data[i+shift+12,3]/minute_data[i+shift,3]) + 1
  
  # gettting squred error
  minute_data[i+shift,45] <- (minute_data[i+shift,40] - minute_data[i+shift,13])^2
  minute_data[i+shift,46] <- (minute_data[i+shift,41] - minute_data[i+shift,26])^2
  minute_data[i+shift,47] <- (minute_data[i+shift,42] - minute_data[i+shift,29])^2
  minute_data[i+shift,48] <- (minute_data[i+shift,43] - minute_data[i+shift,32])^2
  minute_data[i+shift,49] <- (minute_data[i+shift,44] - minute_data[i+shift,35])^2
  
  # gettting direction accuracy
  minute_data[i+shift,50] <- (sign(minute_data[i+shift,40]-1) == sign(minute_data[i+shift,23]-1))*1
  minute_data[i+shift,51] <- (sign(minute_data[i+shift,41]-1) == sign(minute_data[i+shift,26]-1))*1
  minute_data[i+shift,52] <- (sign(minute_data[i+shift,42]-1) == sign(minute_data[i+shift,29]-1))*1
  minute_data[i+shift,53] <- (sign(minute_data[i+shift,43]-1) == sign(minute_data[i+shift,32]-1))*1
  minute_data[i+shift,54] <- (sign(minute_data[i+shift,44]-1) == sign(minute_data[i+shift,35]-1))*1
  
  setTkProgressBar(pb, i, label=paste( round(i/(nrow(minute_data)-timeframe)*1000, 0),"� done"))
  
  print(paste(q, "_",i))
  
  }

#five_minute_data[,40] <- NA
#five_minute_data[,41] <- NA


save(minute_data,file=paste(window_start,"_", window_end,"_", lookback,"_minut_VAR_hourly_split_fit_XBT.Rdata", sep = ""))

#********************************************** end of fitting ***************************************************************

# ************** analysis of the results (VAR solely) ********************************

forecasts <- five_minute_data

#actual returns share
returns_benchmark <- five_minute_data
returns_benchmark$five  <- nrow(returns_benchmark[which(returns_benchmark$R_5 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m5)))*100
returns_benchmark$ten <- nrow(returns_benchmark[which(returns_benchmark$R_10 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m10)))*100
returns_benchmark$fifteen <- nrow(returns_benchmark[which(returns_benchmark$R_15 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m15)))*100
returns_benchmark$thirty <- nrow(returns_benchmark[which(returns_benchmark$R_30 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m30)))*100
returns_benchmark$sixty  <- nrow(returns_benchmark[which(returns_benchmark$R_60 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m60)))*100
returns_benchmark <- returns_benchmark[1,-c(1:56)]

#MDA
returns_benchmark[2,1] <- sum(forecasts$A_DA_5,na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_5)))*100
returns_benchmark[2,2] <- sum(forecasts$A_DA_10, na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_10)))*100
returns_benchmark[2,3] <- sum(forecasts$A_DA_15,na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_15)))*100
returns_benchmark[2,4] <- sum(forecasts$A_DA_30,na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_30)))*100
returns_benchmark[2,5] <- sum(forecasts$A_DA_60, na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_60)))*100

#actual change in %
temp <- as.data.frame(matrix(1, ncol = 5, nrow = nrow(forecasts)))
for(i in 1:nrow(forecasts)){
  temp[i,1] <- (forecasts$R_5[i] - 1)*100
  temp[i,2] <- (forecasts$R_10[i] -1)*100
  temp[i,3] <- (forecasts$R_15[i] -1)*100
  temp[i,4] <- (forecasts$R_30[i] -1)*100
  temp[i,5] <- (forecasts$R_60[i] -1)*100
}

returns_benchmark[3,1] <- mean(temp[,1],na.rm = TRUE)
returns_benchmark[3,2] <- mean(temp[,2],na.rm = TRUE)
returns_benchmark[3,3] <- mean(temp[,3],na.rm = TRUE)
returns_benchmark[3,4] <- mean(temp[,4],na.rm = TRUE)
returns_benchmark[3,5] <- mean(temp[,5],na.rm = TRUE)

#Actual preditcion
temp <- as.data.frame(matrix(1, ncol = 5, nrow = nrow(forecasts)))
for(i in 1:nrow(forecasts)){
  temp[i,1] <- ((forecasts$A_m5[i]) - 1)*100
  temp[i,2] <- ((forecasts$A_m10[i]) -1)*100
  temp[i,3] <- ((forecasts$A_m15[i]) -1)*100
  temp[i,4] <- ((forecasts$A_m30[i]) -1)*100
  temp[i,5] <- ((forecasts$A_m60[i]) -1)*100
}

returns_benchmark[4,1] <- mean(temp[,1],na.rm = TRUE)
returns_benchmark[4,2] <- mean(temp[,2],na.rm = TRUE)
returns_benchmark[4,3] <- mean(temp[,3],na.rm = TRUE)
returns_benchmark[4,4] <- mean(temp[,4],na.rm = TRUE)
returns_benchmark[4,5] <- mean(temp[,5],na.rm = TRUE)

# MSE of the prediction
temp <- as.data.frame(matrix(1, ncol = 5, nrow = nrow(forecasts)))
for(i in 1:nrow(forecasts)){
  temp[i,1] <- ((forecasts$A_m5[i] - forecasts$R_5[i])^2)
  temp[i,2] <- ((forecasts$A_m10[i] - forecasts$R_10[i])^2)
  temp[i,3] <- ((forecasts$A_m15[i] - forecasts$R_15[i])^2)
  temp[i,4] <- ((forecasts$A_m30[i] - forecasts$R_30[i])^2)
  temp[i,5] <- ((forecasts$A_m60[i] - forecasts$R_60[i])^2)
}

returns_benchmark[5,1] <- mean(temp[,1],na.rm = TRUE)
returns_benchmark[5,2] <- mean(temp[,2],na.rm = TRUE)
returns_benchmark[5,3] <- mean(temp[,3],na.rm = TRUE)
returns_benchmark[5,4] <- mean(temp[,4],na.rm = TRUE)
returns_benchmark[5,5] <- mean(temp[,5],na.rm = TRUE)

save(returns_benchmark,file=paste(window_start,"_", window_end,"_", lookback,"_five_minut_VAR1_benchmark_XBT.Rdata", sep = ""))

# analysis part 2 - same time frame as ARIMA
load(file=paste(window_start,"_", window_end,"_", lookback,"_time_vector_five_minut_ARIMA_fit_XBT.Rdata", sep = ""))

forecasts <- five_minute_data[which(five_minute_data$time %in% time_vector),]

#actual returns share
returns_benchmark <- five_minute_data[which(five_minute_data$time %in% time_vector),]
returns_benchmark$five  <- nrow(returns_benchmark[which(returns_benchmark$R_5 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m5)))*100
returns_benchmark$ten <- nrow(returns_benchmark[which(returns_benchmark$R_10 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m10)))*100
returns_benchmark$fifteen <- nrow(returns_benchmark[which(returns_benchmark$R_15 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m15)))*100
returns_benchmark$thirty <- nrow(returns_benchmark[which(returns_benchmark$R_30 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m30)))*100
returns_benchmark$sixty  <- nrow(returns_benchmark[which(returns_benchmark$R_60 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m60)))*100
returns_benchmark <- returns_benchmark[1,-c(1:56)]

#MDA
returns_benchmark[2,1] <- sum(forecasts$A_DA_5,na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_5)))*100
returns_benchmark[2,2] <- sum(forecasts$A_DA_10, na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_10)))*100
returns_benchmark[2,3] <- sum(forecasts$A_DA_15,na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_15)))*100
returns_benchmark[2,4] <- sum(forecasts$A_DA_30,na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_30)))*100
returns_benchmark[2,5] <- sum(forecasts$A_DA_60, na.rm = TRUE) / (nrow(forecasts) - sum(is.na(forecasts$A_DA_60)))*100

#actual change in %
temp <- as.data.frame(matrix(1, ncol = 5, nrow = nrow(forecasts)))
for(i in 1:nrow(forecasts)){
  temp[i,1] <- (forecasts$R_5[i] - 1)*100
  temp[i,2] <- (forecasts$R_10[i] -1)*100
  temp[i,3] <- (forecasts$R_15[i] -1)*100
  temp[i,4] <- (forecasts$R_30[i] -1)*100
  temp[i,5] <- (forecasts$R_60[i] -1)*100
}

returns_benchmark[3,1] <- mean(temp[,1],na.rm = TRUE)
returns_benchmark[3,2] <- mean(temp[,2],na.rm = TRUE)
returns_benchmark[3,3] <- mean(temp[,3],na.rm = TRUE)
returns_benchmark[3,4] <- mean(temp[,4],na.rm = TRUE)
returns_benchmark[3,5] <- mean(temp[,5],na.rm = TRUE)

#Actual preditcion
temp <- as.data.frame(matrix(1, ncol = 5, nrow = nrow(forecasts)))
for(i in 1:nrow(forecasts)){
  temp[i,1] <- ((forecasts$A_m5[i]) - 1)*100
  temp[i,2] <- ((forecasts$A_m10[i]) -1)*100
  temp[i,3] <- ((forecasts$A_m15[i]) -1)*100
  temp[i,4] <- ((forecasts$A_m30[i]) -1)*100
  temp[i,5] <- ((forecasts$A_m60[i]) -1)*100
}

returns_benchmark[4,1] <- mean(temp[,1],na.rm = TRUE)
returns_benchmark[4,2] <- mean(temp[,2],na.rm = TRUE)
returns_benchmark[4,3] <- mean(temp[,3],na.rm = TRUE)
returns_benchmark[4,4] <- mean(temp[,4],na.rm = TRUE)
returns_benchmark[4,5] <- mean(temp[,5],na.rm = TRUE)

# MSE of the prediction
temp <- as.data.frame(matrix(1, ncol = 5, nrow = nrow(forecasts)))
for(i in 1:nrow(forecasts)){
  temp[i,1] <- ((forecasts$A_m5[i] - forecasts$R_5[i])^2)
  temp[i,2] <- ((forecasts$A_m10[i] - forecasts$R_10[i])^2)
  temp[i,3] <- ((forecasts$A_m15[i] - forecasts$R_15[i])^2)
  temp[i,4] <- ((forecasts$A_m30[i] - forecasts$R_30[i])^2)
  temp[i,5] <- ((forecasts$A_m60[i] - forecasts$R_60[i])^2)
}

returns_benchmark[5,1] <- mean(temp[,1],na.rm = TRUE)
returns_benchmark[5,2] <- mean(temp[,2],na.rm = TRUE)
returns_benchmark[5,3] <- mean(temp[,3],na.rm = TRUE)
returns_benchmark[5,4] <- mean(temp[,4],na.rm = TRUE)
returns_benchmark[5,5] <- mean(temp[,5],na.rm = TRUE)

save(returns_benchmark,file=paste(window_start,"_", window_end,"_", lookback,"_five_minut_VAR2_benchmark_XBT.Rdata", sep = ""))

}


# ******************* plot "analysis"********************** 

# **********  5 minute*******************************************
ggplot(five_minute_data, aes(five_minute_data$time)) +                                    
  geom_line(aes(y=five_minute_data$A_m5), colour="red") +               #estimates         
  geom_line(aes(y=five_minute_data$R_5), colour="green")+               #real
  geom_line(aes(y=five_minute_data$`A_u5(95%)`), colour="black")+       #upper estimated bound
  geom_line(aes(y=five_minute_data$`A_l5(95%)`), colour="black")        #lower estimated bound

# **********  10 minute*******************************************
ggplot(five_minute_data, aes(five_minute_data$time)) +                                    
  geom_line(aes(y=five_minute_data$A_m10), colour="red") +               #estimates         
  geom_line(aes(y=five_minute_data$R_10), colour="green")+               #real
  geom_line(aes(y=five_minute_data$`A_u10(95%)`), colour="black")+       #upper estimated bound
  geom_line(aes(y=five_minute_data$`A_l10(95%)`), colour="black")        #lower estimated bound



# **********  15 minute*******************************************
ggplot(five_minute_data, aes(five_minute_data$time)) +                                    
  geom_line(aes(y=five_minute_data$A_m15), colour="red") +               #estimates         
  geom_line(aes(y=five_minute_data$R_15), colour="green")+               #real
  geom_line(aes(y=five_minute_data$`A_u15(95%)`), colour="black")+       #upper estimated bound
  geom_line(aes(y=five_minute_data$`A_l15(95%)`), colour="black")        #lower estimated bound

# **********  30 minute*******************************************
ggplot(five_minute_data, aes(five_minute_data$time)) +                                    
  geom_line(aes(y=five_minute_data$A_m30), colour="red") +               #estimates         
  geom_line(aes(y=five_minute_data$R_30), colour="green")+               #real
  geom_line(aes(y=five_minute_data$`A_u30(95%)`), colour="black")+       #upper estimated bound
  geom_line(aes(y=five_minute_data$`A_l30(95%)`), colour="black")        #lower estimated bound

# **********  60 minute*******************************************
ggplot(five_minute_data, aes(five_minute_data$time)) +                                    
  geom_line(aes(y=five_minute_data$A_m60), colour="red") +               #estimates         
  geom_line(aes(y=five_minute_data$R_60), colour="green")+               #real
  geom_line(aes(y=five_minute_data$`A_u60(95%)`), colour="black")+       #upper estimated bound
  geom_line(aes(y=five_minute_data$`A_l60(95%)`), colour="black")        #lower estimated bound
