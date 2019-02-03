setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken", sep = "")
setwd(wd) 
rm(list=ls())


library(ggplot2)
# 1) *********** data prep ******************************************
load(file="XBT_5minute_returns.Rdata")

# 2) *********** preparing structure for fit ******************************

#setting up parametr of the window
window_start <- 31200 #any number from number of lags to nrow-1
window_end <- 32200 #any number from start to nrow
lookback <- 72

five_minute_data <- five_minute_data[c((window_start-lookback):window_end),]

# Progress bar
 # before loop pb <- tkProgressBar(title = "progress bar", min = 0, max = nrow(OM_uni), width = 1000)
 # into the loop setTkProgressBar(pb, i, label=paste( round(i/(nrow(OM_uni))*1000, 0),"‰ done"))
 
 #preparing struncture
 five_minute_data[,19:50] <- 1
 colnames(five_minute_data)[c(19,22,25,28,31)] <- c("A_m5","A_m10","A_m15","A_m30", "A_m60")                             #mean for x minutes forward estimate
 colnames(five_minute_data)[c(20,23,26,29,32)] <- c("A_u5(95%)", "A_u10(95%)", "A_u15(95%)", "A_u30(95%)","A_u60(95%)")  #upper bound for x minutes forward estimate
 colnames(five_minute_data)[c(21,24,27,30,33)] <- c("A_l5(95%)" ,"A_l10(95%)", "A_l15(95%)", "A_l30(95%)", "A_l60(95%)")  # lower bound for x minutes forward estimate
 colnames(five_minute_data)[c(34,35)] <- c("A_BIC", "A_AICC")                                                            # AIC a BIC criteria for choosing proper order
 colnames(five_minute_data)[c(36:40)] <- c("R_5","R_10","R_15","R_30", "R_60")                                          #real values
 colnames(five_minute_data)[c(41:45)] <- c("A_SQe_5","A_SQe_10","A_SQe_15","A_SQe_30", "A_SQe_60")                      #squared errors of estimate 
 colnames(five_minute_data)[c(46:50)] <- c("A_DA_5","A_DA_10","A_DA_15","A_DA_30", "A_DA_60")                           #directional accuracy 

 # 4) *************** looping to fit ARIMA ******************************************************************************************
 timeframe <- lookback #reference period
 shift <- timeframe - 1 # last block of time frame it starts forecasting 5/10/15/30/60 mins
 
 pb <- tkProgressBar(title = "progress bar", min = 0, max = (nrow(five_minute_data)- timeframe), width = 1000)
 
 for(i in 1:(nrow(five_minute_data)-timeframe)){
 
  fit <- auto.arima(five_minute_data[(i:(i+shift)),18], d = 0)
 forecast60 <- forecast(fit, h=12)
 # fitting 5 minutes ahead
 five_minute_data[i+shift,19] <- forecast60$mean[1] + 1
 five_minute_data[i+shift,20] <- forecast60$upper[1,2] + 1
 five_minute_data[i+shift,21] <- forecast60$lower[1,2] + 1
 
 
 # fitting 10 minute ahead
 for(z in 1:2){
   five_minute_data[i+shift,22] <- (forecast60$mean[z] + 1) *  five_minute_data[i+shift,22]
   five_minute_data[i+shift,23] <- (forecast60$upper[z,2] + 1) * five_minute_data[i+shift,23]
   five_minute_data[i+shift,24] <- (forecast60$lower[z,2] + 1) * five_minute_data[i+shift,24]
 }
 
 # fitting 15 minute ahead
 for(z in 1:3){
   five_minute_data[i+shift,25] <- (forecast60$mean[z] + 1) *  five_minute_data[i+shift,25]
   five_minute_data[i+shift,26] <- (forecast60$upper[z,2] + 1) * five_minute_data[i+shift,26]
   five_minute_data[i+shift,27] <- (forecast60$lower[z,2] + 1) * five_minute_data[i+shift,27]
 }
 
 # fitting 30 minute ahead
 for(z in 1:6){
   five_minute_data[i+shift,28] <- (forecast60$mean[z] + 1) *  five_minute_data[i+shift,28]
   five_minute_data[i+shift,29] <- (forecast60$upper[z,2] + 1) * five_minute_data[i+shift,29]
   five_minute_data[i+shift,30] <- (forecast60$lower[z,2] + 1) * five_minute_data[i+shift,30]
 }
 
 # fitting 60 minute ahead
 for(z in 1:12){
   five_minute_data[i+shift,31] <- (forecast60$mean[z] + 1) *  five_minute_data[i+shift,31]
   five_minute_data[i+shift,32] <- (forecast60$upper[z,2] + 1) * five_minute_data[i+shift,32]
   five_minute_data[i+shift,33] <- (forecast60$lower[z,2] + 1) * five_minute_data[i+shift,33]
 }

 five_minute_data[i+shift,34] <- forecast60$model$bic
 five_minute_data[i+shift,35] <- forecast60$model$aicc
 
 # gettting real returns
 five_minute_data[i+shift,36] <- log(five_minute_data[i+shift+1,3]/five_minute_data[i+shift,3]) + 1
 five_minute_data[i+shift,37] <- log(five_minute_data[i+shift+2,3]/five_minute_data[i+shift,3]) + 1
 five_minute_data[i+shift,38] <- log(five_minute_data[i+shift+3,3]/five_minute_data[i+shift,3]) + 1
 five_minute_data[i+shift,39] <- log(five_minute_data[i+shift+6,3]/five_minute_data[i+shift,3]) + 1
 five_minute_data[i+shift,40] <- log(five_minute_data[i+shift+12,3]/five_minute_data[i+shift,3]) + 1
 
 # gettting squred error
 five_minute_data[i+shift,41] <- (five_minute_data[i+shift,36] - five_minute_data[i+shift,19])^2
 five_minute_data[i+shift,42] <- (five_minute_data[i+shift,37] - five_minute_data[i+shift,22])^2
 five_minute_data[i+shift,43] <- (five_minute_data[i+shift,38] - five_minute_data[i+shift,25])^2
 five_minute_data[i+shift,44] <- (five_minute_data[i+shift,39] - five_minute_data[i+shift,28])^2
 five_minute_data[i+shift,45] <- (five_minute_data[i+shift,40] - five_minute_data[i+shift,31])^2
 
 # gettting direction accuracy
 five_minute_data[i+shift,46] <- (sign(five_minute_data[i+shift,36]-1) == sign(five_minute_data[i+shift,19]-1))*1
 five_minute_data[i+shift,47] <- (sign(five_minute_data[i+shift,37]-1) == sign(five_minute_data[i+shift,22]-1))*1
 five_minute_data[i+shift,48] <- (sign(five_minute_data[i+shift,38]-1) == sign(five_minute_data[i+shift,25]-1))*1
 five_minute_data[i+shift,49] <- (sign(five_minute_data[i+shift,39]-1) == sign(five_minute_data[i+shift,28]-1))*1
 five_minute_data[i+shift,50] <- (sign(five_minute_data[i+shift,40]-1) == sign(five_minute_data[i+shift,31]-1))*1
 
 setTkProgressBar(pb, i, label=paste( round(i/(nrow(five_minute_data)-timeframe)*1000, 0),"‰ done"))
 
 }
 
 save(five_minute_data,file=paste("five_minut_ARIMA_fit_XBT.Rdata",window_start, window_end, lookback, sep = ""))
 
 forecasts <- five_minute_data[which(five_minute_data$A_m5 !=1),]
 
 #actual returns share
 returns_benchmark <- five_minute_data[which(five_minute_data$A_m5 !=1),]
 returns_benchmark$five  <- nrow(returns_benchmark[which(returns_benchmark$R_5 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m5)))
 returns_benchmark$ten <- nrow(returns_benchmark[which(returns_benchmark$R_10 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m10)))
 returns_benchmark$fifteen <- nrow(returns_benchmark[which(returns_benchmark$R_15 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m15)))
 returns_benchmark$thirty <- nrow(returns_benchmark[which(returns_benchmark$R_30 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m30)))
 returns_benchmark$sixty  <- nrow(returns_benchmark[which(returns_benchmark$R_60 > 1),])/(nrow(returns_benchmark)-sum(is.na(five_minute_data$A_m60)))
 returns_benchmark <- returns_benchmark[1,-c(1:50)]
 
 #MDA
 returns_benchmark[2,1] <- sum(five_minute_data$A_DA_5[timeframe:nrow(five_minute_data)]) / (nrow(five_minute_data) - sum(five_minute_data$A_m5[which(five_minute_data$A_m5 == 1)])-sum(is.na(five_minute_data$A_m5)))
 returns_benchmark[2,2] <- sum(five_minute_data$A_DA_10[timeframe:nrow(five_minute_data)],na.rm = TRUE) / (nrow(five_minute_data) - sum(five_minute_data$A_m5[which(five_minute_data$A_m10 == 1)])-sum(is.na(five_minute_data$A_m10)))
 returns_benchmark[2,3] <- sum(five_minute_data$A_DA_15[timeframe:nrow(five_minute_data)],na.rm = TRUE) / (nrow(five_minute_data) - sum(five_minute_data$A_m5[which(five_minute_data$A_m15 == 1)])-sum(is.na(five_minute_data$A_m15)))
 returns_benchmark[2,4] <- sum(five_minute_data$A_DA_30[timeframe:nrow(five_minute_data)],na.rm = TRUE) / (nrow(five_minute_data) - sum(five_minute_data$A_m5[which(five_minute_data$A_m30 == 1)])-sum(is.na(five_minute_data$A_m30)))
 returns_benchmark[2,5] <- sum(five_minute_data$A_DA_60[timeframe:nrow(five_minute_data)],na.rm = TRUE) / (nrow(five_minute_data) - sum(five_minute_data$A_m5[which(five_minute_data$A_m60 == 1)])-sum(is.na(five_minute_data$A_m60)))

 #Miss in %
 temp <- as.data.frame(matrix(1, ncol = 5, nrow = nrow(forecasts)))
 for(i in 1:nrow(forecasts)){
 temp[i,1] <- (forecasts$A_m5[i] - forecasts$R_5[i]) * forecasts$price_all[i]
 temp[i,2] <- (forecasts$A_m10[i] - forecasts$R_10[i]) * forecasts$price_all[i]
 temp[i,3] <- (forecasts$A_m15[i] - forecasts$R_15[i]) * forecasts$price_all[i]
 temp[i,4] <- (forecasts$A_m30[i] - forecasts$R_30[i]) * forecasts$price_all[i]
 temp[i,5] <- (forecasts$A_m60[i] - forecasts$R_60[i]) * forecasts$price_all[i]
 }
 
 returns_benchmark[3,1] <- sum(temp[,1],na.rm = TRUE)
 returns_benchmark[3,2] <- sum(temp[,2],na.rm = TRUE)
 returns_benchmark[3,3] <- sum(temp[,3],na.rm = TRUE)
 returns_benchmark[3,4] <- sum(temp[,4],na.rm = TRUE)
 returns_benchmark[3,5] <- sum(temp[,5],na.rm = TRUE)
 

 
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