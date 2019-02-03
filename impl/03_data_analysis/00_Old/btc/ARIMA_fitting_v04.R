setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/test_sample/bitcoin", sep = "")
setwd(wd) 
rm(list=ls())

library(ggplot2)
# 1) *********** data prep ******************************************
load(file = "pacf_1day_5mi_bc.Rdata")
load(file = "five_minute_data_ret_bc.Rdata")

# 2) ********* lag analysis (Showing inconsisntency) ***************************************


time_pacf$period <- time_pacf$Lag1 > 0.05
 plot(time_pacf$period)
 
 par(mfrow=c(3,1))
 plot(time_pacf$Lag1)
 plot(time_pacf$Lag2)
 plot(time_pacf$Lag3)
 plot(time_pacf$Lag4)
 plot(time_pacf$Lag5)

 
 
# 3) *********** preparing structure for fit ******************************

# Progress bar
 # before loop pb <- tkProgressBar(title = "progress bar", min = 0, max = nrow(OM_uni), width = 1000)
 # into the loop setTkProgressBar(pb, i, label=paste( round(i/(nrow(OM_uni))*1000, 0),"‰ done"))
 
 #prepring struncture
 five_minute_data[,19:50] <- 1
 colnames(five_minute_data)[c(19,22,25,28,31)] <- c("A_m5","A_m10","A_m15","A_m30", "A_m60")                             #mean for x minutes forward estimate
 colnames(five_minute_data)[c(20,23,26,29,32)] <- c("A_u5(95%)", "A_u10(95%)", "A_u15(95%)", "A_u30(95%)","A_u60(95%)")  #upper bound for x minutes forward estimate
 colnames(five_minute_data)[c(21,24,27,30,33)] <- c("A_l5(95%)" ,"A_l10(95%)", "A_l15(95%)", "A_l30(95%)", "A_l60(95%)")  # lower bound for x minutes forward estimate
 colnames(five_minute_data)[c(34,35)] <- c("A_BIC", "A_AICC")                                                            # AIC a BIC criteria for choosing proper order
 colnames(five_minute_data)[c(36:40)] <- c("R_5","R_10","R_15","R_30", "R_60")                                          #real values
 colnames(five_minute_data)[c(41:45)] <- c("A_SQe_5","A_SQe_10","A_SQe_15","A_SQe_30", "A_SQe_60")                      #squared errors of estimate 
 colnames(five_minute_data)[c(46:50)] <- c("A_DA_5","A_DA_10","A_DA_15","A_DA_30", "A_DA_60")                           #directional accuracy 

 # 4) *************** looping to fit ARIMA ******************************************************************************************
 pb <- tkProgressBar(title = "progress bar", min = 0, max = nrow(five_minute_data), width = 1000)
 
 timeframe <- 288 #day in 5minutes blocks
 shift <- timeframe - 1 # last block of time frame it starts forecasting 5/10/15/30/60 mins
 
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
 five_minute_data[i+shift,36] <- log(five_minute_data[i+shift+1,2]/five_minute_data[i+shift,2]) + 1
 five_minute_data[i+shift,37] <- log(five_minute_data[i+shift+2,2]/five_minute_data[i+shift,2]) + 1
 five_minute_data[i+shift,38] <- log(five_minute_data[i+shift+3,2]/five_minute_data[i+shift,2]) + 1
 five_minute_data[i+shift,39] <- log(five_minute_data[i+shift+6,2]/five_minute_data[i+shift,2]) + 1
 five_minute_data[i+shift,40] <- log(five_minute_data[i+shift+12,2]/five_minute_data[i+shift,2]) + 1
 
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
 
 setTkProgressBar(pb, i, label=paste( round(i/(nrow(five_minute_data))*1000, 0),"‰ done"))
 
 }
 
 save(five_minute_data,file="five_minut_A_fit_288_bc.Rdata")
 
 #MSE
 mean(five_minute_data$A_SQe_5[289:5999])
 mean(five_minute_data$A_SQe_10[289:5999])
 mean(five_minute_data$A_SQe_15[289:5999])
 mean(five_minute_data$A_SQe_30[289:5999])
 mean(five_minute_data$A_SQe_60[289:5999])
 
 #MDA
 mean(five_minute_data$A_DA_5[289:5998])
 mean(five_minute_data$A_DA_10[290:5997])
 mean(five_minute_data$A_DA_15[291:5996])
 mean(five_minute_data$A_DA_30[295:5992])
 mean(five_minute_data$A_DA_60[301:5986])
 
 
 # ******************* plot "analysis"********************** 
 
 begin <- 2000 #start of the window
 end <- 2300 # end of the window
 # **********  5 minute*******************************************
 ggplot(five_minute_data[begin:end,], aes(five_minute_data$time_min[begin:end])) +                                    
   geom_line(aes(y=five_minute_data$A_m5[begin:end]), colour="red") +               #estimates         
   geom_line(aes(y=five_minute_data$R_5[begin:end]), colour="green")+               #real
   geom_line(aes(y=five_minute_data$`A_u5(95%)`[begin:end]), colour="black")+       #upper estimated bound
   geom_line(aes(y=five_minute_data$`A_l5(95%)`[begin:end]), colour="black")        #lower estimated bound
 
 # **********  10 minute*******************************************
 ggplot(five_minute_data[begin:end,], aes(five_minute_data$time_min[begin:end])) +                                    
   geom_line(aes(y=five_minute_data$A_m10[begin:end]), colour="red") +               #estimates         
   geom_line(aes(y=five_minute_data$R_10[begin:end]), colour="green")+               #real
   geom_line(aes(y=five_minute_data$`A_u10(95%)`[begin:end]), colour="black")+       #upper estimated bound
   geom_line(aes(y=five_minute_data$`A_l10(95%)`[begin:end]), colour="black")        #lower estimated bound
 
 
 
 # **********  15 minute*******************************************
 ggplot(five_minute_data[begin:end,], aes(five_minute_data$time_min[begin:end])) +                                    
   geom_line(aes(y=five_minute_data$A_m15[begin:end]), colour="red") +               #estimates         
   geom_line(aes(y=five_minute_data$R_15[begin:end]), colour="green")+               #real
   geom_line(aes(y=five_minute_data$`A_u15(95%)`[begin:end]), colour="black")+       #upper estimated bound
   geom_line(aes(y=five_minute_data$`A_l15(95%)`[begin:end]), colour="black")        #lower estimated bound
 
 # **********  30 minute*******************************************
 ggplot(five_minute_data[begin:end,], aes(five_minute_data$time_min[begin:end])) +                                    
   geom_line(aes(y=five_minute_data$A_m30[begin:end]), colour="red") +               #estimates         
   geom_line(aes(y=five_minute_data$R_30[begin:end]), colour="green")+               #real
   geom_line(aes(y=five_minute_data$`A_u30(95%)`[begin:end]), colour="black")+       #upper estimated bound
   geom_line(aes(y=five_minute_data$`A_l30(95%)`[begin:end]), colour="black")        #lower estimated bound
 
 # **********  60 minute*******************************************
 ggplot(five_minute_data[begin:end,], aes(five_minute_data$time_min[begin:end])) +                                    
   geom_line(aes(y=five_minute_data$A_m60[begin:end]), colour="red") +               #estimates         
   geom_line(aes(y=five_minute_data$R_60[begin:end]), colour="green")+               #real
   geom_line(aes(y=five_minute_data$`A_u60(95%)`[begin:end]), colour="black")+       #upper estimated bound
   geom_line(aes(y=five_minute_data$`A_l60(95%)`[begin:end]), colour="black")        #lower estimated bound