rm(list=ls())
clist <- c("XBT", "ETH", "XMR", "LTC")
for(w in clist){
setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/", w, sep = "")
setwd(wd) 



library("tseries")
library("zoo")
library("forecast")
library("ggplot2")
library('stats')
library("gridExtra")
library("reshape2")
library("tcltk")
library("sarima")
library("nortest")
library("lmtest")


#loop for multiple rolling window lenghts
for(q in c(14,21,28,42)){

  # 1) *********** data prep ******************************************
  load(file=paste(w,"_day_returns_quart.Rdata",sep = ""))
  
  # 2) *********** preparing structure for fit ******************************
  
  #setting up parametr of the window
  
  
  window_start <- q+1 #any number from number of lags to nrow-1
  window_end <- nrow(day_data) #any number from start to nrow
  lookback <- q
  
  day_data <- day_data[c((window_start-lookback):window_end),]
  
  print(q)
  
  # Progress bar
  # before loop pb <- tkProgressBar(title = "progress bar", min = 0, max = nrow(OM_uni), width = 1000)
  # into the loop setTkProgressBar(pb, i, label=paste( round(i/(nrow(OM_uni))*1000, 0),"‰ done"))
  
  #preparing struncture
  day_data[,29:68] <- 1
  day_data[,69] <- 0 
  day_data[,70:83] <- 0 
  colnames(day_data)[c(29,32,35,38,41)] <- c("A_m1","A_m3","A_m5","A_m10", "A_m15")                               #mean for x minutes forward estimate
  colnames(day_data)[c(30,33,36,39,42)] <- c("A_u1", "A_u3", "A_u5", "A_u10","A_u15")    #upper bound for x minutes forward estimate
  colnames(day_data)[c(31,34,37,40,43)] <- c("A_l1" ,"A_l3", "A_l5", "A_l10", "A_l15")   #lower bound for x minutes forward estimate
  colnames(day_data)[c(44,45)] <- c("A_BIC", "A_AICC")                                                            #AIC a BIC criteria for choosing proper order
  colnames(day_data)[c(46:50)] <- c("R_1","R_3","R_5","R_10", "R_15")                                             #real values
  colnames(day_data)[c(51:55)] <- c("A_SQe_1","A_SQe_3","A_SQe_5","A_SQe_10", "A_SQe_15")                         #squared errors of estimate 
  colnames(day_data)[c(56:60)] <- c("A_DA_1","A_DA_3","A_DA_5","A_DA_10", "A_DA_15")                              #directional accuracy 
  colnames(day_data)[c(61:63)] <- c("p","d","q")                                                                  #orders 
  colnames(day_data)[c(64:68)] <- c("Box.test_p_v", "Shapiro_Wilk_p_v","Lilliefors_p_v", "Zero_mean_ttest_p_v","Heteroscedasticity_p_v" ) #residuals testing
  colnames(day_data)[69] <- "WN_RW_indication"
  colnames(day_data)[c(70:76)] <- c("alpha", "beta", "gamma", "fi", "zeta", "theta", "eta")
  colnames(day_data)[c(77:83)] <- c("buy_sell", "eur", "crypto", "taker26_eur", "taker26_crypto","taker10_eur", "taker10_crypto")
  
  # 4) *************** looping to fit ARIMA ******************************************************************************************
  timeframe <- lookback #reference period
  shift <- timeframe - 1 # last block of time frame it starts forecasting 1/3/5/10/15 steps
  
  day_data$eur[shift] <- day_data$price_all[shift] #initial eur of 1 XBT to trade with
  day_data$taker26_eur[shift] <- day_data$price_all[shift] #initial of 1 XBT to trade with
  day_data$taker10_eur[shift] <- day_data$price_all[shift] #initial of 1 XBT to trade with  
  #pb <- tkProgressBar(title = "progress bar", min = 0, max = (nrow(day_data)- timeframe), width = 1000)
  
  
  #for(i in 100:500){  
  for(i in 1:(nrow(day_data)-timeframe)){

    
    fit <- auto.arima(day_data[(i:(i+shift)),18])
    forecast60 <- forecast(fit, h=15)
    # fitting 1 step ahead
    day_data$A_m1[i+shift] <- forecast60$mean[1] + 1
    day_data$A_u1[i+shift] <- forecast60$upper[1,2] + 1
    day_data$A_l1[i+shift] <- forecast60$lower[1,2] + 1
    
    
    # fitting 3 steps ahead
    for(z in 1:3){
      day_data$A_m3[i+shift] <- (forecast60$mean[z] + 1) *  day_data$A_m3[i+shift] 
      day_data$A_u3[i+shift] <- (forecast60$upper[z,2] + 1) * day_data$A_u3[i+shift] 
      day_data$A_l3[i+shift] <- (forecast60$lower[z,2] + 1) * day_data$A_l3[i+shift]
    }
    
    # fitting 5 steps ahead
    for(z in 1:5){
      day_data$A_m5[i+shift] <- (forecast60$mean[z] + 1) *  day_data$A_m5[i+shift]
      day_data$A_u5[i+shift] <- (forecast60$upper[z,2] + 1) * day_data$A_u5[i+shift]
      day_data$A_l5[i+shift] <- (forecast60$lower[z,2] + 1) * day_data$A_l5[i+shift]
    }
    
    # fitting 10 steps ahead
    for(z in 1:10){
      day_data$A_m10[i+shift] <- (forecast60$mean[z] + 1) *  day_data$A_m10[i+shift]
      day_data$A_u10[i+shift] <- (forecast60$upper[z,2] + 1) * day_data$A_u10[i+shift]
      day_data$A_l10[i+shift] <- (forecast60$lower[z,2] + 1) * day_data$A_l10[i+shift]
    }
    
    # fitting 15 steps ahead
    for(z in 1:15){
      day_data$A_m15[i+shift] <- (forecast60$mean[z] + 1) *  day_data$A_m15[i+shift]
      day_data$A_u15[i+shift] <- (forecast60$upper[z,2] + 1) *  day_data$A_u15[i+shift]
      day_data$A_l15[i+shift] <- (forecast60$lower[z,2] + 1) * day_data$A_l15[i+shift]
    }
    
    day_data$A_BIC[i+shift] <- forecast60$model$bic
    day_data$A_AICC[i+shift] <- forecast60$model$aicc
    
    # gettting real returns
    day_data$R_1[i+shift] <- log(day_data$price_all[i+shift+1]/day_data$price_all[i+shift]) + 1
    day_data$R_3[i+shift] <- log(day_data$price_all[i+shift+2]/day_data$price_all[i+shift]) + 1
    day_data$R_5[i+shift] <- log(day_data$price_all[i+shift+3]/day_data$price_all[i+shift]) + 1
    day_data$R_10[i+shift] <- log(day_data$price_all[i+shift+6]/day_data$price_all[i+shift]) + 1
    day_data$R_15[i+shift] <- log(day_data$price_all[i+shift+12]/day_data$price_all[i+shift]) + 1
    
    # gettting squred error
    day_data$A_SQe_1[i+shift] <- (day_data$R_1[i+shift] - day_data$A_m1[i+shift])^2
    day_data$A_SQe_3[i+shift] <- (day_data$R_3[i+shift] - day_data$A_m3[i+shift])^2
    day_data$A_SQe_5[i+shift] <- (day_data$R_5[i+shift] - day_data$A_m5[i+shift])^2
    day_data$A_SQe_10[i+shift] <- (day_data$R_10[i+shift] - day_data$A_m10[i+shift])^2
    day_data$A_SQe_15[i+shift] <- (day_data$R_15[i+shift] - day_data$A_m15[i+shift])^2
    
    # gettting direction accuracy
    day_data$A_DA_1[i+shift] <- (sign(day_data$R_1[i+shift]-1) == sign(day_data$A_m1[i+shift]-1))*1
    day_data$A_DA_3[i+shift] <- (sign(day_data$R_3[i+shift]-1) == sign(day_data$A_m3[i+shift]-1))*1
    day_data$A_DA_5[i+shift] <- (sign(day_data$R_5[i+shift]-1) == sign(day_data$A_m5[i+shift]-1))*1
    day_data$A_DA_10[i+shift] <- (sign(day_data$R_10[i+shift]-1) == sign(day_data$A_m10[i+shift]-1))*1
    day_data$A_DA_15[i+shift] <- (sign(day_data$R_15[i+shift]-1) == sign(day_data$A_m15[i+shift]-1))*1
    
    #extracting lags
    day_data$p[i+shift] <- arimaorder(fit)[1]
    day_data$d[i+shift] <- arimaorder(fit)[2]
    day_data$q[i+shift] <- arimaorder(fit)[3]
    
    #testing
    #Box test
    t <- try(Box.test (fit$residuals, lag = 10)[3])
    if(inherits(t, "try-error")){
      day_data$Box.test_p_v[i+shift] <- NA
    } else {
      day_data$Box.test_p_v[i+shift] <- t
    }
   
    #Shapiro test
    t <- try(shapiro.test(fit$residuals)[2])
    if(inherits(t, "try-error")){
      day_data$Shapiro_Wilk_p_v[i+shift] <- NA
    } else {
      day_data$Shapiro_Wilk_p_v[i+shift] <- t
    }
    
    #Lillie test
    t <- try(lillie.test(fit$residuals)[2])
    if(inherits(t, "try-error")){
      day_data$Lilliefors_p_v[i+shift] <- NA
    } else {
      day_data$Lilliefors_p_v[i+shift] <- t
    }

    #t.test test
    t <- try(t.test(fit$residuals)[3])
    if(inherits(t, "try-error")){
      day_data$Zero_mean_ttest_p_v[i+shift] <- NA
    } else {
      day_data$Zero_mean_ttest_p_v[i+shift] <- t
    }

    #Box^2 test
    t <- try(Box.test(fit$residuals^2, lag = 10)[3])
    if(inherits(t, "try-error")){
      day_data$Heteroscedasticity_p_v[i+shift] <- NA
    } else {
      day_data$Heteroscedasticity_p_v[i+shift] <- t
    }

    day_data$WN_RW_indication[i+shift] <- day_data$p[i+shift] + day_data$q[i+shift]
    
    #extracting model parameters
    day_data$alpha[i+shift] <- fit$coef[2]
    day_data$beta[i+shift] <- fit$coef[1] * (day_data$p[i+shift] > 0)
    
    # buy / sell decision
  if(day_data$A_m1[i + shift] < 1)
      {day_data$`buy_sell`[i + shift] <- "S"}
      else if (day_data$A_m1[i + shift] > 1) 
      {day_data$`buy_sell`[i + shift] <- "B"}
      else 
      {day_data$`buy_sell`[i + shift] <- "H"}
    
  #calculating portoflio
  if(day_data$`buy_sell`[i + shift] == "S")
  {day_data$eur[i + shift] <- day_data$eur[i + shift-1] + day_data$crypto[i + shift-1]*day_data$price_all[i + shift]
  day_data$crypto[i + shift] <- 0 }
    else if (day_data$`buy_sell`[i + shift] == "B") 
    {day_data$eur[i + shift] <- 0
    day_data$crypto[i + shift] <- day_data$eur[i + shift-1]/day_data$price_all[i + shift] + day_data$crypto[i + shift-1]}
    else 
    {day_data$crypto[i + shift] <- day_data$crypto[i + shift-1]
      day_data$eur[i + shift] <- day_data$eur[i + shift-1]}
    
    #calculating net portfolio - taker 26
    if(day_data$`buy_sell`[i + shift] == "S")
    {day_data$taker26_eur[i + shift] <- day_data$taker26_eur[i + shift-1] + day_data$taker26_crypto[i + shift-1]*day_data$price_all[i + shift]*0.9974
    day_data$taker26_crypto[i + shift] <- 0 }
    else if (day_data$`buy_sell`[i + shift] == "B") 
    {day_data$taker26_eur[i + shift] <- 0
    day_data$taker26_crypto[i + shift] <- day_data$taker26_eur[i + shift-1]/day_data$price_all[i + shift]*0.9974 + day_data$taker26_crypto[i + shift-1]}
    else 
    {day_data$taker26_crypto[i + shift] <- day_data$taker26_crypto[i + shift-1]
    day_data$taker26_eur[i + shift] <- day_data$taker26_eur[i + shift-1]}

    #calculating net portfolio - taker 10
    if(day_data$`buy_sell`[i + shift] == "S")
    {day_data$taker10_eur[i + shift] <- day_data$taker10_eur[i + shift-1] + day_data$taker10_crypto[i + shift-1]*day_data$price_all[i + shift]*0.999
    day_data$taker10_crypto[i + shift] <- 0 }
    else if (day_data$`buy_sell`[i + shift] == "B") 
    {day_data$taker10_eur[i + shift] <- 0
    day_data$taker10_crypto[i + shift] <- day_data$taker10_eur[i + shift-1]/day_data$price_all[i + shift]*0.999 + day_data$taker10_crypto[i + shift-1]}
    else 
    {day_data$taker10_crypto[i + shift] <- day_data$taker10_crypto[i + shift-1]
    day_data$taker10_eur[i + shift] <- day_data$taker10_eur[i + shift-1]}
        

        #setTkProgressBar(pb, i, label=paste( round(i/(nrow(day_data)-timeframe)*1000, 0),"‰ done"))
    print(paste(w,"_",q, "_",i))
    
  }
  
  par(mfrow=c(2,2))
  plot(day_data$price_all,pch=16,cex = 0.3, ylim=c(0, max(day_data$eur,day_data$price_all)),xlab='' , ylab = "", main="Price")
  plot(day_data$eur,pch=16,cex = 0.3, ylim=c(0, max(day_data$eur,day_data$price_all)),xlab='' , ylab = "", main="0% fees")
  plot(day_data$taker26_eur,pch=16,cex = 0.3,ylim=c(0, max(day_data$eur,day_data$price_all)),xlab='' , ylab = "", main="0.26% fees")
  plot(day_data$taker10_eur,pch=16,cex = 0.3,ylim=c(0, max(day_data$eur,day_data$price_all)),xlab='' , ylab = "", main="0.10% fees")
  
  save(day_data,file=paste(w,"_day_ARIMA_fit_",lookback ,".Rdata",sep = ""))

}
}
    
  forecasts <- day_data[which(day_data$WN_RW_indication !=0), ]
  time_vector <- forecasts$time
  save(time_vector,file=paste(window_start,"_", window_end,"_", lookback,"_time_vector_day_ARIMA_fit_XBT.Rdata", sep = ""))
  
  
  
  #actual returns share
  returns_benchmark <- day_data[which(day_data$WN_RW_indication !=0), ]
  returns_benchmark$one  <- nrow(returns_benchmark[which(returns_benchmark$R_1 > 1),])/(nrow(returns_benchmark)-sum(is.na(day_data$A_m1)))*100
  returns_benchmark$three <- nrow(returns_benchmark[which(returns_benchmark$R_3 > 1),])/(nrow(returns_benchmark)-sum(is.na(day_data$A_m3)))*100
  returns_benchmark$five <- nrow(returns_benchmark[which(returns_benchmark$R_5 > 1),])/(nrow(returns_benchmark)-sum(is.na(day_data$A_m5)))*100
  returns_benchmark$ten <- nrow(returns_benchmark[which(returns_benchmark$R_10 > 1),])/(nrow(returns_benchmark)-sum(is.na(day_data$A_m10)))*100
  returns_benchmark$fifteen  <- nrow(returns_benchmark[which(returns_benchmark$R_15 > 1),])/(nrow(returns_benchmark)-sum(is.na(day_data$A_m15)))*100
  returns_benchmark <- returns_benchmark[1,-c(1:59)]
  
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
  
  
  save(returns_benchmark,file=paste(window_start,"_", window_end,"_", lookback,"_day_ARIMA_benchmark_XBT.Rdata", sep = ""))
  
  diagnostics <- as.data.frame(matrix(data = 1, nrow = 1, ncol = 9))
  colnames(diagnostics) <- c("p", "d", "q", "Box.test_p_v", "Shapiro_Wilk_p_v","Lilliefors_p_v", "Zero_mean_ttest_p_v", "Heteroscedasticity_p_v", "WN_share" )
  diagnostics$p <- mean(forecasts$p, na.rm = TRUE)
  diagnostics$d <- mean(forecasts$d, na.rm = TRUE)
  diagnostics$q <- mean(forecasts$q, na.rm = TRUE)
  diagnostics$Box.test_p_v <- mean(forecasts$Box.test_p_v, na.rm = TRUE)
  diagnostics$`Shapiro_Wilk_p_v` <- mean(forecasts$`Shapiro_Wilk_p_v`, na.rm = TRUE)
  diagnostics$Lilliefors_p_v <- mean(forecasts$Lilliefors_p_v, na.rm = TRUE)
  diagnostics$Zero_mean_ttest_p_v <- mean(forecasts$Zero_mean_ttest_p_v, na.rm = TRUE)
  diagnostics$Heteroscedasticity_p_v <- mean(forecasts$Heteroscedasticity_p_v, na.rm = TRUE)
  diagnostics$WN_share <- 1 - (nrow(forecasts)/nrow(day_data))
  
  save(diagnostics,file=paste(window_start,"_", window_end,"_", lookback,"_day_ARIMA_diagnostics_XBT.Rdata", sep = ""))


