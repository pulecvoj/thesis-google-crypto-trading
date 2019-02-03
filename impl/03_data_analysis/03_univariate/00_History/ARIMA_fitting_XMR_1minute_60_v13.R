setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
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
library("nortest")
library("lmtest")


#loop for multiple rolling window lenghts
q <- 60
  # 1) *********** data prep ******************************************
  load(file="XMR_1minute_returns.Rdata")
  
  # 2) *********** preparing structure for fit ******************************
  
  #setting up parametr of the window
  
  
  window_start <- q+1 #any number from number of lags to nrow-1
  window_end <- nrow(minute_data) #any number from start to nrow
  lookback <- q
  
  minute_data <- minute_data[c((window_start-lookback):window_end),]
  
  print(q)
  
  # Progress bar
  # before loop pb <- tkProgressBar(title = "progress bar", min = 0, max = nrow(OM_uni), width = 1000)
  # into the loop setTkProgressBar(pb, i, label=paste( round(i/(nrow(OM_uni))*1000, 0),"‰ done"))
  
  #preparing struncture
  minute_data[,19:58] <- 1
  minute_data[,59] <- 0 
  colnames(minute_data)[c(19,22,25,28,31)] <- c("A_m1","A_m3","A_m5","A_m10", "A_m15")                               #mean for x minutes forward estimate
  colnames(minute_data)[c(20,23,26,29,32)] <- c("A_u1(95%)", "A_u3(95%)", "A_u5(95%)", "A_u10(95%)","A_u15(95%)")    #upper bound for x minutes forward estimate
  colnames(minute_data)[c(21,24,27,30,33)] <- c("A_l1(95%)" ,"A_l3(95%)", "A_l5(95%)", "A_l10(95%)", "A_l15(95%)")   # lower bound for x minutes forward estimate
  colnames(minute_data)[c(34,35)] <- c("A_BIC", "A_AICC")                                                            # AIC a BIC criteria for choosing proper order
  colnames(minute_data)[c(36:40)] <- c("R_1","R_3","R_5","R_10", "R_15")                                             #real values
  colnames(minute_data)[c(41:45)] <- c("A_SQe_1","A_SQe_3","A_SQe_5","A_SQe_10", "A_SQe_15")                         #squared errors of estimate 
  colnames(minute_data)[c(46:50)] <- c("A_DA_1","A_DA_3","A_DA_5","A_DA_10", "A_DA_15")                              #directional accuracy 
  colnames(minute_data)[c(51:53)] <- c("p","d","q")                                                                  #orders 
  colnames(minute_data)[c(54:58)] <- c("Box.test_p_v", "Shapiro-Wilk_p_v","Lilliefors_p_v", "Zero_mean_ttest_p_v", "Heteroscedasticity_p_v" ) #residuals testing
  colnames(minute_data)[59] <- "WN_RW_indication"
  
  # 4) *************** looping to fit ARIMA ******************************************************************************************
  timeframe <- lookback #reference period
  shift <- timeframe - 1 # last block of time frame it starts forecasting 1/3/5/10/15 mins
  
  #pb <- tkProgressBar(title = "progress bar", min = 0, max = (nrow(minute_data)- timeframe), width = 1000)
  
  
  #for(i in 54500:54750){  
  for(i in 1:(nrow(minute_data)-timeframe)){

    
    fit <- auto.arima(minute_data[(i:(i+shift)),18], d = 0)
    forecast60 <- forecast(fit, h=15)
    # fitting 1 minutes ahead
    minute_data[i+shift,19] <- forecast60$mean[1] + 1
    minute_data[i+shift,20] <- forecast60$upper[1,2] + 1
    minute_data[i+shift,21] <- forecast60$lower[1,2] + 1
    
    
    # fitting 3 minute ahead
    for(z in 1:3){
      minute_data[i+shift,22] <- (forecast60$mean[z] + 1) *  minute_data[i+shift,22]
      minute_data[i+shift,23] <- (forecast60$upper[z,2] + 1) * minute_data[i+shift,23]
      minute_data[i+shift,24] <- (forecast60$lower[z,2] + 1) * minute_data[i+shift,24]
    }
    
    # fitting 5 minute ahead
    for(z in 1:5){
      minute_data[i+shift,25] <- (forecast60$mean[z] + 1) *  minute_data[i+shift,25]
      minute_data[i+shift,26] <- (forecast60$upper[z,2] + 1) * minute_data[i+shift,26]
      minute_data[i+shift,27] <- (forecast60$lower[z,2] + 1) * minute_data[i+shift,27]
    }
    
    # fitting 10 minute ahead
    for(z in 1:10){
      minute_data[i+shift,28] <- (forecast60$mean[z] + 1) *  minute_data[i+shift,28]
      minute_data[i+shift,29] <- (forecast60$upper[z,2] + 1) * minute_data[i+shift,29]
      minute_data[i+shift,30] <- (forecast60$lower[z,2] + 1) * minute_data[i+shift,30]
    }
    
    # fitting 15 minute ahead
    for(z in 1:15){
      minute_data[i+shift,31] <- (forecast60$mean[z] + 1) *  minute_data[i+shift,31]
      minute_data[i+shift,32] <- (forecast60$upper[z,2] + 1) * minute_data[i+shift,32]
      minute_data[i+shift,33] <- (forecast60$lower[z,2] + 1) * minute_data[i+shift,33]
    }
    
    minute_data[i+shift,34] <- forecast60$model$bic
    minute_data[i+shift,35] <- forecast60$model$aicc
    
    # gettting real returns
    minute_data[i+shift,36] <- log(minute_data[i+shift+1,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,37] <- log(minute_data[i+shift+2,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,38] <- log(minute_data[i+shift+3,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,39] <- log(minute_data[i+shift+6,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,40] <- log(minute_data[i+shift+12,3]/minute_data[i+shift,3]) + 1
    
    # gettting squred error
    minute_data[i+shift,41] <- (minute_data[i+shift,36] - minute_data[i+shift,19])^2
    minute_data[i+shift,42] <- (minute_data[i+shift,37] - minute_data[i+shift,22])^2
    minute_data[i+shift,43] <- (minute_data[i+shift,38] - minute_data[i+shift,25])^2
    minute_data[i+shift,44] <- (minute_data[i+shift,39] - minute_data[i+shift,28])^2
    minute_data[i+shift,45] <- (minute_data[i+shift,40] - minute_data[i+shift,31])^2
    
    # gettting direction accuracy
    minute_data[i+shift,46] <- (sign(minute_data[i+shift,36]-1) == sign(minute_data[i+shift,19]-1))*1
    minute_data[i+shift,47] <- (sign(minute_data[i+shift,37]-1) == sign(minute_data[i+shift,22]-1))*1
    minute_data[i+shift,48] <- (sign(minute_data[i+shift,38]-1) == sign(minute_data[i+shift,25]-1))*1
    minute_data[i+shift,49] <- (sign(minute_data[i+shift,39]-1) == sign(minute_data[i+shift,28]-1))*1
    minute_data[i+shift,50] <- (sign(minute_data[i+shift,40]-1) == sign(minute_data[i+shift,31]-1))*1
    
    #extracting lags
    minute_data[i+shift,51] <- arimaorder(fit)[1]
    minute_data[i+shift,52] <- arimaorder(fit)[2]
    minute_data[i+shift,53] <- arimaorder(fit)[3]
    
    #testing
    #Box test
    t <- try(Box.test (fit$residuals, lag = 10)[3])
    if(inherits(t, "try-error")){
      minute_data[i+shift,54] <- NA
    } else {
      minute_data[i+shift,54] <- t
    }
   
    #Shapiro test
    t <- try(shapiro.test(fit$residuals)[2])
    if(inherits(t, "try-error")){
      minute_data[i+shift,55] <- NA
    } else {
      minute_data[i+shift,55] <- t
    }
    
    #Lillie test
    t <- try(lillie.test(fit$residuals)[2])
    if(inherits(t, "try-error")){
      minute_data[i+shift,56] <- NA
    } else {
      minute_data[i+shift,56] <- t
    }

    #t.test test
    t <- try(t.test(fit$residuals)[3])
    if(inherits(t, "try-error")){
      minute_data[i+shift,57] <- NA
    } else {
      minute_data[i+shift,57] <- t
    }

    #Box^2 test
    t <- try(Box.test(fit$residuals^2, lag = 10)[3])
    if(inherits(t, "try-error")){
      minute_data[i+shift,58] <- NA
    } else {
      minute_data[i+shift,58] <- t
    }

    minute_data[i+shift,59] <- minute_data[i+shift,51] + minute_data[i+shift,53]
    
    #setTkProgressBar(pb, i, label=paste( round(i/(nrow(minute_data)-timeframe)*1000, 0),"‰ done"))
    print(paste("XMR","_",q, "_",i))
    
  }
  
  save(minute_data,file=paste(window_start,"_", window_end,"_", lookback,"_one_minut_ARIMA_fit_XMR.Rdata", sep = ""))
  
  forecasts <- minute_data[which(minute_data$WN_RW_indication !=0), ]
  time_vector <- forecasts$time
  save(time_vector,file=paste(window_start,"_", window_end,"_", lookback,"_time_vector_one_minut_ARIMA_fit_XMR.Rdata", sep = ""))
  
  
  
  #actual returns share
  returns_benchmark <- minute_data[which(minute_data$WN_RW_indication !=0), ]
  returns_benchmark$one  <- nrow(returns_benchmark[which(returns_benchmark$R_1 > 1),])/(nrow(returns_benchmark)-sum(is.na(minute_data$A_m1)))*100
  returns_benchmark$three <- nrow(returns_benchmark[which(returns_benchmark$R_3 > 1),])/(nrow(returns_benchmark)-sum(is.na(minute_data$A_m3)))*100
  returns_benchmark$five <- nrow(returns_benchmark[which(returns_benchmark$R_5 > 1),])/(nrow(returns_benchmark)-sum(is.na(minute_data$A_m5)))*100
  returns_benchmark$ten <- nrow(returns_benchmark[which(returns_benchmark$R_10 > 1),])/(nrow(returns_benchmark)-sum(is.na(minute_data$A_m10)))*100
  returns_benchmark$fifteen  <- nrow(returns_benchmark[which(returns_benchmark$R_15 > 1),])/(nrow(returns_benchmark)-sum(is.na(minute_data$A_m15)))*100
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
  
  
  save(returns_benchmark,file=paste(window_start,"_", window_end,"_", lookback,"_minut_ARIMA_benchmark_XMR.Rdata", sep = ""))
  
  diagnostics <- as.data.frame(matrix(data = 1, nrow = 1, ncol = 9))
  colnames(diagnostics) <- c("p", "d", "q", "Box.test_p_v", "Shapiro-Wilk_p_v","Lilliefors_p_v", "Zero_mean_ttest_p_v", "Heteroscedasticity_p_v", "WN_share" )
  diagnostics$p <- mean(forecasts$p, na.rm = TRUE)
  diagnostics$d <- mean(forecasts$d, na.rm = TRUE)
  diagnostics$q <- mean(forecasts$q, na.rm = TRUE)
  diagnostics$Box.test_p_v <- mean(forecasts$Box.test_p_v, na.rm = TRUE)
  diagnostics$`Shapiro-Wilk_p_v` <- mean(forecasts$`Shapiro-Wilk_p_v`, na.rm = TRUE)
  diagnostics$Lilliefors_p_v <- mean(forecasts$Lilliefors_p_v, na.rm = TRUE)
  diagnostics$Zero_mean_ttest_p_v <- mean(forecasts$Zero_mean_ttest_p_v, na.rm = TRUE)
  diagnostics$Heteroscedasticity_p_v <- mean(forecasts$Heteroscedasticity_p_v, na.rm = TRUE)
  diagnostics$WN_share <- 1 - (nrow(forecasts)/nrow(minute_data))
  
  save(diagnostics,file=paste(window_start,"_", window_end,"_", lookback,"_minut_ARIMA_diagnostics_XMR.Rdata", sep = ""))

