setwd("C:/Users/servis/Documents/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
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
  load(file="LTC_1minute_returns.Rdata")
  
  # 1) *********** preparing structure for fit ******************************
  
  #setting up parametr of the window
  
  
  window_start <- q+1 #any number from number of lags to nrow-1
  window_end <- nrow(minute_data) #any number from start to nrow
  lookback <- q
  
  minute_data <- minute_data[c((window_start-lookback):window_end),]
  minute_data[c(is.infinite(minute_data[,20])),20] <- 0 
  minute_data[c(is.na(minute_data[,20])),20] <- 0 
  minute_data[c(is.nan(minute_data[,20])),20] <- 0 
  
  # Progress bar
  # before loop pb <- tkProgressBar(title = "progress bar", min = 0, max = nrow(OM_uni), width = 1000)
  # into the loop setTkProgressBar(pb, i, label=paste( round(i/(nrow(OM_uni))*1000, 0),"‰ done"))
  #fitting values for missing attention splits
  minute_data[,c(21:23)] <- NA
  colnames(minute_data)[c(21:23)] <- c("Positive","Negative","Mixed")  
  
  #preparing struncture - 23
  minute_data[,c(24:60)] <- 1
  colnames(minute_data)[c(24,27,30,33,36)] <- c("A_m1","A_m3","A_m5","A_m10", "A_m15")                             #mean for x minutes forward estimate
  colnames(minute_data)[c(25,28,31,34,37)] <- c("A_u1(95%)", "A_u3(95%)", "A_u5(95%)", "A_u10(95%)","A_u15(95%)")  #upper bound for x minutes forward estimate
  colnames(minute_data)[c(26,29,32,35,38)] <- c("A_l1(95%)" ,"A_l3(95%)", "A_l5(95%)", "A_l10(95%)", "A_l15(95%)")  # lower bound for x minutes forward estimate
  colnames(minute_data)[c(39,40)] <- c("Ordr", "A_AIC")                                                            # AIC a BIC criteria for choosing proper order
  colnames(minute_data)[c(41:45)] <- c("R_1","R_3","R_5","R_10", "R_15")                                          #real values
  colnames(minute_data)[c(46:50)] <- c("A_SQe_1","A_SQe_3","A_SQe_5","A_SQe_10", "A_SQe_15")                      #squared errors of estimate 
  colnames(minute_data)[c(51:55)] <- c("A_DA_1","A_DA_3","A_DA_5","A_DA_10", "A_DA_15")                           #directional accuracy
  colnames(minute_data)[c(56:60)] <- c("Zero_mean","Hetero","Serial","Normality", "Arch_testing")
  
  
  # **** fitting var ******************************
  timeframe <- lookback #reference period
  shift <- timeframe - 1 # last block of time frame it starts forecasting 5/10/15/30/60 mins
  
  #pb <- tkProgressBar(title = "progress bar", min = 0, max = (nrow(minute_data)- timeframe), width = 1000)
  
  for(i in 1:(nrow(minute_data)-timeframe)){
    sbst <- as.matrix(minute_data[c(i:(i+shift)), c(18,20)])
    
    fit <- VAR(sbst, lag.max = 4, ic = "AIC", type = "none")
    forecast60 <- predict(fit, n.ahead = 15, ic = "AIC", ci = 0.95)
    forecast60 <- forecast60$fcst$logreturn_price_all 
    
    # fitting 1 minutes ahead
    minute_data[i+shift,24] <- forecast60[1,1] + 1
    minute_data[i+shift,25] <- forecast60[1,3] + 1
    minute_data[i+shift,26] <- forecast60[1,2] + 1
    
    
    # fitting 3 minute ahead
    for(z in 1:3){
      minute_data[i+shift,27] <- (forecast60[z,1] + 1) *  minute_data[i+shift,27]
      minute_data[i+shift,28] <- (forecast60[z,3] + 1) * minute_data[i+shift,28]
      minute_data[i+shift,29] <- (forecast60[z,2] + 1) * minute_data[i+shift,29]
    }
    
    # fitting 5 minute ahead
    for(z in 1:5){
      minute_data[i+shift,30] <- (forecast60[z,1] + 1) *  minute_data[i+shift,30]
      minute_data[i+shift,31] <- (forecast60[z,3] + 1) * minute_data[i+shift,31]
      minute_data[i+shift,32] <- (forecast60[z,2] + 1) * minute_data[i+shift,32]
    }
    
    # fitting 10 minute ahead
    for(z in 1:10){
      minute_data[i+shift,33] <- (forecast60[z,1] + 1) *  minute_data[i+shift,33]
      minute_data[i+shift,34] <- (forecast60[z,3] + 1) * minute_data[i+shift,34]
      minute_data[i+shift,35] <- (forecast60[z,2] + 1) * minute_data[i+shift,35]
    }
    
    # fitting 15 minute ahead
    for(z in 1:15){
      minute_data[i+shift,36] <- (forecast60[z,1] + 1) *  minute_data[i+shift,36]
      minute_data[i+shift,37] <- (forecast60[z,3] + 1) * minute_data[i+shift,37]
      minute_data[i+shift,38] <- (forecast60[z,2] + 1) * minute_data[i+shift,38]
    }
    
    #getting AIC and BIC
    temp <- VARselect(sbst, lag.max = 4)
    minute_data[i+shift,39] <- sum((temp$criteria[1,] == min(temp$criteria[1,]))* c(1:4))
    minute_data[i+shift,40] <- min(temp$criteria[1,])
    
    # gettting real returns
    minute_data[i+shift,41] <- log(minute_data[i+shift+1,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,42] <- log(minute_data[i+shift+3,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,43] <- log(minute_data[i+shift+5,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,44] <- log(minute_data[i+shift+10,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,45] <- log(minute_data[i+shift+15,3]/minute_data[i+shift,3]) + 1
    
    # gettting squred error
    minute_data[i+shift,46] <- (minute_data[i+shift,41] - minute_data[i+shift,24])^2
    minute_data[i+shift,47] <- (minute_data[i+shift,42] - minute_data[i+shift,27])^2
    minute_data[i+shift,48] <- (minute_data[i+shift,43] - minute_data[i+shift,30])^2
    minute_data[i+shift,49] <- (minute_data[i+shift,44] - minute_data[i+shift,33])^2
    minute_data[i+shift,50] <- (minute_data[i+shift,45] - minute_data[i+shift,36])^2
    
    # gettting direction accuracy
    minute_data[i+shift,51] <- (sign(minute_data[i+shift,41]-1) == sign(minute_data[i+shift,24]-1))*1
    minute_data[i+shift,52] <- (sign(minute_data[i+shift,42]-1) == sign(minute_data[i+shift,27]-1))*1
    minute_data[i+shift,53] <- (sign(minute_data[i+shift,43]-1) == sign(minute_data[i+shift,30]-1))*1
    minute_data[i+shift,54] <- (sign(minute_data[i+shift,44]-1) == sign(minute_data[i+shift,33]-1))*1
    minute_data[i+shift,55] <- (sign(minute_data[i+shift,45]-1) == sign(minute_data[i+shift,36]-1))*1
    
    #testing
    #t.test test
    t <- try(t.test(fit$varresult$logreturn_price_all$residuals))
    if(inherits(t, "try-error")){
      minute_data[i+shift,56] <- NA
    } else {
      minute_data[i+shift,56] <- t$p.value
    }
    
    #Hetero
    t <- try(Box.test((fit$varresult$logreturn_price_all$residuals)^2, lag = 10))
    if(inherits(t, "try-error")){
      minute_data[i+shift,57] <- NA
    } else {
      minute_data[i+shift,57] <- t[3]
    }
    
    #Serial test
    t <- try(serial.test(fit))
    if(inherits(t, "try-error")){
      minute_data[i+shift,58] <- NA
    } else {
      minute_data[i+shift,58] <- t$serial[3]
    }
    
    #Normality
    t <- try(normality.test(fit))
    if(inherits(t, "try-error")){
      minute_data[i+shift,59] <- NA
    } else {
      minute_data[i+shift,59] <- t$jb.mul$JB[3]
    }
    
    #Arch test
    t <- try(arch.test(fit))
    if(inherits(t, "try-error")){
      minute_data[i+shift,60] <- NA
    } else {
      minute_data[i+shift,60] <- t$arch.mul$p.value
    }
    
    
    
    #setTkProgressBar(pb, i, label=paste( round(i/(nrow(minute_data)-timeframe)*1000, 0),"‰ done"))
    
    print(paste("LTC","_", "No_Diff","_",q, "_",i))
    
  }
  
  save(minute_data,file=paste(window_start,"_", window_end,"_", lookback,"_1minut_VAR_no_diff_fit_LTC.Rdata", sep = ""))
}


#********************************************** end of fitting ***************************************************************

setwd("C:/Users/servis/Documents/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
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
  load(file="ETH_1minute_returns.Rdata")
  
  # 1) *********** preparing structure for fit ******************************
  
  #setting up parametr of the window
  
  
  window_start <- q+1 #any number from number of lags to nrow-1
  window_end <- nrow(minute_data) #any number from start to nrow
  lookback <- q
  
  minute_data <- minute_data[c((window_start-lookback):window_end),]
  
  minute_data[c(is.infinite(minute_data[,20])),20] <- 0 
  minute_data[c(is.na(minute_data[,20])),20] <- 0 
  minute_data[c(is.nan(minute_data[,20])),20] <- 0 
  
  
  # Progress bar
  # before loop pb <- tkProgressBar(title = "progress bar", min = 0, max = nrow(OM_uni), width = 1000)
  # into the loop setTkProgressBar(pb, i, label=paste( round(i/(nrow(OM_uni))*1000, 0),"‰ done"))
  #fitting values for missing attention splits
  minute_data[,c(21:23)] <- NA
  colnames(minute_data)[c(21:23)] <- c("Positive","Negative","Mixed")  
  
  #preparing struncture - 23
  minute_data[,c(24:60)] <- 1
  colnames(minute_data)[c(24,27,30,33,36)] <- c("A_m1","A_m3","A_m5","A_m10", "A_m15")                             #mean for x minutes forward estimate
  colnames(minute_data)[c(25,28,31,34,37)] <- c("A_u1(95%)", "A_u3(95%)", "A_u5(95%)", "A_u10(95%)","A_u15(95%)")  #upper bound for x minutes forward estimate
  colnames(minute_data)[c(26,29,32,35,38)] <- c("A_l1(95%)" ,"A_l3(95%)", "A_l5(95%)", "A_l10(95%)", "A_l15(95%)")  # lower bound for x minutes forward estimate
  colnames(minute_data)[c(39,40)] <- c("Ordr", "A_AIC")                                                            # AIC a BIC criteria for choosing proper order
  colnames(minute_data)[c(41:45)] <- c("R_1","R_3","R_5","R_10", "R_15")                                          #real values
  colnames(minute_data)[c(46:50)] <- c("A_SQe_1","A_SQe_3","A_SQe_5","A_SQe_10", "A_SQe_15")                      #squared errors of estimate 
  colnames(minute_data)[c(51:55)] <- c("A_DA_1","A_DA_3","A_DA_5","A_DA_10", "A_DA_15")                           #directional accuracy
  colnames(minute_data)[c(56:60)] <- c("Zero_mean","Hetero","Serial","Normality", "Arch_testing")
  
  
  # **** fitting var ******************************
  timeframe <- lookback #reference period
  shift <- timeframe - 1 # last block of time frame it starts forecasting 5/10/15/30/60 mins
  
  #pb <- tkProgressBar(title = "progress bar", min = 0, max = (nrow(minute_data)- timeframe), width = 1000)
  
  for(i in 1:(nrow(minute_data)-timeframe)){
    sbst <- as.matrix(minute_data[c(i:(i+shift)), c(18,20)])
    
    fit <- VAR(sbst, lag.max = 4, ic = "AIC", type = "none")
    forecast60 <- predict(fit, n.ahead = 15, ic = "AIC", ci = 0.95)
    forecast60 <- forecast60$fcst$logreturn_price_all 
    
    # fitting 1 minutes ahead
    minute_data[i+shift,24] <- forecast60[1,1] + 1
    minute_data[i+shift,25] <- forecast60[1,3] + 1
    minute_data[i+shift,26] <- forecast60[1,2] + 1
    
    
    # fitting 3 minute ahead
    for(z in 1:3){
      minute_data[i+shift,27] <- (forecast60[z,1] + 1) *  minute_data[i+shift,27]
      minute_data[i+shift,28] <- (forecast60[z,3] + 1) * minute_data[i+shift,28]
      minute_data[i+shift,29] <- (forecast60[z,2] + 1) * minute_data[i+shift,29]
    }
    
    # fitting 5 minute ahead
    for(z in 1:5){
      minute_data[i+shift,30] <- (forecast60[z,1] + 1) *  minute_data[i+shift,30]
      minute_data[i+shift,31] <- (forecast60[z,3] + 1) * minute_data[i+shift,31]
      minute_data[i+shift,32] <- (forecast60[z,2] + 1) * minute_data[i+shift,32]
    }
    
    # fitting 10 minute ahead
    for(z in 1:10){
      minute_data[i+shift,33] <- (forecast60[z,1] + 1) *  minute_data[i+shift,33]
      minute_data[i+shift,34] <- (forecast60[z,3] + 1) * minute_data[i+shift,34]
      minute_data[i+shift,35] <- (forecast60[z,2] + 1) * minute_data[i+shift,35]
    }
    
    # fitting 15 minute ahead
    for(z in 1:15){
      minute_data[i+shift,36] <- (forecast60[z,1] + 1) *  minute_data[i+shift,36]
      minute_data[i+shift,37] <- (forecast60[z,3] + 1) * minute_data[i+shift,37]
      minute_data[i+shift,38] <- (forecast60[z,2] + 1) * minute_data[i+shift,38]
    }
    
    #getting AIC and BIC
    temp <- VARselect(sbst, lag.max = 4)
    minute_data[i+shift,39] <- sum((temp$criteria[1,] == min(temp$criteria[1,]))* c(1:4))
    minute_data[i+shift,40] <- min(temp$criteria[1,])
    
    # gettting real returns
    minute_data[i+shift,41] <- log(minute_data[i+shift+1,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,42] <- log(minute_data[i+shift+3,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,43] <- log(minute_data[i+shift+5,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,44] <- log(minute_data[i+shift+10,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,45] <- log(minute_data[i+shift+15,3]/minute_data[i+shift,3]) + 1
    
    # gettting squred error
    minute_data[i+shift,46] <- (minute_data[i+shift,41] - minute_data[i+shift,24])^2
    minute_data[i+shift,47] <- (minute_data[i+shift,42] - minute_data[i+shift,27])^2
    minute_data[i+shift,48] <- (minute_data[i+shift,43] - minute_data[i+shift,30])^2
    minute_data[i+shift,49] <- (minute_data[i+shift,44] - minute_data[i+shift,33])^2
    minute_data[i+shift,50] <- (minute_data[i+shift,45] - minute_data[i+shift,36])^2
    
    # gettting direction accuracy
    minute_data[i+shift,51] <- (sign(minute_data[i+shift,41]-1) == sign(minute_data[i+shift,24]-1))*1
    minute_data[i+shift,52] <- (sign(minute_data[i+shift,42]-1) == sign(minute_data[i+shift,27]-1))*1
    minute_data[i+shift,53] <- (sign(minute_data[i+shift,43]-1) == sign(minute_data[i+shift,30]-1))*1
    minute_data[i+shift,54] <- (sign(minute_data[i+shift,44]-1) == sign(minute_data[i+shift,33]-1))*1
    minute_data[i+shift,55] <- (sign(minute_data[i+shift,45]-1) == sign(minute_data[i+shift,36]-1))*1
    
    #testing
    #t.test test
    t <- try(t.test(fit$varresult$logreturn_price_all$residuals))
    if(inherits(t, "try-error")){
      minute_data[i+shift,56] <- NA
    } else {
      minute_data[i+shift,56] <- t$p.value
    }
    
    #Hetero
    t <- try(Box.test((fit$varresult$logreturn_price_all$residuals)^2, lag = 10))
    if(inherits(t, "try-error")){
      minute_data[i+shift,57] <- NA
    } else {
      minute_data[i+shift,57] <- t[3]
    }
    
    #Serial test
    t <- try(serial.test(fit))
    if(inherits(t, "try-error")){
      minute_data[i+shift,58] <- NA
    } else {
      minute_data[i+shift,58] <- t$serial[3]
    }
    
    #Normality
    t <- try(normality.test(fit))
    if(inherits(t, "try-error")){
      minute_data[i+shift,59] <- NA
    } else {
      minute_data[i+shift,59] <- t$jb.mul$JB[3]
    }
    
    #Arch test
    t <- try(arch.test(fit))
    if(inherits(t, "try-error")){
      minute_data[i+shift,60] <- NA
    } else {
      minute_data[i+shift,60] <- t$arch.mul$p.value
    }
    
    
    
    #setTkProgressBar(pb, i, label=paste( round(i/(nrow(minute_data)-timeframe)*1000, 0),"‰ done"))
    
    print(paste("ETH","_", "No_Diff","_",q, "_",i))
    
  }
  
  save(minute_data,file=paste(window_start,"_", window_end,"_", lookback,"_1minut_VAR_no_diff_fit_ETH.Rdata", sep = ""))
}

setwd("C:/Users/servis/Documents/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
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
  
  # 1) *********** preparing structure for fit ******************************
  
  #setting up parametr of the window
  
  
  window_start <- q+1 #any number from number of lags to nrow-1
  window_end <- nrow(minute_data) #any number from start to nrow
  lookback <- q
  
  minute_data <- minute_data[c((window_start-lookback):window_end),]
  
  
  minute_data[c(is.infinite(minute_data[,20])),20] <- 0 
  minute_data[c(is.na(minute_data[,20])),20] <- 0 
  minute_data[c(is.nan(minute_data[,20])),20] <- 0 
  
  
  # Progress bar
  # before loop pb <- tkProgressBar(title = "progress bar", min = 0, max = nrow(OM_uni), width = 1000)
  # into the loop setTkProgressBar(pb, i, label=paste( round(i/(nrow(OM_uni))*1000, 0),"‰ done"))
  #fitting values for missing attention splits
  minute_data[,c(21:23)] <- NA
  colnames(minute_data)[c(21:23)] <- c("Positive","Negative","Mixed")  
  
  #preparing struncture - 23
  minute_data[,c(24:60)] <- 1
  colnames(minute_data)[c(24,27,30,33,36)] <- c("A_m1","A_m3","A_m5","A_m10", "A_m15")                             #mean for x minutes forward estimate
  colnames(minute_data)[c(25,28,31,34,37)] <- c("A_u1(95%)", "A_u3(95%)", "A_u5(95%)", "A_u10(95%)","A_u15(95%)")  #upper bound for x minutes forward estimate
  colnames(minute_data)[c(26,29,32,35,38)] <- c("A_l1(95%)" ,"A_l3(95%)", "A_l5(95%)", "A_l10(95%)", "A_l15(95%)")  # lower bound for x minutes forward estimate
  colnames(minute_data)[c(39,40)] <- c("Ordr", "A_AIC")                                                            # AIC a BIC criteria for choosing proper order
  colnames(minute_data)[c(41:45)] <- c("R_1","R_3","R_5","R_10", "R_15")                                          #real values
  colnames(minute_data)[c(46:50)] <- c("A_SQe_1","A_SQe_3","A_SQe_5","A_SQe_10", "A_SQe_15")                      #squared errors of estimate 
  colnames(minute_data)[c(51:55)] <- c("A_DA_1","A_DA_3","A_DA_5","A_DA_10", "A_DA_15")                           #directional accuracy
  colnames(minute_data)[c(56:60)] <- c("Zero_mean","Hetero","Serial","Normality", "Arch_testing")
  
  
  # **** fitting var ******************************
  timeframe <- lookback #reference period
  shift <- timeframe - 1 # last block of time frame it starts forecasting 5/10/15/30/60 mins
  
  #pb <- tkProgressBar(title = "progress bar", min = 0, max = (nrow(minute_data)- timeframe), width = 1000)
  
  for(i in 1:(nrow(minute_data)-timeframe)){
    sbst <- as.matrix(minute_data[c(i:(i+shift)), c(18,20)])
    
    fit <- VAR(sbst, lag.max = 4, ic = "AIC", type = "none")
    forecast60 <- predict(fit, n.ahead = 15, ic = "AIC", ci = 0.95)
    forecast60 <- forecast60$fcst$logreturn_price_all 
    
    # fitting 1 minutes ahead
    minute_data[i+shift,24] <- forecast60[1,1] + 1
    minute_data[i+shift,25] <- forecast60[1,3] + 1
    minute_data[i+shift,26] <- forecast60[1,2] + 1
    
    
    # fitting 3 minute ahead
    for(z in 1:3){
      minute_data[i+shift,27] <- (forecast60[z,1] + 1) *  minute_data[i+shift,27]
      minute_data[i+shift,28] <- (forecast60[z,3] + 1) * minute_data[i+shift,28]
      minute_data[i+shift,29] <- (forecast60[z,2] + 1) * minute_data[i+shift,29]
    }
    
    # fitting 5 minute ahead
    for(z in 1:5){
      minute_data[i+shift,30] <- (forecast60[z,1] + 1) *  minute_data[i+shift,30]
      minute_data[i+shift,31] <- (forecast60[z,3] + 1) * minute_data[i+shift,31]
      minute_data[i+shift,32] <- (forecast60[z,2] + 1) * minute_data[i+shift,32]
    }
    
    # fitting 10 minute ahead
    for(z in 1:10){
      minute_data[i+shift,33] <- (forecast60[z,1] + 1) *  minute_data[i+shift,33]
      minute_data[i+shift,34] <- (forecast60[z,3] + 1) * minute_data[i+shift,34]
      minute_data[i+shift,35] <- (forecast60[z,2] + 1) * minute_data[i+shift,35]
    }
    
    # fitting 15 minute ahead
    for(z in 1:15){
      minute_data[i+shift,36] <- (forecast60[z,1] + 1) *  minute_data[i+shift,36]
      minute_data[i+shift,37] <- (forecast60[z,3] + 1) * minute_data[i+shift,37]
      minute_data[i+shift,38] <- (forecast60[z,2] + 1) * minute_data[i+shift,38]
    }
    
    #getting AIC and BIC
    temp <- VARselect(sbst, lag.max = 4)
    minute_data[i+shift,39] <- sum((temp$criteria[1,] == min(temp$criteria[1,]))* c(1:4))
    minute_data[i+shift,40] <- min(temp$criteria[1,])
    
    # gettting real returns
    minute_data[i+shift,41] <- log(minute_data[i+shift+1,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,42] <- log(minute_data[i+shift+3,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,43] <- log(minute_data[i+shift+5,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,44] <- log(minute_data[i+shift+10,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,45] <- log(minute_data[i+shift+15,3]/minute_data[i+shift,3]) + 1
    
    # gettting squred error
    minute_data[i+shift,46] <- (minute_data[i+shift,41] - minute_data[i+shift,24])^2
    minute_data[i+shift,47] <- (minute_data[i+shift,42] - minute_data[i+shift,27])^2
    minute_data[i+shift,48] <- (minute_data[i+shift,43] - minute_data[i+shift,30])^2
    minute_data[i+shift,49] <- (minute_data[i+shift,44] - minute_data[i+shift,33])^2
    minute_data[i+shift,50] <- (minute_data[i+shift,45] - minute_data[i+shift,36])^2
    
    # gettting direction accuracy
    minute_data[i+shift,51] <- (sign(minute_data[i+shift,41]-1) == sign(minute_data[i+shift,24]-1))*1
    minute_data[i+shift,52] <- (sign(minute_data[i+shift,42]-1) == sign(minute_data[i+shift,27]-1))*1
    minute_data[i+shift,53] <- (sign(minute_data[i+shift,43]-1) == sign(minute_data[i+shift,30]-1))*1
    minute_data[i+shift,54] <- (sign(minute_data[i+shift,44]-1) == sign(minute_data[i+shift,33]-1))*1
    minute_data[i+shift,55] <- (sign(minute_data[i+shift,45]-1) == sign(minute_data[i+shift,36]-1))*1
    
    #testing
    #t.test test
    t <- try(t.test(fit$varresult$logreturn_price_all$residuals))
    if(inherits(t, "try-error")){
      minute_data[i+shift,56] <- NA
    } else {
      minute_data[i+shift,56] <- t$p.value
    }
    
    #Hetero
    t <- try(Box.test((fit$varresult$logreturn_price_all$residuals)^2, lag = 10))
    if(inherits(t, "try-error")){
      minute_data[i+shift,57] <- NA
    } else {
      minute_data[i+shift,57] <- t[3]
    }
    
    #Serial test
    t <- try(serial.test(fit))
    if(inherits(t, "try-error")){
      minute_data[i+shift,58] <- NA
    } else {
      minute_data[i+shift,58] <- t$serial[3]
    }
    
    #Normality
    t <- try(normality.test(fit))
    if(inherits(t, "try-error")){
      minute_data[i+shift,59] <- NA
    } else {
      minute_data[i+shift,59] <- t$jb.mul$JB[3]
    }
    
    #Arch test
    t <- try(arch.test(fit))
    if(inherits(t, "try-error")){
      minute_data[i+shift,60] <- NA
    } else {
      minute_data[i+shift,60] <- t$arch.mul$p.value
    }
    
    
    
    #setTkProgressBar(pb, i, label=paste( round(i/(nrow(minute_data)-timeframe)*1000, 0),"‰ done"))
    
    print(paste("XMR","_", "No_Diff","_",q, "_",i))
    
  }
  
  save(minute_data,file=paste(window_start,"_", window_end,"_", lookback,"_1minut_VAR_no_diff_fit_XMR.Rdata", sep = ""))
}


#********************************************** end of fitting ***************************************************************

setwd("C:/Users/servis/Documents/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
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
  load(file="XBT_1minute_returns.Rdata")
  
  # 1) *********** preparing structure for fit ******************************
  
  #setting up parametr of the window
  
  
  window_start <- q+1 #any number from number of lags to nrow-1
  window_end <- nrow(minute_data) #any number from start to nrow
  lookback <- q
  
  minute_data <- minute_data[c((window_start-lookback):window_end),]
  
  
  minute_data[c(is.infinite(minute_data[,20])),20] <- 0 
  minute_data[c(is.na(minute_data[,20])),20] <- 0 
  minute_data[c(is.nan(minute_data[,20])),20] <- 0 
  
  
  # Progress bar
  # before loop pb <- tkProgressBar(title = "progress bar", min = 0, max = nrow(OM_uni), width = 1000)
  # into the loop setTkProgressBar(pb, i, label=paste( round(i/(nrow(OM_uni))*1000, 0),"‰ done"))
  #fitting values for missing attention splits
  minute_data[,c(21:23)] <- NA
  colnames(minute_data)[c(21:23)] <- c("Positive","Negative","Mixed")  
  
  #preparing struncture - 23
  minute_data[,c(24:60)] <- 1
  colnames(minute_data)[c(24,27,30,33,36)] <- c("A_m1","A_m3","A_m5","A_m10", "A_m15")                             #mean for x minutes forward estimate
  colnames(minute_data)[c(25,28,31,34,37)] <- c("A_u1(95%)", "A_u3(95%)", "A_u5(95%)", "A_u10(95%)","A_u15(95%)")  #upper bound for x minutes forward estimate
  colnames(minute_data)[c(26,29,32,35,38)] <- c("A_l1(95%)" ,"A_l3(95%)", "A_l5(95%)", "A_l10(95%)", "A_l15(95%)")  # lower bound for x minutes forward estimate
  colnames(minute_data)[c(39,40)] <- c("Ordr", "A_AIC")                                                            # AIC a BIC criteria for choosing proper order
  colnames(minute_data)[c(41:45)] <- c("R_1","R_3","R_5","R_10", "R_15")                                          #real values
  colnames(minute_data)[c(46:50)] <- c("A_SQe_1","A_SQe_3","A_SQe_5","A_SQe_10", "A_SQe_15")                      #squared errors of estimate 
  colnames(minute_data)[c(51:55)] <- c("A_DA_1","A_DA_3","A_DA_5","A_DA_10", "A_DA_15")                           #directional accuracy
  colnames(minute_data)[c(56:60)] <- c("Zero_mean","Hetero","Serial","Normality", "Arch_testing")
  
  
  # **** fitting var ******************************
  timeframe <- lookback #reference period
  shift <- timeframe - 1 # last block of time frame it starts forecasting 5/10/15/30/60 mins
  
  #pb <- tkProgressBar(title = "progress bar", min = 0, max = (nrow(minute_data)- timeframe), width = 1000)
  
  for(i in 1:(nrow(minute_data)-timeframe)){
    sbst <- as.matrix(minute_data[c(i:(i+shift)), c(18,20)])
    
    fit <- VAR(sbst, lag.max = 4, ic = "AIC", type = "none")
    forecast60 <- predict(fit, n.ahead = 15, ic = "AIC", ci = 0.95)
    forecast60 <- forecast60$fcst$logreturn_price_all 
    
    # fitting 1 minutes ahead
    minute_data[i+shift,24] <- forecast60[1,1] + 1
    minute_data[i+shift,25] <- forecast60[1,3] + 1
    minute_data[i+shift,26] <- forecast60[1,2] + 1
    
    
    # fitting 3 minute ahead
    for(z in 1:3){
      minute_data[i+shift,27] <- (forecast60[z,1] + 1) *  minute_data[i+shift,27]
      minute_data[i+shift,28] <- (forecast60[z,3] + 1) * minute_data[i+shift,28]
      minute_data[i+shift,29] <- (forecast60[z,2] + 1) * minute_data[i+shift,29]
    }
    
    # fitting 5 minute ahead
    for(z in 1:5){
      minute_data[i+shift,30] <- (forecast60[z,1] + 1) *  minute_data[i+shift,30]
      minute_data[i+shift,31] <- (forecast60[z,3] + 1) * minute_data[i+shift,31]
      minute_data[i+shift,32] <- (forecast60[z,2] + 1) * minute_data[i+shift,32]
    }
    
    # fitting 10 minute ahead
    for(z in 1:10){
      minute_data[i+shift,33] <- (forecast60[z,1] + 1) *  minute_data[i+shift,33]
      minute_data[i+shift,34] <- (forecast60[z,3] + 1) * minute_data[i+shift,34]
      minute_data[i+shift,35] <- (forecast60[z,2] + 1) * minute_data[i+shift,35]
    }
    
    # fitting 15 minute ahead
    for(z in 1:15){
      minute_data[i+shift,36] <- (forecast60[z,1] + 1) *  minute_data[i+shift,36]
      minute_data[i+shift,37] <- (forecast60[z,3] + 1) * minute_data[i+shift,37]
      minute_data[i+shift,38] <- (forecast60[z,2] + 1) * minute_data[i+shift,38]
    }
    
    #getting AIC and BIC
    temp <- VARselect(sbst, lag.max = 4)
    minute_data[i+shift,39] <- sum((temp$criteria[1,] == min(temp$criteria[1,]))* c(1:4))
    minute_data[i+shift,40] <- min(temp$criteria[1,])
    
    # gettting real returns
    minute_data[i+shift,41] <- log(minute_data[i+shift+1,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,42] <- log(minute_data[i+shift+3,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,43] <- log(minute_data[i+shift+5,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,44] <- log(minute_data[i+shift+10,3]/minute_data[i+shift,3]) + 1
    minute_data[i+shift,45] <- log(minute_data[i+shift+15,3]/minute_data[i+shift,3]) + 1
    
    # gettting squred error
    minute_data[i+shift,46] <- (minute_data[i+shift,41] - minute_data[i+shift,24])^2
    minute_data[i+shift,47] <- (minute_data[i+shift,42] - minute_data[i+shift,27])^2
    minute_data[i+shift,48] <- (minute_data[i+shift,43] - minute_data[i+shift,30])^2
    minute_data[i+shift,49] <- (minute_data[i+shift,44] - minute_data[i+shift,33])^2
    minute_data[i+shift,50] <- (minute_data[i+shift,45] - minute_data[i+shift,36])^2
    
    # gettting direction accuracy
    minute_data[i+shift,51] <- (sign(minute_data[i+shift,41]-1) == sign(minute_data[i+shift,24]-1))*1
    minute_data[i+shift,52] <- (sign(minute_data[i+shift,42]-1) == sign(minute_data[i+shift,27]-1))*1
    minute_data[i+shift,53] <- (sign(minute_data[i+shift,43]-1) == sign(minute_data[i+shift,30]-1))*1
    minute_data[i+shift,54] <- (sign(minute_data[i+shift,44]-1) == sign(minute_data[i+shift,33]-1))*1
    minute_data[i+shift,55] <- (sign(minute_data[i+shift,45]-1) == sign(minute_data[i+shift,36]-1))*1
    
    #testing
    #t.test test
    t <- try(t.test(fit$varresult$logreturn_price_all$residuals))
    if(inherits(t, "try-error")){
      minute_data[i+shift,56] <- NA
    } else {
      minute_data[i+shift,56] <- t$p.value
    }
    
    #Hetero
    t <- try(Box.test((fit$varresult$logreturn_price_all$residuals)^2, lag = 10))
    if(inherits(t, "try-error")){
      minute_data[i+shift,57] <- NA
    } else {
      minute_data[i+shift,57] <- t[3]
    }
    
    #Serial test
    t <- try(serial.test(fit))
    if(inherits(t, "try-error")){
      minute_data[i+shift,58] <- NA
    } else {
      minute_data[i+shift,58] <- t$serial[3]
    }
    
    #Normality
    t <- try(normality.test(fit))
    if(inherits(t, "try-error")){
      minute_data[i+shift,59] <- NA
    } else {
      minute_data[i+shift,59] <- t$jb.mul$JB[3]
    }
    
    #Arch test
    t <- try(arch.test(fit))
    if(inherits(t, "try-error")){
      minute_data[i+shift,60] <- NA
    } else {
      minute_data[i+shift,60] <- t$arch.mul$p.value
    }
    
    
    
    #setTkProgressBar(pb, i, label=paste( round(i/(nrow(minute_data)-timeframe)*1000, 0),"‰ done"))
    
    print(paste("XBT","_", "No_Diff","_",q, "_",i))
    
  }
  
  save(minute_data,file=paste(window_start,"_", window_end,"_", lookback,"_1minut_VAR_no_diff_fit_XBT.Rdata", sep = ""))
}


#********************************************** end of fitting ***************************************************************
