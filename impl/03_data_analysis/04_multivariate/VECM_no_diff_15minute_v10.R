rm(list=ls())
clist <- c("XBT", "ETH", "XMR", "LTC")
for(w in clist){
  #setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
  setwd("C:/Users/vojta/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec")
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
  library("vars")

#loop for multiple rolling window lenghts
for(q in c(48,96,288,672)){

# *** loading data ****************************
  load(file=paste(w,"_15minute_returns_quart.Rdata",sep = ""))
  
  # 1) *********** preparing structure for fit ******************************
  
  #setting up parametr of the window
  
  
  window_start <- q+1 #any number from number of lags to nrow-1
  window_end <- nrow(fifteen_minute_data) #any number from start to nrow
  lookback <- q
  
  fifteen_minute_data <- fifteen_minute_data[c((window_start-lookback):window_end),]
  

  # Progress bar
  # before loop pb <- tkProgressBar(title = "progress bar", min = 0, max = nrow(OM_uni), width = 1000)
  # into the loop setTkProgressBar(pb, i, label=paste( round(i/(nrow(OM_uni))*1000, 0),"‰ done"))
  #fitting values for missing attention splits
  
  #preparing struncture
  fifteen_minute_data[,29:68] <- 1
  fifteen_minute_data[,69] <- 0 
  fifteen_minute_data[,70:95] <- 0 
  colnames(fifteen_minute_data)[c(29,32,35,38,41)] <- c("A_m1","A_m3","A_m5","A_m10", "A_m15")                               #mean for x minutes forward estimate
  colnames(fifteen_minute_data)[c(30,33,36,39,42)] <- c("A_u1", "A_u3", "A_u5", "A_u10","A_u15")    #upper bound for x minutes forward estimate
  colnames(fifteen_minute_data)[c(31,34,37,40,43)] <- c("A_l1" ,"A_l3", "A_l5", "A_l10", "A_l15")   #lower bound for x minutes forward estimate
  colnames(fifteen_minute_data)[c(44,45)] <- c("A_BIC", "A_AICC")                                                            #AIC a BIC criteria for choosing proper order
  colnames(fifteen_minute_data)[c(46:50)] <- c("R_1","R_3","R_5","R_10", "R_15")                                             #real values
  colnames(fifteen_minute_data)[c(51:55)] <- c("A_SQe_1","A_SQe_3","A_SQe_5","A_SQe_10", "A_SQe_15")                         #squared errors of estimate 
  colnames(fifteen_minute_data)[c(56:60)] <- c("A_DA_1","A_DA_3","A_DA_5","A_DA_10", "A_DA_15")                              #directional accuracy 
  colnames(fifteen_minute_data)[c(61:63)] <- c("Lags","NA","NA")                                                                  #orders 
  colnames(fifteen_minute_data)[c(64:68)] <- c("Box.test_p_v", "Shapiro_Wilk_p_v","Lilliefors_p_v", "Zero_mean_ttest_p_v","Heteroscedasticity_p_v" ) #residuals testing
  colnames(fifteen_minute_data)[69] <- "NA"
  colnames(fifteen_minute_data)[c(70:76)] <- c("alpha", "beta", "gamma", "fi", "zeta", "theta", "eta")
  colnames(fifteen_minute_data)[c(77:83)] <- c("buy_sell", "eur", "crypto", "taker26_eur", "taker26_crypto","taker10_eur", "taker10_crypto")
  colnames(fifteen_minute_data)[c(84:89)] <- c( "beta_l2", "gamma_l2", "fi_l2", "zeta_l2", "theta_l2", "eta_l2")
  colnames(fifteen_minute_data)[c(90:95)] <- c( "beta_l3", "gamma_l3", "fi_l3", "zeta_l3", "theta_l3", "eta_l3")
  
# **** fitting var ******************************
timeframe <- lookback #reference period
shift <- timeframe - 1 # last block of time frame it starts forecasting 5/10/15/30/60 mins

fifteen_minute_data$eur[shift] <- fifteen_minute_data$price_all[shift] #initial eur of 1 XBT to trade with
fifteen_minute_data$taker26_eur[shift] <- fifteen_minute_data$price_all[shift] #initial of 1 XBT to trade with
fifteen_minute_data$taker10_eur[shift] <- fifteen_minute_data$price_all[shift] #initial of 1 XBT to trade with 


for(i in 1:(nrow(fifteen_minute_data)-timeframe)){
  sbst <- as.matrix(fifteen_minute_data[c(i:(i+shift)), c(19,21)])
  
  lags <- VARselect(sbst)
  lags <- max(2,round(median(lags$selection)))
  
  t <- try(ca.jo(sbst, type = "eigen", K = lags, spec = "transitory"))
  if(inherits(t, "try-error")){
    next()
  } else {
    cajoclass <- t
  }
  
  fit <- vec2var(cajoclass, r=1)
  #fit <- VAR(sbst, lag.max = 4, ic = "AIC", type = "none")
  forecast60 <- predict(fit, n.ahead = 15, ic = "AIC", ci = 0.95)
  forecast60 <- forecast60$fcst$logreturn_price_all 
  
  # fitting 1 minutes ahead
  fifteen_minute_data$A_m1[i+shift]  <- forecast60[1,1] + 1
  fifteen_minute_data$A_u1[i+shift] <- forecast60[1,3] + 1
  fifteen_minute_data$A_l1[i+shift]  <- forecast60[1,2] + 1
  
  
  # fitting 3 minute ahead
  for(z in 1:3){
    fifteen_minute_data$A_m3[i+shift] <- (forecast60[z,1] + 1) *  fifteen_minute_data$A_m3[i+shift]
    fifteen_minute_data$A_u3[i+shift] <- (forecast60[z,3] + 1) * fifteen_minute_data$A_u3[i+shift]
    fifteen_minute_data$A_l3[i+shift] <- (forecast60[z,2] + 1) * fifteen_minute_data$A_l3[i+shift]
  }
  
  # fitting 5 minute ahead
  for(z in 1:5){
    fifteen_minute_data$A_m5[i+shift] <- (forecast60[z,1] + 1) *  fifteen_minute_data$A_m5[i+shift]
    fifteen_minute_data$A_u5[i+shift] <- (forecast60[z,3] + 1) * fifteen_minute_data$A_u5[i+shift]
    fifteen_minute_data$A_l5[i+shift] <- (forecast60[z,2] + 1) * fifteen_minute_data$A_l5[i+shift]
  }
  
  # fitting 10 minute ahead
  for(z in 1:10){
    fifteen_minute_data$A_m10[i+shift] <- (forecast60[z,1] + 1) *  fifteen_minute_data$A_m10[i+shift]
    fifteen_minute_data$A_u10[i+shift] <- (forecast60[z,3] + 1) *  fifteen_minute_data$A_u10[i+shift]
    fifteen_minute_data$A_l10[i+shift] <- (forecast60[z,2] + 1) * fifteen_minute_data$A_l10[i+shift]
  }
  
  # fitting 15 minute ahead
  for(z in 1:15){
    fifteen_minute_data$A_m15[i+shift] <- (forecast60[z,1] + 1) *  fifteen_minute_data$A_m15[i+shift]
    fifteen_minute_data$A_u15[i+shift] <- (forecast60[z,3] + 1) * fifteen_minute_data$A_u15[i+shift]
    fifteen_minute_data$A_l15[i+shift] <- (forecast60[z,2] + 1) * fifteen_minute_data$A_l15[i+shift]
  }
  
  #getting AIC and BIC
  temp <- VARselect(sbst)
  fifteen_minute_data$A_BIC[i+shift] <- temp$criteria[3,max(lags,2)]
  fifteen_minute_data$A_AICC[i+shift] <- temp$criteria[1,max(lags,2)]
  
  # gettting real returns
  fifteen_minute_data$R_1[i+shift] <- log(fifteen_minute_data$price_all[i+shift+1]/fifteen_minute_data$price_all[i+shift]) + 1
  fifteen_minute_data$R_3[i+shift] <- log(fifteen_minute_data$price_all[i+shift+2]/fifteen_minute_data$price_all[i+shift]) + 1
  fifteen_minute_data$R_5[i+shift] <- log(fifteen_minute_data$price_all[i+shift+3]/fifteen_minute_data$price_all[i+shift]) + 1
  fifteen_minute_data$R_10[i+shift] <- log(fifteen_minute_data$price_all[i+shift+6]/fifteen_minute_data$price_all[i+shift]) + 1
  fifteen_minute_data$R_15[i+shift] <- log(fifteen_minute_data$price_all[i+shift+12]/fifteen_minute_data$price_all[i+shift]) + 1
  
  # gettting squred error
  fifteen_minute_data$A_SQe_1[i+shift] <- (fifteen_minute_data$R_1[i+shift] - fifteen_minute_data$A_m1[i+shift])^2
  fifteen_minute_data$A_SQe_3[i+shift] <- (fifteen_minute_data$R_3[i+shift] - fifteen_minute_data$A_m3[i+shift])^2
  fifteen_minute_data$A_SQe_5[i+shift] <- (fifteen_minute_data$R_5[i+shift] - fifteen_minute_data$A_m5[i+shift])^2
  fifteen_minute_data$A_SQe_10[i+shift] <- (fifteen_minute_data$R_10[i+shift] - fifteen_minute_data$A_m10[i+shift])^2
  fifteen_minute_data$A_SQe_15[i+shift] <- (fifteen_minute_data$R_15[i+shift] - fifteen_minute_data$A_m15[i+shift])^2
  
  # gettting direction accuracy
  fifteen_minute_data$A_DA_1[i+shift] <- (sign(fifteen_minute_data$R_1[i+shift]-1) == sign(fifteen_minute_data$A_m1[i+shift]-1))*1
  fifteen_minute_data$A_DA_3[i+shift] <- (sign(fifteen_minute_data$R_3[i+shift]-1) == sign(fifteen_minute_data$A_m3[i+shift]-1))*1
  fifteen_minute_data$A_DA_5[i+shift] <- (sign(fifteen_minute_data$R_5[i+shift]-1) == sign(fifteen_minute_data$A_m5[i+shift]-1))*1
  fifteen_minute_data$A_DA_10[i+shift] <- (sign(fifteen_minute_data$R_10[i+shift]-1) == sign(fifteen_minute_data$A_m10[i+shift]-1))*1
  fifteen_minute_data$A_DA_15[i+shift] <- (sign(fifteen_minute_data$R_15[i+shift]-1) == sign(fifteen_minute_data$A_m15[i+shift]-1))*1
  
  #testing
  #Box test
  t <- try(Box.test (fit$resid[,1], lag = 10)[3])
  if(inherits(t, "try-error")){
    fifteen_minute_data$Box.test_p_v[i+shift] <- NA
  } else {
    fifteen_minute_data$Box.test_p_v[i+shift] <- t
  }
  
  #Shapiro test
  t <- try(shapiro.test(fit$resid[,1])[2])
  if(inherits(t, "try-error")){
    fifteen_minute_data$Shapiro_Wilk_p_v[i+shift] <- NA
  } else {
    fifteen_minute_data$Shapiro_Wilk_p_v[i+shift] <- t
  }
  
  #Lillie test
  t <- try(lillie.test(fit$resid[,1])[2])
  if(inherits(t, "try-error")){
    fifteen_minute_data$Lilliefors_p_v[i+shift] <- NA
  } else {
    fifteen_minute_data$Lilliefors_p_v[i+shift] <- t
  }
  
  #t.test test
  t <- try(t.test(fit$resid[,1])[3])
  if(inherits(t, "try-error")){
    fifteen_minute_data$Zero_mean_ttest_p_v[i+shift] <- NA
  } else {
    fifteen_minute_data$Zero_mean_ttest_p_v[i+shift] <- t
  }
  
  #Box^2 test
  t <- try(Box.test(fit$resid[,1]^2, lag = 10)[3])
  if(inherits(t, "try-error")){
    fifteen_minute_data$Heteroscedasticity_p_v[i+shift] <- NA
  } else {
    fifteen_minute_data$Heteroscedasticity_p_v[i+shift] <- t
  }
  
  #recording lags
  fifteen_minute_data$Lags[i+shift] <- lags
  
  # buy / sell decision
  if(fifteen_minute_data$A_m1[i + shift] < 1)
  {fifteen_minute_data$`buy_sell`[i + shift] <- "S"
  } else if (fifteen_minute_data$A_m1[i + shift] > 1) {
    fifteen_minute_data$`buy_sell`[i + shift] <- "B"
  } else {
      fifteen_minute_data$`buy_sell`[i + shift] <- "H"}
  
  #calculating portoflio
  if(fifteen_minute_data$`buy_sell`[i + shift] == "S") {
    fifteen_minute_data$eur[i + shift] <- fifteen_minute_data$eur[i + shift-1] + fifteen_minute_data$crypto[i + shift-1]*fifteen_minute_data$price_all[i + shift]
    fifteen_minute_data$crypto[i + shift] <- 0 
  } else if (fifteen_minute_data$`buy_sell`[i + shift] == "B") {
     fifteen_minute_data$eur[i + shift] <- 0
     fifteen_minute_data$crypto[i + shift] <- fifteen_minute_data$eur[i + shift-1]/fifteen_minute_data$price_all[i + shift] + fifteen_minute_data$crypto[i + shift-1]
    } else {
      fifteen_minute_data$crypto[i + shift] <- fifteen_minute_data$crypto[i + shift-1]
      fifteen_minute_data$eur[i + shift] <- fifteen_minute_data$eur[i + shift-1]
  }
  
  #calculating net portfolio - taker 26
  if(fifteen_minute_data$`buy_sell`[i + shift] == "S") {
    fifteen_minute_data$taker26_eur[i + shift] <- fifteen_minute_data$taker26_eur[i + shift-1] + fifteen_minute_data$taker26_crypto[i + shift-1]*fifteen_minute_data$price_all[i + shift]*0.9974
     fifteen_minute_data$taker26_crypto[i + shift] <- 0 
     } else if (fifteen_minute_data$`buy_sell`[i + shift] == "B") {
       fifteen_minute_data$taker26_eur[i + shift] <- 0
      fifteen_minute_data$taker26_crypto[i + shift] <- fifteen_minute_data$taker26_eur[i + shift-1]/fifteen_minute_data$price_all[i + shift]*0.9974 + fifteen_minute_data$taker26_crypto[i + shift-1]
      } else {
        fifteen_minute_data$taker26_crypto[i + shift] <- fifteen_minute_data$taker26_crypto[i + shift-1]
  fifteen_minute_data$taker26_eur[i + shift] <- fifteen_minute_data$taker26_eur[i + shift-1]}
  
  #calculating net portfolio - taker 10
  if(fifteen_minute_data$`buy_sell`[i + shift] == "S") {
    fifteen_minute_data$taker10_eur[i + shift] <- fifteen_minute_data$taker10_eur[i + shift-1] + fifteen_minute_data$taker10_crypto[i + shift-1]*fifteen_minute_data$price_all[i + shift]*0.999
    fifteen_minute_data$taker10_crypto[i + shift] <- 0 
    } else if (fifteen_minute_data$`buy_sell`[i + shift] == "B") {
      fifteen_minute_data$taker10_eur[i + shift] <- 0
      fifteen_minute_data$taker10_crypto[i + shift] <- fifteen_minute_data$taker10_eur[i + shift-1]/fifteen_minute_data$price_all[i + shift]*0.999 + fifteen_minute_data$taker10_crypto[i + shift-1]
      } else {
        fifteen_minute_data$taker10_crypto[i + shift] <- fifteen_minute_data$taker10_crypto[i + shift-1]
        fifteen_minute_data$taker10_eur[i + shift] <- fifteen_minute_data$taker10_eur[i + shift-1]}
  
  
  #extracting model coeficients
  fifteen_minute_data$alpha[i + shift] <- fit$deterministic[1] #constant
  
  #autoregressive first lag
  fifteen_minute_data$beta[i + shift] <- fit$A$A1[1,1]
  
  #autoregressive secondt lag
  fifteen_minute_data$beta_l2[i + shift] <- fit$A$A2[1,1]

  
  #autoregressive third lag
  t <- try(fifteen_minute_data$beta_l3[i + shift] <- fit$A$A3[1,1])
  if(inherits(t, "try-error")){
    fifteen_minute_data$beta_l3[i + shift] <- NA
  } else {
    fifteen_minute_data$beta_l3[i + shift] <- t
  }
  
  #SVI first lag
  fifteen_minute_data$gamma[i + shift] <- fit$A$A1[1,2] 
  
  #SVI second lag
  fifteen_minute_data$gamma_l2[i + shift] <- fit$A$A2[1,2] 
  
  #SVI third lag
  t <- try(fifteen_minute_data$gamma_l3[i + shift] <- fit$A$A3[1,2])
  if(inherits(t, "try-error")){
    fifteen_minute_data$gamma_l3[i + shift] <- NA
  } else {
    fifteen_minute_data$gamma_l3[i + shift] <- t
  }
   
    
  
  #setTkProgressBar(pb, i, label=paste( round(i/(nrow(fifteen_minute_data)-timeframe)*1000, 0),"‰ done"))
  print(paste(w,"_",q, "_",i))
  
}

par(mfrow=c(2,2))
plot(fifteen_minute_data$price_all,pch=16,cex = 0.3, ylim=c(0, max(fifteen_minute_data$eur,fifteen_minute_data$price_all)),xlab='' , ylab = "", main="Price")
plot(fifteen_minute_data$eur,pch=16,cex = 0.3, ylim=c(0, max(fifteen_minute_data$eur,fifteen_minute_data$price_all)),xlab='' , ylab = "", main="0% fees")
plot(fifteen_minute_data$taker26_eur,pch=16,cex = 0.3,ylim=c(0, max(fifteen_minute_data$eur,fifteen_minute_data$price_all)),xlab='' , ylab = "", main="0.26% fees")
plot(fifteen_minute_data$taker10_eur,pch=16,cex = 0.3,ylim=c(0, max(fifteen_minute_data$eur,fifteen_minute_data$price_all)),xlab='' , ylab = "", main="0.10% fees")

save(fifteen_minute_data,file=paste(w,"_15minut_VECM_no_diff_fit_",lookback ,".Rdata",sep = ""))

}
}

#********************************************** end of fitting ***************************************************************


