rm(list=ls())
library(robustHD)
library(utils)

# **** preparing data frames *****************************************************
results <- as.data.frame(matrix(data = 1, nrow = 14, ncol = 6))
colnames(results) <- c("crypto", "granularity","learning_period","model","metrics", "value")

results_long <- as.data.frame(matrix(data = 1, nrow = 1, ncol = 6))
colnames(results_long) <- c("crypto", "granularity","learning_period","model","metrics", "value")


#************specifying model *****************************************************
crypto <- "XBT"       #("XBT", "ETH", "XMR", "LTC") - one of that
gran <- "60minut"        #("15minut","30minut","60minut", "day")
learn <-  168   #(48,96,288,168,336,144,672,14,21,28,42)
model <-  "VECM_bin_fit"      #("ARIMA_fit", "VECM_no_diff_fit", "VECM_bin_fit", "VECM_quart_fit")

for (crypto in c("XBT", "ETH", "XMR", "LTC")) {
  for (model in c("ARIMA_fit", "VECM_no_diff_fit", "VECM_bin_fit", "VECM_quart_fit")){
    for (gran in c("15minut","30minut","60minut", "day")){
      
      #filtering learnin periods
      if (gran == "15minut") {periods <- c(48,96,288,672)} 
      if (gran == "30minut") {periods <- c(48,144,336,672)} 
      if (gran == "60minut") {periods <- c(48,96,168,336)}
      if (gran == "day") {periods <- c(14,21,28,42)}
      
      for (learn in periods){ 
#************* setting WD *************************************************
#setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
#setwd("C:/Users/vojta/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec")
setwd("C:/Users/pulec.vojtech/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec")
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/", crypto, sep = "")
setwd(wd) 
  
# *** loading data ****************************
load(file=paste(crypto,"_",gran, "_", model, "_",learn,".Rdata",sep = ""))

if (gran == "15minut") {fit <- fifteen_minute_data} 
if (gran == "30minut") {fit <- thirty_minute_data} 
if (gran == "60minut") {fit <- hour_data}
if (gran == "day") {fit <- day_data}
        

fit_compl <- fit
fit <- fit[which(fit$A_m1 != 1),]
fit <- fit[which(!is.na(fit$A_m1)),]

results$crypto <- crypto
results$model <- model
results$granularity <- gran
results$learning_period <- learn

results[1,5] <- "MDA_1step"
results[2,5] <- "MDA_3step"
results[3,5] <- "MDA_5step"
results[4,5] <- "MDA_10step"
results[5,5] <- "MDA_15step"
results[6,5] <- "MSE_1step"
results[7,5] <- "MSE_3step"
results[8,5] <- "MSE_5step"
results[9,5] <- "MSE_10step"
results[10,5] <- "MSE_15step"
results[11,5] <- "Share"
results[12,5] <- "Profit_0.00"
results[13,5] <- "Profit_0.26"
results[14,5] <- "Profit_0.10"


results[1,6] <- round(mean(fit$A_DA_1),3)
results[2,6] <- round(mean(fit$A_DA_3, na.rm = TRUE),3)
results[3,6] <- round(mean(fit$A_DA_5, na.rm = TRUE),3)
results[4,6] <- round(mean(fit$A_DA_10, na.rm = TRUE),3)
results[5,6] <- round(mean(fit$A_DA_15, na.rm = TRUE),3)
results[6,6] <- round(mean(winsorize(na.omit(fit$A_SQe_1),probs = 0.95), na.rm = TRUE)*1000,3)
results[7,6] <- round(mean(winsorize(na.omit(fit$A_SQe_3),probs = 0.95), na.rm = TRUE)*1000,3)
results[8,6] <- round(mean(winsorize(na.omit(fit$A_SQe_5),probs = 0.95), na.rm = TRUE)*1000,3)
results[9,6] <- round(mean(winsorize(na.omit(fit$A_SQe_10),probs = 0.95), na.rm = TRUE)*1000,3)
results[10,6] <- round(mean(winsorize(na.omit(fit$A_SQe_15),probs = 0.95), na.rm = TRUE)*1000,3)
results[11,6] <- round(nrow(fit)/nrow(fit_compl),3)
results[12,6] <- round(fit$eur[max(which(fit$eur != 0))]/fit$eur[min(which(fit$eur != 0))],3)
results[13,6] <- round(fit$taker26_eur[max(which(fit$taker26_eur != 0))]/fit$taker26_eur[min(which(fit$taker26_eur != 0))],3)
results[14,6] <- round(fit$taker10_eur[max(which(fit$taker10_eur != 0))]/fit$taker10_eur[min(which(fit$taker10_eur != 0))],3)

results_long <- rbind(results_long, results)
print(paste(crypto,"_",gran, "_", model, "_",learn))
}
}
}
}
results_long <- results_long[-1,]

setwd("C:/Users/pulec.vojtech/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec/data/03_final_sample/Kraken")
write.csv(results_long, file = "results.csv",row.names=FALSE)