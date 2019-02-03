rm(list=ls())
library(robustHD)


#************specifying model *****************************************************
crypto <- "XBT"       #("XBT", "ETH", "XMR", "LTC") - one of that
gran <- "60minut"        #("15minut","30minut","60minut", "day")
learn <-  48   #(48,96,288,168,336,144,672,14,21,28,42)
model <-  "VECM_bin_fit"      #("ARIMA_fit", "VECM_no_diff_fit", "VECM_bin_fit", "VECM_quart_fit")

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
if (gran == "day") {fit <- day_hour}
        

fit_compl <- fit
fit <- fit[which(fit$A_m1 != 1),]
fit <- fit[which(!is.na(fit$A_m1)),]

results <- matrix(data = "dumm", nrow = 14, ncol = 2)

results[1,1] <- "MDA_1step"
results[2,1] <- "MDA_3step"
results[3,1] <- "MDA_5step"
results[4,1] <- "MDA_10step"
results[5,1] <- "MDA_15step"
results[6,1] <- "MSE_1step"
results[7,1] <- "MSE_3step"
results[8,1] <- "MSE_5step"
results[9,1] <- "MSE_10step"
results[10,1] <- "MSE_15step"
results[11,1] <- "Share"
results[12,1] <- "Profit_0.00"
results[13,1] <- "Profit_0.26"
results[14,1] <- "Profit_0.10"

results[1,2] <- round(mean(fit$A_DA_1),3)
results[2,2] <- round(mean(fit$A_DA_3, na.rm = TRUE),3)
results[3,2] <- round(mean(fit$A_DA_5, na.rm = TRUE),3)
results[4,2] <- round(mean(fit$A_DA_10, na.rm = TRUE),3)
results[5,2] <- round(mean(fit$A_DA_15, na.rm = TRUE),3)
results[6,2] <- round(mean(winsorize(na.omit(fit$A_SQe_1),probs = 0.95), na.rm = TRUE)*1000,3)
results[7,2] <- round(mean(winsorize(na.omit(fit$A_SQe_3),probs = 0.95), na.rm = TRUE)*1000,3)
results[8,2] <- round(mean(winsorize(na.omit(fit$A_SQe_5),probs = 0.95), na.rm = TRUE)*1000,3)
results[9,2] <- round(mean(winsorize(na.omit(fit$A_SQe_10),probs = 0.95), na.rm = TRUE)*1000,3)
results[10,2] <- round(mean(winsorize(na.omit(fit$A_SQe_15),probs = 0.95), na.rm = TRUE)*1000,3)
results[11,2] <- round(nrow(fit)/nrow(fit_compl),3)
results[12,2] <- round(fit$eur[max(which(fit$eur != 0))]/fit$eur[min(which(fit$eur != 0))],3)
results[13,2] <- round(fit$taker26_eur[max(which(fit$taker26_eur != 0))]/fit$taker26_eur[min(which(fit$taker26_eur != 0))],3)
results[14,2] <- round(fit$taker10_eur[max(which(fit$taker10_eur != 0))]/fit$taker10_eur[min(which(fit$taker10_eur != 0))],3)

print(results)