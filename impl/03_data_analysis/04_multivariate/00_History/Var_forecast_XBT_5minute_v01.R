#Prepsat to na skalovatelnost trendu

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken", sep = "")
setwd(wd) 
rm(list=ls())

#install.packages("sparsevar")


library(ggplot2)
library(tcltk)
library(forecast)
library(sparsevar)
library(vars)

# *** loading data ****************************
load(file="XBT_5minute_returns_w_search_trends.Rdata")

#preparing struncture
five_minute_data[,25:56] <- 1
colnames(five_minute_data)[c(19,22,25,28,31)] <- c("A_m5","A_m10","A_m15","A_m30", "A_m60")                             #mean for x minutes forward estimate
colnames(five_minute_data)[c(20,23,26,29,32)] <- c("A_u5(95%)", "A_u10(95%)", "A_u15(95%)", "A_u30(95%)","A_u60(95%)")  #upper bound for x minutes forward estimate
colnames(five_minute_data)[c(21,24,27,30,33)] <- c("A_l5(95%)" ,"A_l10(95%)", "A_l15(95%)", "A_l30(95%)", "A_l60(95%)")  # lower bound for x minutes forward estimate
colnames(five_minute_data)[c(34,35)] <- c("A_BIC", "A_AICC")                                                            # AIC a BIC criteria for choosing proper order
colnames(five_minute_data)[c(36:40)] <- c("R_5","R_10","R_15","R_30", "R_60")                                          #real values
colnames(five_minute_data)[c(41:45)] <- c("A_SQe_5","A_SQe_10","A_SQe_15","A_SQe_30", "A_SQe_60")                      #squared errors of estimate 
colnames(five_minute_data)[c(46:50)] <- c("A_DA_5","A_DA_10","A_DA_15","A_DA_30", "A_DA_60")                           #directional accuracy 



#setting up parametr of the window
window_start <- 50000 #any number from number of lags to nrow-1
window_end <- 51999 #any number from start to nrow
lookback <- 72



# **** fitting var ******************************
sbst <- as.matrix(five_minute_data[c(window_start:window_end), c(18,21,22)])

fit <- VAR(sbst, p = 1, type = "none")
predict(fit, n.ahead = 12)

