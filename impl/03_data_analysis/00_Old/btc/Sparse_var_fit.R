setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/test_sample/bitcoin", sep = "")
setwd(wd) 
rm(list=ls())

#install.packages("sparsevar")


library(ggplot2)
library(tcltk)
library(forecast)
library(sparsevar)

# *** loading data ****************************
load(file = "five_minute_data_ret_bc.RData")

sbst <- as.matrix(five_minute_data[ , c(9,16)], nrow = 5999, ncol = 2)

# **** fitting s var ******************************

fit <- fitVAR(sbst, p = 25)

#sparsevar::plotVAR(fit)

irf <- impulseResponse(fit)

fit$A

computeForecasts(fit, 10)

irf <- impulseResponse(fit,25)
eb <- errorBandsIRF(fit, irf)

plotIRF(irf, eb, j = 1 , i = 1)

plotIRFGrid(irf, eb, c(1,2))