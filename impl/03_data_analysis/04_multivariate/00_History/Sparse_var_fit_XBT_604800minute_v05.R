#Prepsat to na skalovatelnost trendu

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken", sep = "")
setwd(wd) 
rm(list=ls())

#install.packages("vars")


library(ggplot2)
library(tcltk)
library(forecast)
library(sparsevar)
library(vars)
library(splines)

# *** loading data ****************************
load(file="XBT_week_returns.Rdata")

# *** adding hourly / daily trend ***********************************************
#generating the data structure
previous_week <- as.data.frame(matrix(1, nrow = nrow(week_data), ncol = 2))
colnames(previous_week) <- c("time", "ret")

#getting hourly trend
for(i in 3:nrow(previous_week)){
  previous_week$time[i] <- week_data$time[i]
  previous_week$ret[i] <- log(week_data$price_all[i-1]/week_data$price_all[i-2])
}

for(i in 1:nrow(previous_week)){
  previous_week$mood[i] <- sign(previous_week$ret[i])
}



#structure for splitting
week_data$search_negative <- NA
week_data$search_positive <- NA

week_data <- week_data[-c(1,2),]


#splitting search into positive and negative
for (i in 1:nrow(week_data )) {
  if(previous_week[i+2,3] < 0)
  {week_data$search_negative[i] <- week_data$SVI_diff[i]}
  else
  {week_data$search_negative[i] <- 0}
  
  if(previous_week[i+2,3] > 0) 
  {week_data$search_positive[i] <- week_data$SVI_diff[i]}
  else
  {week_data$search_positive[i] <-0}
}

sbst <- as.matrix(week_data[, c(18,21)])

# **** fitting var ******************************

fit <- VAR(sbst, p = 1, type = "none")
irf.var <- irf(fit, impulse = "search_negative", response = "logreturn_price_all",  n.ahead = 15, ci = 0.9)
plot(irf.var)

irf_med <- irf.var$irf$search_negative
irf_med[1] <- c("")

irf_low <- as.data.frame(matrix(NA, nrow = 16, ncol = 2))
irf_low[ ,1] <- c(1:16)
irf_low[,2] <- irf.var$Lower$search_negative
irf_low[1,2] <- c("")

irf_up <- as.data.frame(matrix(NA, nrow = 16, ncol = 2))
irf_up[,1] <- c(1:16)
irf_up[,2] <- irf.var$Upper$search_negative
irf_up[1,2] <- c("")

plot(irf_med,type = 'l', xlim = c(0,15), ylab = 'Log Returns', xlab = 'Weeks', col ="darkred",lwd=2)
polygon(c(irf_low[,1],rev(irf_up[,1])),c(irf_low[,2],rev(irf_up[,2])),col="lightgrey", border = NA)
abline(h = 0, lty=2)
lines(irf_low[,2], col = 'lightgrey')
lines(irf_up[,2], col = 'lightgrey')
lines(irf_med, lwd = 2, col = "darkred")




sbst <- as.matrix(week_data[, c(18,22)])

fit <- VAR(sbst, p = 1, type = "none")
irf.var <- irf(fit, impulse = "search_positive", response = "logreturn_price_all",  n.ahead = 15, ci = 0.9)
plot(irf.var)

irf_med <- irf.var$irf$search_positive
irf_med[1] <- c("")

irf_low <- as.data.frame(matrix(NA, nrow = 16, ncol = 2))
irf_low[ ,1] <- c(1:16)
irf_low[,2] <- irf.var$Lower$search_positive
irf_low[1,2] <- c("")

irf_up <- as.data.frame(matrix(NA, nrow = 16, ncol = 2))
irf_up[,1] <- c(1:16)
irf_up[,2] <- irf.var$Upper$search_positive
irf_up[1,2] <- c("")

plot(irf_med,type = 'l', xlim = c(0,15), ylab = 'Log Returns', xlab = 'Weeks', col ="darkred",lwd=2)
polygon(c(irf_low[,1],rev(irf_up[,1])),c(irf_low[,2],rev(irf_up[,2])),col="lightgrey", border = NA)
abline(h = 0, lty=2)
lines(irf_low[,2], col = 'lightgrey')
lines(irf_up[,2], col = 'lightgrey')
lines(irf_med, lwd = 2, col = "darkred")


sbst <- as.matrix(week_data[, c(18,19)])

fit <- VAR(sbst, p = 1, type = "none")
irf.var <- irf(fit, impulse = "SVI_diff", response = "logreturn_price_all",  n.ahead = 15, ci = 0.9)
plot(irf.var)


# **** fitting s var ******************************

fit <- fitVAR(sbst, p = 20, penalty = "SCAD", alpha = 1)

#sparsevar::plotVAR(fit)
fit$A


#computeForecasts(fit, 10)

irf <- impulseResponse(fit,20)
eb <- errorBandsIRF(fit, irf)


plotIRFGrid(irf, eb, c(1,2))