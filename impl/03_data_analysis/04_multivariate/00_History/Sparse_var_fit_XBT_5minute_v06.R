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
load(file="XBT_5minute_returns.Rdata")

# *** adding hourly / daily trend ***********************************************
#generating the data structure
hourly_trend <- as.data.frame(matrix(1, nrow = nrow(five_minute_data), ncol = 2))
colnames(hourly_trend) <- c("time", "hourly_ret")

daily_trend <- as.data.frame(matrix(1, nrow = nrow(five_minute_data), ncol = 2))
colnames(daily_trend) <- c("time", "daily_ret")

#getting hourly trend
for(i in 13:nrow(five_minute_data)){
  hourly_trend$time_min[i] <- five_minute_data$time_min[i]
  hourly_trend$hourly_ret[i] <- log(five_minute_data$price_all[i]/five_minute_data$price_all[i-12])
}

for(i in 1:nrow(five_minute_data)){
  hourly_trend$hourly_trend[i] <- sign(hourly_trend$hourly_ret[i])
}

# getting daily trend
for(i in 289:nrow(five_minute_data)){
  daily_trend$time_min[i] <- five_minute_data$time_min[i]
  daily_trend$daily_ret[i] <- log(five_minute_data$price_all[i]/five_minute_data$price_all[i-288])
}

for(i in 1:nrow(five_minute_data)){
  daily_trend$daily_trend[i] <- sign(daily_trend$daily_ret[i])
}

#looping to assign trend to time stamp
for (i in 1:nrow(five_minute_data)) {
  five_minute_data$daily_trend[i] <- daily_trend$daily_trend[match(five_minute_data$time_min[i], daily_trend$time_min)]
  five_minute_data$hourly_trend[i] <- hourly_trend$hourly_trend[match(five_minute_data$time_min[i], hourly_trend$time_min)]
}

#structure for splitting
five_minute_data$search_negative_hourly <- NA
five_minute_data$search_positive_hourly <- NA
five_minute_data$search_negative_daily <- NA
five_minute_data$search_positive_daily <- NA

#splitting search into positive and negative
for (i in 13:nrow(five_minute_data)) {
  if(hourly_trend[i,3] < 0)
  {five_minute_data$search_negative_hourly[i] <- five_minute_data$SVI_log_diff[i]}
  else
  {five_minute_data$search_negative_hourly[i] <- 0}
  
  if(hourly_trend[i,3] > 0) 
  {five_minute_data$search_positive_hourly[i] <- five_minute_data$SVI_log_diff[i]}
  else
  {five_minute_data$search_positive_hourly[i] <-0}
}
for (i in 289:nrow(five_minute_data)) {
  if(daily_trend[i,3] < 0)
  {five_minute_data$search_negative_daily[i] <- five_minute_data$SVI_log_diff[i]}
  else
  {five_minute_data$search_negative_daily[i] <- 0}
  
  if(daily_trend[i,3] > 0) 
  {five_minute_data$search_positive_daily[i] <- five_minute_data$SVI_log_diff[i]}
  else
  {five_minute_data$search_positive_daily[i] <-0}
}

save(five_minute_data,file="XBT_5minute_returns_w_search_trends.Rdata")
load(file="XBT_5minute_returns_w_search_trends.Rdata")

#setting up parametr of the window
window_start <- 289 #any number from number of lags to nrow-1
window_end <- 105420 #any number from start to nrow
lookback <- 72



# **** fitting var ******************************
sbst <- as.matrix(five_minute_data[c(window_start:window_end), c(18,23)])

fit <- VAR(sbst, p = 1, type = "none")
irf.var <- irf(fit, impulse = "search_negative_daily", response = "logreturn_price_all",  n.ahead = 15, ci = 0.95)
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

pdf("plot_neg_5mins_XBT_daily_tred.pdf")

plot(irf_med,type = 'l', xlim = c(0,15), ylim = c(-5e-04,5e-04), ylab = 'Log Returns', xlab = 'Weeks', col ="darkred",lwd=2)
polygon(c(irf_low[,1],rev(irf_up[,1])),c(irf_low[,2],rev(irf_up[,2])),col="lightgrey", border = NA)
abline(h = 0, lty=2)
lines(irf_low[,2], col = 'lightgrey')
lines(irf_up[,2], col = 'lightgrey')
lines(irf_med, lwd = 2, col = "darkred")

dev.off()


# **************** plotting IRF for positive search ****************************

sbst <- as.matrix(five_minute_data[c(window_start:window_end), c(18,24)])

fit <- VAR(sbst, p = 1, type = "none")
irf.var <- irf(fit, impulse = "search_positive_daily", response = "logreturn_price_all",  n.ahead = 15, ci = 0.95)
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

pdf("plot_pos_5mins_XBT_daily_tred.pdf")

plot(irf_med,type = 'l', xlim = c(0,15),ylim = c(-5e-04,5e-04), ylab = 'Log Returns', xlab = 'Weeks', col ="darkred",lwd=2)
polygon(c(irf_low[,1],rev(irf_up[,1])),c(irf_low[,2],rev(irf_up[,2])),col="lightgrey", border = NA)
abline(h = 0, lty=2)
lines(irf_low[,2], col = 'lightgrey')
lines(irf_up[,2], col = 'lightgrey')
lines(irf_med, lwd = 2, col = "darkred")

dev.off()

# *************** plotting IRF for all search **********************
sbst <- as.matrix(five_minute_data[c(window_start:window_end), c(18,19)])

fit <- VAR(sbst, p = 1, type = "none")
irf.var <- irf(fit, impulse = "SVI_diff", response = "logreturn_price_all",  n.ahead = 15, ci = 0.95)
plot(irf.var)

irf_med <- irf.var$irf$SVI_diff
irf_med[1] <- c("")

irf_low <- as.data.frame(matrix(NA, nrow = 16, ncol = 2))
irf_low[ ,1] <- c(1:16)
irf_low[,2] <- irf.var$Lower$SVI_diff
irf_low[1,2] <- c("")

irf_up <- as.data.frame(matrix(NA, nrow = 16, ncol = 2))
irf_up[,1] <- c(1:16)
irf_up[,2] <- irf.var$Upper$SVI_diff
irf_up[1,2] <- c("")

pdf("plot_neu_5mins_XBT_hourly_tred.pdf")

plot(irf_med,type = 'l', xlim = c(0,15), ylab = 'Log Returns', xlab = 'Weeks', col ="darkred",lwd=2)
polygon(c(irf_low[,1],rev(irf_up[,1])),c(irf_low[,2],rev(irf_up[,2])),col="lightgrey", border = NA)
abline(h = 0, lty=2)
lines(irf_low[,2], col = 'lightgrey')
lines(irf_up[,2], col = 'lightgrey')
lines(irf_med, lwd = 2, col = "darkred")

dev.off()

# sbst <- as.matrix(na.omit(five_minute_data[c((window_start) :window_end), c(18,21)]))
# 
# fit <- VAR(sbst, p = 1, type = "none")
# irf.var.neg <- irf(fit, impulse = "search_negative_hourly", response = "logreturn_price_all",  n.ahead = 25, ci = 0.9)
# 
# 
# sbst <- as.matrix(na.omit(five_minute_data[c((window_start) :window_end), c(22,18)]))
# 
# fit <- VAR(sbst, p = 1, type = "none")
# irf.var.pos <- irf(fit, response = "search_positive_hourly", impulse = "logreturn_price_all",  n.ahead = 25, ci = 0.9)
# 
# sbst <- as.matrix(na.omit(five_minute_data[c((window_start) :window_end), c(2,10)]))
# 
# fit <- VAR(sbst, p = 1, type = "none")
# irf.var.neu <- irf(fit, impulse = "SVI", response = "volume_all",  n.ahead = 25, ci = 0.9)
# 
# par(mfrow=c(2,1))
# plot(irf.var.pos)
# plot(irf.var.neg)
# plot(irf.var.neu)
# 
# # **** fitting s var ******************************
# 
# fit <- fitVAR(sbst, p = 20, penalty = "SCAD", alpha = 1)
# 
# #sparsevar::plotVAR(fit)
# fit$A
# 
# 
# #computeForecasts(fit, 10)
# 
# irf <- impulseResponse(fit,20)
# eb <- errorBandsIRF(fit, irf)
# 
# 
# plotIRFGrid(irf, eb, c(1,2,3))