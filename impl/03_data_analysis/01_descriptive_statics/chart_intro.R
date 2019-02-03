setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd) 
rm(list=ls())

#library(tseries)
#library(moments)
#library(ggplot2)
par(mfrow=c(2,2))

# 1) *** loading data ****************************
load(file="XBT_day_returns.Rdata")
load(file="XBT_15minutes_returns.Rdata")

plot(fifteen_minute_data$SVI, type = "l", col = "red",xlab="Observations", ylab="SVI",  main="SVI vs Price")
par(new=TRUE)
plot(fifteen_minute_data$price_all, col = "black", type = "l",xaxt="n",yaxt="n",xlab="", ylab="")
axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price"),
       col=c("red", "black", "white"), lty=1, cex=0.9)

cor(fifteen_minute_data$SVI,fifteen_minute_data$price_all)

plot(hour_data$SVI, type = "l", col = "red",xlab="Observations", ylab="SVI",  main="SVI vs Price")
par(new=TRUE)
plot(hour_data$price_all, col = "black", type = "l",xaxt="n",yaxt="n",xlab="", ylab="")
axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price"),
       col=c("red", "black", "white"), lty=1, cex=0.9)

cor(hour_data$SVI,hour_data$price_all)

plot(day_data$SVI, type = "l", col = "red",xlab="Observations", ylab="SVI",  main="SVI vs Price")
par(new=TRUE)
plot(day_data$price_all, col = "black", type = "l",xaxt="n",yaxt="n",xlab="", ylab="")
axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price"),
       col=c("red", "black", "white"), lty=1, cex=0.9)

cor(day_data$SVI,day_data$price_all)

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
setwd(wd) 

load(file="ETH_day_returns.Rdata")

plot(hour_data$SVI, type = "l", col = "red",xlab="Observations", ylab="SVI",  main="SVI vs Price")
par(new=TRUE)
plot(hour_data$price_all, col = "black", type = "l",xaxt="n",yaxt="n",xlab="", ylab="")
axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price"),
       col=c("red", "black", "white"), lty=1, cex=0.9)

cor(hour_data$SVI,hour_data$price_all)

plot(day_data$SVI, type = "l", col = "red",xlab="Observations", ylab="SVI",  main="SVI vs Price")
par(new=TRUE)
plot(day_data$price_all, col = "black", type = "l",xaxt="n",yaxt="n",xlab="", ylab="")
axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price"),
       col=c("red", "black", "white"), lty=1, cex=0.9)

cor(day_data$SVI,day_data$price_all)

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
setwd(wd) 

load(file="LTC_day_returns.Rdata")

plot(day_data$SVI, type = "l", col = "red",xlab="Observations", ylab="SVI",  main="SVI vs Price")
par(new=TRUE)
plot(day_data$price_all, col = "black", type = "l",xaxt="n",yaxt="n",xlab="", ylab="")
axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price"),
       col=c("red", "black", "white"), lty=1, cex=0.9)

cor(day_data$SVI,day_data$price_all)

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
setwd(wd) 

load(file="XMR_day_returns.Rdata")

plot(day_data$SVI, type = "l", col = "red",xlab="Observations", ylab="SVI",  main="SVI vs Price")
par(new=TRUE)
plot(day_data$price_all, col = "black", type = "l",xaxt="n",yaxt="n",xlab="", ylab="")
axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price"),
       col=c("red", "black", "white"), lty=1, cex=0.9)

cor(day_data$SVI,day_data$price_all)

par(mfrow=c(2,2))
hist(histogram$XBT,
     main="XBT", 
     xlab="1 minute returns", 
     ylab = "",
     border="black", 
     col="gray",
     xlim=c(-0.005,0.005),
     ylim = c(0,175000),
     las=1, 
     breaks=2500)
hist(histogram$ETH,
     main="ETH", 
     xlab="1 minute returns", 
     ylab = "",
     border="black", 
     col="gray",
     xlim=c(-0.005,0.005),
     ylim = c(0,175000),
     las=1, 
     breaks= 1000)
hist(histogram$LTC,
     main="LTC", 
     xlab="1 minute returns", 
     ylab = "",
     border="black", 
     col="gray",
     xlim=c(-0.005,0.005),
     ylim = c(0,175000),
     las=1, 
     breaks=500)
hist(histogram$XMR,
     main="XMR", 
     xlab="1 minute returns", 
     ylab = "",
     border="black", 
     col="gray",
     xlim=c(-0.005,0.005),
     ylim = c(0,175000),
     las=1, 
     breaks=500)

