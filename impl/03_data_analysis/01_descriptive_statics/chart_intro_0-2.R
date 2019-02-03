#setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
setwd("C:/Users/pulec.vojtech/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec")
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

head(day_data$price_all)
tail(day_data$price_all)

plot(day_data$SVI, type = "l", col = "darkblue",xlab="Observations", ylab="",  main="XBT", lwd = 3)
par(new=TRUE)
plot(day_data$price_all, col = "darkgrey", type = "l",xaxt="n",yaxt="n",xlab="", ylab="", lwd = 3, ann=FALSE)
#axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price"),
       col=c("darkblue", "darkgrey", "white"), lty=1, cex=1.2, lwd = 3)

cor(day_data$SVI,day_data$price_all)

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
setwd(wd) 

load(file="ETH_day_returns.Rdata")

head(day_data$price_all)
tail(day_data$price_all)

plot(day_data$SVI, type = "l", col = "darkblue",xlab="Observations", ylab="",  main="ETH", lwd = 3, yaxt='n')
par(new=TRUE)
plot(day_data$price_all, col = "darkgrey", type = "l",xaxt="n",yaxt="n",xlab="", ylab="", lwd = 3, ann=FALSE)
#axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price"),
       col=c("darkblue", "darkgrey", "white"), lty=1, cex=1.2, lwd = 3)

cor(day_data$SVI,day_data$price_all)

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
setwd(wd) 

load(file="LTC_day_returns.Rdata")

head(day_data$price_all)
tail(day_data$price_all)

plot(day_data$SVI, type = "l", col = "darkblue",xlab="Observations", ylab="",  main="LTC", lwd = 3, yaxt='n')
par(new=TRUE)
plot(day_data$price_all, col = "darkgrey", type = "l",xaxt="n",yaxt="n",xlab="", ylab="", lwd = 3, ann=FALSE)
#axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price"),
       col=c("darkblue", "darkgrey", "white"), lty=1, cex=1.2, lwd = 3)

cor(day_data$SVI,day_data$price_all)

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
setwd(wd) 

load(file="XMR_day_returns.Rdata")

head(day_data$price_all)
tail(day_data$price_all)

plot(day_data$SVI, type = "l", col = "darkblue",xlab="Observations", ylab="",  main="XMR", lwd = 3, yaxt='n')
par(new=TRUE)
plot(day_data$price_all, col = "darkgrey", type = "l",xaxt="n",yaxt="n",xlab="", ylab="", lwd = 3, ann=FALSE)
#axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price"),
       col=c("darkblue", "darkgrey", "white"), lty=1, cex=1.2, lwd = 3)

cor(day_data$SVI,day_data$price_all)