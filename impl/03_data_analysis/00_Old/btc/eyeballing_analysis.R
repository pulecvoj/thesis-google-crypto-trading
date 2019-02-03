setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/test_sample/bitcoin", sep = "")
setwd(wd) 
rm(list=ls())

load(file = "minute_data_bc.RData")
load(file = "five_minute_data_bc.RData")

wd <- getwd()
wd <- substr(wd, start = 1, stop = nchar(wd)-nchar("/data/test_sample/bitcoin"))
wd <- paste(wd, "/impl/data_exploration/btc", sep = "")
setwd(wd) 

plot(five_minute_data$search, type = "l", col = "red",xlab="Observations", ylab="SVI",  main="SVI vs Price")
par(new=TRUE)
plot(five_minute_data$price_all, col = "black", type = "l",xaxt="n",yaxt="n",xlab="", ylab="")
axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price"),
       col=c("red", "black", "white"), lty=1, cex=0.9)


plot(five_minute_data$search, type = "l", col = "red",xlab="Observations", ylab="SVI",  main="SVI vs Price",xlim = c(1000, 1288))
par(new=TRUE)
plot(five_minute_data$price_all, col = "black", type = "l",xaxt="n",yaxt="n",xlab="", ylab="",xlim = c(1000, 1288))
axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price", "Corr = -0.86"),
       col=c("red", "black", "white"), lty=1, cex=0.9)
cor(five_minute_data$search[1000:1288],five_minute_data$price_all[1000:1288])


plot(five_minute_data$search, type = "l", col = "red",xlab="Observations", ylab="SVI",  main="SVI vs Price",xlim = c(3100, 3388))
par(new=TRUE)
plot(five_minute_data$price_all, col = "black", type = "l",xaxt="n",yaxt="n",xlab="", ylab="",xlim = c(3100, 3388))
axis(4)
mtext(side=4,line=3, 'Price')
legend("topleft", legend=c("Search", "Price", "Corr = 0.89"),
       col=c("red", "black", "white"), lty=1, cex=0.9)
cor(five_minute_data$search[3100:3388],five_minute_data$price_all[3100:3388])

five_m_correlation <- NA

for(i in 1:5700){
  five_m_correlation[i] <- cor(five_minute_data$search[i:(i+288)],five_minute_data$price_all[i:(i+288)])
}

plot(five_m_correlation, type = "l",main="Correlation of SVI and Price",ylab="Corr", xlab = "Obs")


