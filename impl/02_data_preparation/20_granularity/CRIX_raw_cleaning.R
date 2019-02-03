setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/CRIX", sep = "")
setwd(wd) 
rm(list=ls())

library("tseries")
library("zoo")
library("forecast")
library("ggplot2")
library('stats')
library("gridExtra")
library("reshape2")
library("tcltk")
library("sarima")


# *** loading data ****************************

library(plyr)

# 1) ***************** raw CRIX *****************************************
CRIX <- read.csv2(file = "Crix_HF.csv", header = TRUE, sep = ",")
CRIX$price <- as.numeric(levels(CRIX$price))[CRIX$price]
CRIX$unix <- as.numeric(as.POSIXct(CRIX$Timestamp, 'GMT'))

# for (i in 2:nrow(CRIX)) {
#   CRIX$diff[i] <- CRIX$unix[i]-CRIX$unix[i-1]
#   print(paste(i, "assigning"))
# }

CRIX <- CRIX[!duplicated(CRIX), ]

which.max(CRIX$price)

CRIX$price[187376] <- 1.716972e+03
CRIX$price[187378] <- 1.723707e+03

min(CRIX$price, na.rm = TRUE)
max(CRIX$price, na.rm = TRUE)
which.max(CRIX$price)
which.min(CRIX$price)

CRIX$price[14221] <- 15946.255569

plot(CRIX$price)

save(CRIX,file="CRIX_raw.Rdata" )



load(file="CRIX_5minute.Rdata")
plot(CRIX_5minute_data$value)

load(file="CRIX_15minute.Rdata")
plot(CRIX_15minute_data$value)

load(file="CRIX_30minute.Rdata")
plot(CRIX$price)

load(file="CRIX_60minute.Rdata")
plot(CRIX$price)

load(file="CRIX_day.Rdata")
plot(CRIX$price)