setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd) 
rm(list=ls())

library(tseries)
library(moments)
library(ggplot2)

# 1) *** loading data ****************************
load(file="XXBTZEUR.Rdata")
XXBTZEUR <- XXBTZEUR[-1,]
XXBTZEUR[,1] <- as.numeric(XXBTZEUR[,1])
XXBTZEUR[,2] <- as.numeric(XXBTZEUR[,2])
sum(XXBTZEUR$Volume, na.rm = TRUE)

XXBTZEUR$Value <- XXBTZEUR$Volume * XXBTZEUR$Price
sum(XXBTZEUR$Value, na.rm = TRUE)

# 2) ******* going to eth ****************
setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
setwd(wd) 

load(file="XETHZEUR.Rdata")
XETHZEUR <- XETHZEUR[-1,]
XETHZEUR[,1] <- as.numeric(XETHZEUR[,1])
XETHZEUR[,2] <- as.numeric(XETHZEUR[,2])
sum(XETHZEUR$Volume, na.rm = TRUE)

XETHZEUR$Value <- XETHZEUR$Volume * XETHZEUR$Price
sum(XETHZEUR$Value, na.rm = TRUE)

# 3) ******* going to LTC ****************
setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
setwd(wd) 

load(file="XLTCZEUR.Rdata")
XLTCZEUR <- XLTCZEUR[-1,]
XLTCZEUR[,1] <- as.numeric(XLTCZEUR[,1])
XLTCZEUR[,2] <- as.numeric(XLTCZEUR[,2])
sum(XLTCZEUR$Volume, na.rm = TRUE)

XLTCZEUR$Value <- XLTCZEUR$Volume * XLTCZEUR$Price
sum(XLTCZEUR$Value, na.rm = TRUE)

# 4) ******* going to XMR ****************
setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
setwd(wd) 

load(file="XXMRZEUR.Rdata")
XXMRZEUR <- XXMRZEUR[-1,]
XXMRZEUR[,1] <- as.numeric(XXMRZEUR[,1])
XXMRZEUR[,2] <- as.numeric(XXMRZEUR[,2])
sum(XXMRZEUR$Volume, na.rm = TRUE)

XXMRZEUR$Value <- XXMRZEUR$Volume * XXMRZEUR$Price
sum(XXMRZEUR$Value, na.rm = TRUE)
