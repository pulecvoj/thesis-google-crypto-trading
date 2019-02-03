setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/CRIX", sep = "")
setwd(wd)
rm(list=ls())

library(plyr)

# 1) ***************** raw CRIX *****************************************
CRIX <- read.csv2(file = "Crix_HF.csv", header = TRUE, sep = ",")
CRIX$price <- as.numeric(levels(CRIX$price))[CRIX$price]
CRIX$unix <- as.numeric(as.POSIXct(CRIX$Timestamp, 'GMT'))
save(CRIX, file="CRIX_raw.RData")

# 2) ***************** 5 mins *****************************************
load(file = "CRIX_raw.RData")
CRIX$round_unix <- (CRIX$unix  %/% 300 +1)* 300

CRIX_5minute_data <- as.data.frame(matrix(data = 1, nrow = length(unique(CRIX$round_unix)), ncol = 2))
CRIX_5minute_data <- colnames(c("timestamp", "Index"))
CRIX_5minute_data$timestamp <- unique(CRIX$round_unix)
CRIX_5minute_data$timestamp <- CRIX_5minute_data$timestamp[order(CRIX_5minute_data$timestamp)]

prev <- CRIX$round_unix
CRIX_5minute_data$value <- aggregate(CRIX$price, list(prev), sum, na.rm = TRUE)[,2] # mean of CRIX
CRIX_5minute_data <- as.data.frame(CRIX_5minute_data)
save(CRIX, file="CRIX_5minute.RData")

# 3) ***************** 15 mins *****************************************
load(file = "CRIX_raw.RData")
CRIX$round_unix <- (CRIX$unix  %/% 900 +1)* 900

CRIX_15minute_data <- as.data.frame(matrix(data = 1, nrow = length(unique(CRIX$round_unix)), ncol = 2))
CRIX_15minute_data <- colnames(c("timestamp", "Index"))
CRIX_15minute_data$timestamp <- unique(CRIX$round_unix)
CRIX_15minute_data$timestamp <- CRIX_15minute_data$timestamp[order(CRIX_15minute_data$timestamp)]

prev <- CRIX$round_unix
CRIX_15minute_data$value <- aggregate(CRIX$price, list(prev), sum, na.rm = TRUE)[,2]/3 # mean of CRIX
CRIX_15minute_data <- as.data.frame(CRIX_15minute_data)
save(CRIX, file="CRIX_15minute.RData")

# 4) ***************** 30 mins *****************************************
load(file = "CRIX_raw.RData")
CRIX$round_unix <- (CRIX$unix  %/% 1800 +1)* 1800

CRIX_30minute_data <- as.data.frame(matrix(data = 1, nrow = length(unique(CRIX$round_unix)), ncol = 2))
CRIX_30minute_data <- colnames(c("timestamp", "Index"))
CRIX_30minute_data$timestamp <- unique(CRIX$round_unix)
CRIX_30minute_data$timestamp <- CRIX_30minute_data$timestamp[order(CRIX_30minute_data$timestamp)]

prev <- CRIX$round_unix
CRIX_30minute_data$value <- aggregate(CRIX$price, list(prev), sum, na.rm = TRUE)[,2]/3 # mean of CRIX
CRIX_30minute_data <- as.data.frame(CRIX_30minute_data)
save(CRIX, file="CRIX_30minute.RData")

# 5) ***************** 60 mins *****************************************
load(file = "CRIX_raw.RData")
CRIX$round_unix <- (CRIX$unix  %/% 3600 +1)* 3600

CRIX_60minute_data <- as.data.frame(matrix(data = 1, nrow = length(unique(CRIX$round_unix)), ncol = 2))
CRIX_60minute_data <- colnames(c("timestamp", "Index"))
CRIX_60minute_data$timestamp <- unique(CRIX$round_unix)
CRIX_60minute_data$timestamp <- CRIX_60minute_data$timestamp[order(CRIX_60minute_data$timestamp)]

prev <- CRIX$round_unix
CRIX_60minute_data$value <- aggregate(CRIX$price, list(prev), sum, na.rm = TRUE)[,2]/3 # mean of CRIX
CRIX_60minute_data <- as.data.frame(CRIX_60minute_data)
save(CRIX, file="CRIX_60minute.RData")

# 6) ***************** day *****************************************
load(file = "CRIX_raw.RData")
CRIX$round_unix <- (CRIX$unix  %/% 86400 +1)* 86400

CRIX_day_data <- as.data.frame(matrix(data = 1, nrow = length(unique(CRIX$round_unix)), ncol = 2))
CRIX_day_data <- colnames(c("timestamp", "Index"))
CRIX_day_data$timestamp <- unique(CRIX$round_unix)
CRIX_day_data$timestamp <- CRIX_day_data$timestamp[order(CRIX_day_data$timestamp)]

prev <- CRIX$round_unix
CRIX_day_data$value <- aggregate(CRIX$price, list(prev), sum, na.rm = TRUE)[,2]/3 # mean of CRIX
CRIX_day_data <- as.data.frame(CRIX_day_data)
save(CRIX, file="CRIX_day.RData")