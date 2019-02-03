setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/CRIX", sep = "")
setwd(wd)
rm(list=ls())

library(plyr)

# # 1) ***************** raw CRIX *****************************************
# CRIX <- read.csv2(file = "Crix_HF.csv", header = TRUE, sep = ",")
# CRIX$price <- as.numeric(levels(CRIX$price))[CRIX$price]
# CRIX$unix <- as.numeric(as.POSIXct(CRIX$Timestamp, 'GMT'))
# 
# for (i in 2:nrow(CRIX)) {
#   CRIX$diff[i] <- CRIX$unix[i]-CRIX$unix[i-1]
#   print(paste(i, "assigning"))
# }
# 
# save(CRIX, file="CRIX_raw.RData")
par(mfrow=c(3,2))
# 2) ***************** 5 mins *****************************************
load(file = "CRIX_raw.RData")

CRIX$round_unix <- (CRIX$unix  %/% 300 +1)* 300

CRIX_5minute_data <- as.data.frame(matrix(data = 1, nrow = length(unique(CRIX$round_unix)), ncol = 2))
colnames(CRIX_5minute_data) <- c("timestamp", "Index")
CRIX_5minute_data$timestamp <- unique(CRIX$round_unix)
CRIX_5minute_data <- CRIX_5minute_data[order(CRIX_5minute_data$timestamp),]

prev <- CRIX$round_unix
CRIX_5minute_data$Index <- aggregate(CRIX$price, list(prev), mean, na.rm = TRUE)[,2] # mean of CRIX
CRIX_5minute_data <- as.data.frame(CRIX_5minute_data)
plot(CRIX_5minute_data$Index)

save(CRIX_5minute_data, file="CRIX_5minute.RData")

# 3) ***************** 15 mins *****************************************
load(file = "CRIX_raw.RData")
CRIX$round_unix <- (CRIX$unix  %/% 900 +1)* 900

CRIX_15minute_data <- as.data.frame(matrix(data = 1, nrow = length(unique(CRIX$round_unix)), ncol = 2))
colnames(CRIX_15minute_data) <- c("timestamp", "Index")
CRIX_15minute_data$timestamp <- unique(CRIX$round_unix)
CRIX_15minute_data <- CRIX_15minute_data[order(CRIX_15minute_data$timestamp),]

prev <- CRIX$round_unix
CRIX_15minute_data$Index <- aggregate(CRIX$price, list(prev), mean, na.rm = TRUE)[,2] # mean of CRIX
CRIX_15minute_data <- as.data.frame(CRIX_15minute_data)
plot(CRIX_15minute_data$Index)

save(CRIX_15minute_data, file="CRIX_15minute.RData")

# 4) ***************** 30 mins *****************************************
load(file = "CRIX_raw.RData")
CRIX$round_unix <- (CRIX$unix  %/% 1800 +1)* 1800

CRIX_30minute_data <- as.data.frame(matrix(data = 1, nrow = length(unique(CRIX$round_unix)), ncol = 2))
colnames(CRIX_30minute_data) <- c("timestamp", "Index")
CRIX_30minute_data$timestamp <- unique(CRIX$round_unix)
CRIX_30minute_data <- CRIX_30minute_data[order(CRIX_30minute_data$timestamp),]

prev <- CRIX$round_unix
CRIX_30minute_data$Index <- aggregate(CRIX$price, list(prev), mean, na.rm = TRUE)[,2] # mean of CRIX
CRIX_30minute_data <- as.data.frame(CRIX_30minute_data)
plot(CRIX_30minute_data$Index)

save(CRIX_30minute_data, file="CRIX_30minute.RData")

# 5) ***************** 60 mins *****************************************
load(file = "CRIX_raw.RData")
CRIX$round_unix <- (CRIX$unix  %/% 3600 +1)* 3600

CRIX_60minute_data <- as.data.frame(matrix(data = 1, nrow = length(unique(CRIX$round_unix)), ncol = 2))
colnames(CRIX_60minute_data) <- c("timestamp", "Index")
CRIX_60minute_data$timestamp <- unique(CRIX$round_unix)
CRIX_60minute_data <- CRIX_60minute_data[order(CRIX_60minute_data$timestamp),]

prev <- CRIX$round_unix
CRIX_60minute_data$Index <- aggregate(CRIX$price, list(prev), mean, na.rm = TRUE)[,2] # mean of CRIX
CRIX_60minute_data <- as.data.frame(CRIX_60minute_data)
plot(CRIX_60minute_data$Index)

save(CRIX_60minute_data, file="CRIX_60minute.RData")

# 6) ***************** day *****************************************
load(file = "CRIX_raw.RData")
CRIX$round_unix <- (CRIX$unix  %/% 86400 +1)* 86400

CRIX_day_data <- as.data.frame(matrix(data = 1, nrow = length(unique(CRIX$round_unix)), ncol = 2))
colnames(CRIX_day_data) <- c("timestamp", "Index")
CRIX_day_data$timestamp <- unique(CRIX$round_unix)
CRIX_day_data <- CRIX_day_data[order(CRIX_day_data$timestamp),]

prev <- CRIX$round_unix
CRIX_day_data$Index <- aggregate(CRIX$price, list(prev), mean, na.rm = TRUE)[,2] # mean of CRIX
CRIX_day_data <- as.data.frame(CRIX_day_data)
plot(CRIX_day_data$Index)

save(CRIX_day_data, file="CRIX_day.RData")