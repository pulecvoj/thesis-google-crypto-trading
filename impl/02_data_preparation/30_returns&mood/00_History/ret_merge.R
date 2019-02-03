setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
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
load(file = "XMR_week.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(week_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(week_data)[i]
  nr_nas_1[i,2] <- nrow(week_data)
  nr_nas_1[i,3] <- sum(is.na(week_data[,i])) + sum(week_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(week_data) <- c("time", "SVI", "price_all",
                         "price_ba","price_bm", "price_bl",
                         "price_sa","price_sm", "price_sl",
                         "volume_all",
                         "volume_ba","volume_bm","volume_bl",
                         "volume_sa","volume_sm","volume_sl")

week_data <- na.locf(week_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
week_data$return_price_all <-NA
week_data$logreturn_price_all <- NA
week_data$SVI_diff <- NA
week_data$SVI_log_diff <- NA

for (i in 2:nrow(week_data)){
  week_data$return_price_all[i] <- (week_data$price_all[i] -  week_data$price_all[i-1])/week_data$price_all[i-1]
}

for (i in 2:nrow(week_data)){
  week_data$logreturn_price_all[i] <- log(week_data$price_all[i] / week_data$price_all[i-1])
}

for (i in 2:nrow(week_data)){
  week_data$SVI_diff[i] <- (week_data$SVI[i] -  week_data$SVI[i-1])/week_data$SVI[i-1]
}

for (i in 2:nrow(week_data)){
  week_data$SVI_log_diff[i] <- log(week_data$SVI[i] / week_data$SVI[i-1])
}


# subsetting
week_data <- week_data[-1,]


save(week_data,file="XMR_week_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
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
load(file = "XMR_day.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(day_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(day_data)[i]
  nr_nas_1[i,2] <- nrow(day_data)
  nr_nas_1[i,3] <- sum(is.na(day_data[,i])) + sum(day_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(day_data) <- c("time", "SVI", "price_all",
                        "price_ba","price_bm", "price_bl",
                        "price_sa","price_sm", "price_sl",
                        "volume_all",
                        "volume_ba","volume_bm","volume_bl",
                        "volume_sa","volume_sm","volume_sl")

day_data <- na.locf(day_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
day_data$return_price_all <-NA
day_data$logreturn_price_all <- NA
day_data$SVI_diff <- NA
day_data$SVI_log_diff <- NA

for (i in 2:nrow(day_data)){
  day_data$return_price_all[i] <- (day_data$price_all[i] -  day_data$price_all[i-1])/day_data$price_all[i-1]
}

for (i in 2:nrow(day_data)){
  day_data$logreturn_price_all[i] <- log(day_data$price_all[i] / day_data$price_all[i-1])
}

for (i in 2:nrow(day_data)){
  day_data$SVI_diff[i] <- (day_data$SVI[i] -  day_data$SVI[i-1])/day_data$SVI[i-1]
}

for (i in 2:nrow(day_data)){
  day_data$SVI_log_diff[i] <- log(day_data$SVI[i] / day_data$SVI[i-1])
}


# subsetting
day_data <- day_data[-1,]


save(day_data,file="XMR_day_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
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
load(file = "XMR_60minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(hour_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(hour_data)[i]
  nr_nas_1[i,2] <- nrow(hour_data)
  nr_nas_1[i,3] <- sum(is.na(hour_data[,i])) + sum(hour_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(hour_data) <- c("time", "SVI", "price_all",
                         "price_ba","price_bm", "price_bl",
                         "price_sa","price_sm", "price_sl",
                         "volume_all",
                         "volume_ba","volume_bm","volume_bl",
                         "volume_sa","volume_sm","volume_sl")

hour_data <- na.locf(hour_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
hour_data$return_price_all <-NA
hour_data$logreturn_price_all <- NA
hour_data$SVI_diff <- NA
hour_data$SVI_log_diff <- NA

for (i in 2:nrow(hour_data)){
  hour_data$return_price_all[i] <- (hour_data$price_all[i] -  hour_data$price_all[i-1])/hour_data$price_all[i-1]
}

for (i in 2:nrow(hour_data)){
  hour_data$logreturn_price_all[i] <- log(hour_data$price_all[i] / hour_data$price_all[i-1])
}

for (i in 2:nrow(hour_data)){
  hour_data$SVI_diff[i] <- (hour_data$SVI[i] -  hour_data$SVI[i-1])/hour_data$SVI[i-1]
}

for (i in 2:nrow(hour_data)){
  hour_data$SVI_log_diff[i] <- log(hour_data$SVI[i] / hour_data$SVI[i-1])
}


# subsetting
hour_data <- hour_data[-1,]


save(hour_data,file="XMR_60minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
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
load(file = "XMR_30minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(thirty_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(thirty_minute_data)[i]
  nr_nas_1[i,2] <- nrow(thirty_minute_data)
  nr_nas_1[i,3] <- sum(is.na(thirty_minute_data[,i])) + sum(thirty_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(thirty_minute_data) <- c("time", "SVI", "price_all",
                                  "price_ba","price_bm", "price_bl",
                                  "price_sa","price_sm", "price_sl",
                                  "volume_all",
                                  "volume_ba","volume_bm","volume_bl",
                                  "volume_sa","volume_sm","volume_sl")

thirty_minute_data <- na.locf(thirty_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
thirty_minute_data$return_price_all <-NA
thirty_minute_data$logreturn_price_all <- NA
thirty_minute_data$SVI_diff <- NA
thirty_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$return_price_all[i] <- (thirty_minute_data$price_all[i] -  thirty_minute_data$price_all[i-1])/thirty_minute_data$price_all[i-1]
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$logreturn_price_all[i] <- log(thirty_minute_data$price_all[i] / thirty_minute_data$price_all[i-1])
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$SVI_diff[i] <- (thirty_minute_data$SVI[i] -  thirty_minute_data$SVI[i-1])/thirty_minute_data$SVI[i-1]
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$SVI_log_diff[i] <- log(thirty_minute_data$SVI[i] / thirty_minute_data$SVI[i-1])
}


# subsetting
thirty_minute_data <- thirty_minute_data[-1,]


save(thirty_minute_data,file="XMR_30minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
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
load(file = "XMR_15minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(fifteen_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(fifteen_minute_data)[i]
  nr_nas_1[i,2] <- nrow(fifteen_minute_data)
  nr_nas_1[i,3] <- sum(is.na(fifteen_minute_data[,i])) + sum(fifteen_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(fifteen_minute_data) <- c("time", "SVI", "price_all",
                                   "price_ba","price_bm", "price_bl",
                                   "price_sa","price_sm", "price_sl",
                                   "volume_all",
                                   "volume_ba","volume_bm","volume_bl",
                                   "volume_sa","volume_sm","volume_sl")

fifteen_minute_data <- na.locf(fifteen_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
fifteen_minute_data$return_price_all <-NA
fifteen_minute_data$logreturn_price_all <- NA
fifteen_minute_data$SVI_diff <- NA
fifteen_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$return_price_all[i] <- (fifteen_minute_data$price_all[i] -  fifteen_minute_data$price_all[i-1])/fifteen_minute_data$price_all[i-1]
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$logreturn_price_all[i] <- log(fifteen_minute_data$price_all[i] / fifteen_minute_data$price_all[i-1])
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$SVI_diff[i] <- (fifteen_minute_data$SVI[i] -  fifteen_minute_data$SVI[i-1])/fifteen_minute_data$SVI[i-1]
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$SVI_log_diff[i] <- log(fifteen_minute_data$SVI[i] / fifteen_minute_data$SVI[i-1])
}


# subsetting
fifteen_minute_data <- fifteen_minute_data[-1,]


save(fifteen_minute_data,file="XMR_15minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
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
load(file = "XMR_5minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(five_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(five_minute_data)[i]
  nr_nas_1[i,2] <- nrow(five_minute_data)
  nr_nas_1[i,3] <- sum(is.na(five_minute_data[,i])) + sum(five_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(five_minute_data) <- c("time", "SVI", "price_all",
                                "price_ba","price_bm", "price_bl",
                                "price_sa","price_sm", "price_sl",
                                "volume_all",
                                "volume_ba","volume_bm","volume_bl",
                                "volume_sa","volume_sm","volume_sl")

five_minute_data <- na.locf(five_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
five_minute_data$return_price_all <-NA
five_minute_data$logreturn_price_all <- NA
five_minute_data$SVI_diff <- NA
five_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(five_minute_data)){
  five_minute_data$return_price_all[i] <- (five_minute_data$price_all[i] -  five_minute_data$price_all[i-1])/five_minute_data$price_all[i-1]
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$logreturn_price_all[i] <- log(five_minute_data$price_all[i] / five_minute_data$price_all[i-1])
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$SVI_diff[i] <- (five_minute_data$SVI[i] -  five_minute_data$SVI[i-1])/five_minute_data$SVI[i-1]
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$SVI_log_diff[i] <- log(five_minute_data$SVI[i] / five_minute_data$SVI[i-1])
}


# subsetting
five_minute_data <- five_minute_data[-1,]


save(five_minute_data,file="XMR_5minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
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
load(file = "XMR_1minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(minute_data)[i]
  nr_nas_1[i,2] <- nrow(minute_data)
  nr_nas_1[i,3] <- sum(is.na(minute_data[,i])) + sum(minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(minute_data) <- c("time", "SVI", "price_all",
                           "price_ba","price_bm", "price_bl",
                           "price_sa","price_sm", "price_sl",
                           "volume_all",
                           "volume_ba","volume_bm","volume_bl",
                           "volume_sa","volume_sm","volume_sl")

minute_data <- na.locf(minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
minute_data$return_price_all <-NA
minute_data$logreturn_price_all <- NA
minute_data$SVI_diff <- NA
minute_data$SVI_log_diff <- NA

for (i in 2:nrow(minute_data)){
  minute_data$return_price_all[i] <- (minute_data$price_all[i] -  minute_data$price_all[i-1])/minute_data$price_all[i-1]
}

for (i in 2:nrow(minute_data)){
  minute_data$logreturn_price_all[i] <- log(minute_data$price_all[i] / minute_data$price_all[i-1])
}

for (i in 2:nrow(minute_data)){
  minute_data$SVI_diff[i] <- (minute_data$SVI[i] -  minute_data$SVI[i-1])/minute_data$SVI[i-1]
}

for (i in 2:nrow(minute_data)){
  minute_data$SVI_log_diff[i] <- log(minute_data$SVI[i] / minute_data$SVI[i-1])
}


# subsetting
minute_data <- minute_data[-1,]


save(minute_data,file="XMR_1minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
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
load(file = "XBT_week.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(week_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(week_data)[i]
  nr_nas_1[i,2] <- nrow(week_data)
  nr_nas_1[i,3] <- sum(is.na(week_data[,i])) + sum(week_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(week_data) <- c("time", "SVI", "price_all",
                         "price_ba","price_bm", "price_bl",
                         "price_sa","price_sm", "price_sl",
                         "volume_all",
                         "volume_ba","volume_bm","volume_bl",
                         "volume_sa","volume_sm","volume_sl")

week_data <- na.locf(week_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
week_data$return_price_all <-NA
week_data$logreturn_price_all <- NA
week_data$SVI_diff <- NA
week_data$SVI_log_diff <- NA

for (i in 2:nrow(week_data)){
  week_data$return_price_all[i] <- (week_data$price_all[i] -  week_data$price_all[i-1])/week_data$price_all[i-1]
}

for (i in 2:nrow(week_data)){
  week_data$logreturn_price_all[i] <- log(week_data$price_all[i] / week_data$price_all[i-1])
}

for (i in 2:nrow(week_data)){
  week_data$SVI_diff[i] <- (week_data$SVI[i] -  week_data$SVI[i-1])/week_data$SVI[i-1]
}

for (i in 2:nrow(week_data)){
  week_data$SVI_log_diff[i] <- log(week_data$SVI[i] / week_data$SVI[i-1])
}


# subsetting
week_data <- week_data[-1,]


save(week_data,file="XBT_week_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
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
load(file = "XBT_day.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(day_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(day_data)[i]
  nr_nas_1[i,2] <- nrow(day_data)
  nr_nas_1[i,3] <- sum(is.na(day_data[,i])) + sum(day_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(day_data) <- c("time", "SVI", "price_all",
                        "price_ba","price_bm", "price_bl",
                        "price_sa","price_sm", "price_sl",
                        "volume_all",
                        "volume_ba","volume_bm","volume_bl",
                        "volume_sa","volume_sm","volume_sl")

day_data <- na.locf(day_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
day_data$return_price_all <-NA
day_data$logreturn_price_all <- NA
day_data$SVI_diff <- NA
day_data$SVI_log_diff <- NA

for (i in 2:nrow(day_data)){
  day_data$return_price_all[i] <- (day_data$price_all[i] -  day_data$price_all[i-1])/day_data$price_all[i-1]
}

for (i in 2:nrow(day_data)){
  day_data$logreturn_price_all[i] <- log(day_data$price_all[i] / day_data$price_all[i-1])
}

for (i in 2:nrow(day_data)){
  day_data$SVI_diff[i] <- (day_data$SVI[i] -  day_data$SVI[i-1])/day_data$SVI[i-1]
}

for (i in 2:nrow(day_data)){
  day_data$SVI_log_diff[i] <- log(day_data$SVI[i] / day_data$SVI[i-1])
}


# subsetting
day_data <- day_data[-1,]


save(day_data,file="XBT_day_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
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
load(file = "XBT_60minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(hour_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(hour_data)[i]
  nr_nas_1[i,2] <- nrow(hour_data)
  nr_nas_1[i,3] <- sum(is.na(hour_data[,i])) + sum(hour_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(hour_data) <- c("time", "SVI", "price_all",
                         "price_ba","price_bm", "price_bl",
                         "price_sa","price_sm", "price_sl",
                         "volume_all",
                         "volume_ba","volume_bm","volume_bl",
                         "volume_sa","volume_sm","volume_sl")

hour_data <- na.locf(hour_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
hour_data$return_price_all <-NA
hour_data$logreturn_price_all <- NA
hour_data$SVI_diff <- NA
hour_data$SVI_log_diff <- NA

for (i in 2:nrow(hour_data)){
  hour_data$return_price_all[i] <- (hour_data$price_all[i] -  hour_data$price_all[i-1])/hour_data$price_all[i-1]
}

for (i in 2:nrow(hour_data)){
  hour_data$logreturn_price_all[i] <- log(hour_data$price_all[i] / hour_data$price_all[i-1])
}

for (i in 2:nrow(hour_data)){
  hour_data$SVI_diff[i] <- (hour_data$SVI[i] -  hour_data$SVI[i-1])/hour_data$SVI[i-1]
}

for (i in 2:nrow(hour_data)){
  hour_data$SVI_log_diff[i] <- log(hour_data$SVI[i] / hour_data$SVI[i-1])
}


# subsetting
hour_data <- hour_data[-1,]


save(hour_data,file="XBT_60minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
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
load(file = "XBT_30minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(thirty_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(thirty_minute_data)[i]
  nr_nas_1[i,2] <- nrow(thirty_minute_data)
  nr_nas_1[i,3] <- sum(is.na(thirty_minute_data[,i])) + sum(thirty_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(thirty_minute_data) <- c("time", "SVI", "price_all",
                                  "price_ba","price_bm", "price_bl",
                                  "price_sa","price_sm", "price_sl",
                                  "volume_all",
                                  "volume_ba","volume_bm","volume_bl",
                                  "volume_sa","volume_sm","volume_sl")

thirty_minute_data <- na.locf(thirty_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
thirty_minute_data$return_price_all <-NA
thirty_minute_data$logreturn_price_all <- NA
thirty_minute_data$SVI_diff <- NA
thirty_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$return_price_all[i] <- (thirty_minute_data$price_all[i] -  thirty_minute_data$price_all[i-1])/thirty_minute_data$price_all[i-1]
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$logreturn_price_all[i] <- log(thirty_minute_data$price_all[i] / thirty_minute_data$price_all[i-1])
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$SVI_diff[i] <- (thirty_minute_data$SVI[i] -  thirty_minute_data$SVI[i-1])/thirty_minute_data$SVI[i-1]
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$SVI_log_diff[i] <- log(thirty_minute_data$SVI[i] / thirty_minute_data$SVI[i-1])
}


# subsetting
thirty_minute_data <- thirty_minute_data[-1,]


save(thirty_minute_data,file="XBT_30minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
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
load(file = "XBT_15minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(fifteen_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(fifteen_minute_data)[i]
  nr_nas_1[i,2] <- nrow(fifteen_minute_data)
  nr_nas_1[i,3] <- sum(is.na(fifteen_minute_data[,i])) + sum(fifteen_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(fifteen_minute_data) <- c("time", "SVI", "price_all",
                                   "price_ba","price_bm", "price_bl",
                                   "price_sa","price_sm", "price_sl",
                                   "volume_all",
                                   "volume_ba","volume_bm","volume_bl",
                                   "volume_sa","volume_sm","volume_sl")

fifteen_minute_data <- na.locf(fifteen_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
fifteen_minute_data$return_price_all <-NA
fifteen_minute_data$logreturn_price_all <- NA
fifteen_minute_data$SVI_diff <- NA
fifteen_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$return_price_all[i] <- (fifteen_minute_data$price_all[i] -  fifteen_minute_data$price_all[i-1])/fifteen_minute_data$price_all[i-1]
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$logreturn_price_all[i] <- log(fifteen_minute_data$price_all[i] / fifteen_minute_data$price_all[i-1])
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$SVI_diff[i] <- (fifteen_minute_data$SVI[i] -  fifteen_minute_data$SVI[i-1])/fifteen_minute_data$SVI[i-1]
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$SVI_log_diff[i] <- log(fifteen_minute_data$SVI[i] / fifteen_minute_data$SVI[i-1])
}


# subsetting
fifteen_minute_data <- fifteen_minute_data[-1,]


save(fifteen_minute_data,file="XBT_15minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
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
load(file = "XBT_5minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(five_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(five_minute_data)[i]
  nr_nas_1[i,2] <- nrow(five_minute_data)
  nr_nas_1[i,3] <- sum(is.na(five_minute_data[,i])) + sum(five_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(five_minute_data) <- c("time", "SVI", "price_all",
                                "price_ba","price_bm", "price_bl",
                                "price_sa","price_sm", "price_sl",
                                "volume_all",
                                "volume_ba","volume_bm","volume_bl",
                                "volume_sa","volume_sm","volume_sl")

five_minute_data <- na.locf(five_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
five_minute_data$return_price_all <-NA
five_minute_data$logreturn_price_all <- NA
five_minute_data$SVI_diff <- NA
five_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(five_minute_data)){
  five_minute_data$return_price_all[i] <- (five_minute_data$price_all[i] -  five_minute_data$price_all[i-1])/five_minute_data$price_all[i-1]
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$logreturn_price_all[i] <- log(five_minute_data$price_all[i] / five_minute_data$price_all[i-1])
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$SVI_diff[i] <- (five_minute_data$SVI[i] -  five_minute_data$SVI[i-1])/five_minute_data$SVI[i-1]
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$SVI_log_diff[i] <- log(five_minute_data$SVI[i] / five_minute_data$SVI[i-1])
}


# subsetting
five_minute_data <- five_minute_data[-1,]


save(five_minute_data,file="XBT_5minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
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
load(file = "LTC_week.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(week_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(week_data)[i]
  nr_nas_1[i,2] <- nrow(week_data)
  nr_nas_1[i,3] <- sum(is.na(week_data[,i])) + sum(week_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(week_data) <- c("time", "SVI", "price_all",
                         "price_ba","price_bm", "price_bl",
                         "price_sa","price_sm", "price_sl",
                         "volume_all",
                         "volume_ba","volume_bm","volume_bl",
                         "volume_sa","volume_sm","volume_sl")

week_data <- na.locf(week_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
week_data$return_price_all <-NA
week_data$logreturn_price_all <- NA
week_data$SVI_diff <- NA
week_data$SVI_log_diff <- NA

for (i in 2:nrow(week_data)){
  week_data$return_price_all[i] <- (week_data$price_all[i] -  week_data$price_all[i-1])/week_data$price_all[i-1]
}

for (i in 2:nrow(week_data)){
  week_data$logreturn_price_all[i] <- log(week_data$price_all[i] / week_data$price_all[i-1])
}

for (i in 2:nrow(week_data)){
  week_data$SVI_diff[i] <- (week_data$SVI[i] -  week_data$SVI[i-1])/week_data$SVI[i-1]
}

for (i in 2:nrow(week_data)){
  week_data$SVI_log_diff[i] <- log(week_data$SVI[i] / week_data$SVI[i-1])
}


# subsetting
week_data <- week_data[-1,]


save(week_data,file="LTC_week_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
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
load(file = "LTC_day.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(day_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(day_data)[i]
  nr_nas_1[i,2] <- nrow(day_data)
  nr_nas_1[i,3] <- sum(is.na(day_data[,i])) + sum(day_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(day_data) <- c("time", "SVI", "price_all",
                        "price_ba","price_bm", "price_bl",
                        "price_sa","price_sm", "price_sl",
                        "volume_all",
                        "volume_ba","volume_bm","volume_bl",
                        "volume_sa","volume_sm","volume_sl")

day_data <- na.locf(day_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
day_data$return_price_all <-NA
day_data$logreturn_price_all <- NA
day_data$SVI_diff <- NA
day_data$SVI_log_diff <- NA

for (i in 2:nrow(day_data)){
  day_data$return_price_all[i] <- (day_data$price_all[i] -  day_data$price_all[i-1])/day_data$price_all[i-1]
}

for (i in 2:nrow(day_data)){
  day_data$logreturn_price_all[i] <- log(day_data$price_all[i] / day_data$price_all[i-1])
}

for (i in 2:nrow(day_data)){
  day_data$SVI_diff[i] <- (day_data$SVI[i] -  day_data$SVI[i-1])/day_data$SVI[i-1]
}

for (i in 2:nrow(day_data)){
  day_data$SVI_log_diff[i] <- log(day_data$SVI[i] / day_data$SVI[i-1])
}


# subsetting
day_data <- day_data[-1,]


save(day_data,file="LTC_day_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
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
load(file = "LTC_60minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(hour_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(hour_data)[i]
  nr_nas_1[i,2] <- nrow(hour_data)
  nr_nas_1[i,3] <- sum(is.na(hour_data[,i])) + sum(hour_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(hour_data) <- c("time", "SVI", "price_all",
                         "price_ba","price_bm", "price_bl",
                         "price_sa","price_sm", "price_sl",
                         "volume_all",
                         "volume_ba","volume_bm","volume_bl",
                         "volume_sa","volume_sm","volume_sl")

hour_data <- na.locf(hour_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
hour_data$return_price_all <-NA
hour_data$logreturn_price_all <- NA
hour_data$SVI_diff <- NA
hour_data$SVI_log_diff <- NA

for (i in 2:nrow(hour_data)){
  hour_data$return_price_all[i] <- (hour_data$price_all[i] -  hour_data$price_all[i-1])/hour_data$price_all[i-1]
}

for (i in 2:nrow(hour_data)){
  hour_data$logreturn_price_all[i] <- log(hour_data$price_all[i] / hour_data$price_all[i-1])
}

for (i in 2:nrow(hour_data)){
  hour_data$SVI_diff[i] <- (hour_data$SVI[i] -  hour_data$SVI[i-1])/hour_data$SVI[i-1]
}

for (i in 2:nrow(hour_data)){
  hour_data$SVI_log_diff[i] <- log(hour_data$SVI[i] / hour_data$SVI[i-1])
}


# subsetting
hour_data <- hour_data[-1,]


save(hour_data,file="LTC_60minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
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
load(file = "LTC_30minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(thirty_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(thirty_minute_data)[i]
  nr_nas_1[i,2] <- nrow(thirty_minute_data)
  nr_nas_1[i,3] <- sum(is.na(thirty_minute_data[,i])) + sum(thirty_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(thirty_minute_data) <- c("time", "SVI", "price_all",
                                  "price_ba","price_bm", "price_bl",
                                  "price_sa","price_sm", "price_sl",
                                  "volume_all",
                                  "volume_ba","volume_bm","volume_bl",
                                  "volume_sa","volume_sm","volume_sl")

thirty_minute_data <- na.locf(thirty_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
thirty_minute_data$return_price_all <-NA
thirty_minute_data$logreturn_price_all <- NA
thirty_minute_data$SVI_diff <- NA
thirty_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$return_price_all[i] <- (thirty_minute_data$price_all[i] -  thirty_minute_data$price_all[i-1])/thirty_minute_data$price_all[i-1]
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$logreturn_price_all[i] <- log(thirty_minute_data$price_all[i] / thirty_minute_data$price_all[i-1])
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$SVI_diff[i] <- (thirty_minute_data$SVI[i] -  thirty_minute_data$SVI[i-1])/thirty_minute_data$SVI[i-1]
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$SVI_log_diff[i] <- log(thirty_minute_data$SVI[i] / thirty_minute_data$SVI[i-1])
}


# subsetting
thirty_minute_data <- thirty_minute_data[-1,]


save(thirty_minute_data,file="LTC_30minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
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
load(file = "LTC_15minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(fifteen_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(fifteen_minute_data)[i]
  nr_nas_1[i,2] <- nrow(fifteen_minute_data)
  nr_nas_1[i,3] <- sum(is.na(fifteen_minute_data[,i])) + sum(fifteen_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(fifteen_minute_data) <- c("time", "SVI", "price_all",
                                   "price_ba","price_bm", "price_bl",
                                   "price_sa","price_sm", "price_sl",
                                   "volume_all",
                                   "volume_ba","volume_bm","volume_bl",
                                   "volume_sa","volume_sm","volume_sl")

fifteen_minute_data <- na.locf(fifteen_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
fifteen_minute_data$return_price_all <-NA
fifteen_minute_data$logreturn_price_all <- NA
fifteen_minute_data$SVI_diff <- NA
fifteen_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$return_price_all[i] <- (fifteen_minute_data$price_all[i] -  fifteen_minute_data$price_all[i-1])/fifteen_minute_data$price_all[i-1]
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$logreturn_price_all[i] <- log(fifteen_minute_data$price_all[i] / fifteen_minute_data$price_all[i-1])
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$SVI_diff[i] <- (fifteen_minute_data$SVI[i] -  fifteen_minute_data$SVI[i-1])/fifteen_minute_data$SVI[i-1]
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$SVI_log_diff[i] <- log(fifteen_minute_data$SVI[i] / fifteen_minute_data$SVI[i-1])
}


# subsetting
fifteen_minute_data <- fifteen_minute_data[-1,]


save(fifteen_minute_data,file="LTC_15minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
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
load(file = "LTC_5minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(five_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(five_minute_data)[i]
  nr_nas_1[i,2] <- nrow(five_minute_data)
  nr_nas_1[i,3] <- sum(is.na(five_minute_data[,i])) + sum(five_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(five_minute_data) <- c("time", "SVI", "price_all",
                                "price_ba","price_bm", "price_bl",
                                "price_sa","price_sm", "price_sl",
                                "volume_all",
                                "volume_ba","volume_bm","volume_bl",
                                "volume_sa","volume_sm","volume_sl")

five_minute_data <- na.locf(five_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
five_minute_data$return_price_all <-NA
five_minute_data$logreturn_price_all <- NA
five_minute_data$SVI_diff <- NA
five_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(five_minute_data)){
  five_minute_data$return_price_all[i] <- (five_minute_data$price_all[i] -  five_minute_data$price_all[i-1])/five_minute_data$price_all[i-1]
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$logreturn_price_all[i] <- log(five_minute_data$price_all[i] / five_minute_data$price_all[i-1])
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$SVI_diff[i] <- (five_minute_data$SVI[i] -  five_minute_data$SVI[i-1])/five_minute_data$SVI[i-1]
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$SVI_log_diff[i] <- log(five_minute_data$SVI[i] / five_minute_data$SVI[i-1])
}


# subsetting
five_minute_data <- five_minute_data[-1,]


save(five_minute_data,file="LTC_5minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
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
load(file = "LTC_1minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(minute_data)[i]
  nr_nas_1[i,2] <- nrow(minute_data)
  nr_nas_1[i,3] <- sum(is.na(minute_data[,i])) + sum(minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(minute_data) <- c("time", "SVI", "price_all",
                           "price_ba","price_bm", "price_bl",
                           "price_sa","price_sm", "price_sl",
                           "volume_all",
                           "volume_ba","volume_bm","volume_bl",
                           "volume_sa","volume_sm","volume_sl")

minute_data <- na.locf(minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
minute_data$return_price_all <-NA
minute_data$logreturn_price_all <- NA
minute_data$SVI_diff <- NA
minute_data$SVI_log_diff <- NA

for (i in 2:nrow(minute_data)){
  minute_data$return_price_all[i] <- (minute_data$price_all[i] -  minute_data$price_all[i-1])/minute_data$price_all[i-1]
}

for (i in 2:nrow(minute_data)){
  minute_data$logreturn_price_all[i] <- log(minute_data$price_all[i] / minute_data$price_all[i-1])
}

for (i in 2:nrow(minute_data)){
  minute_data$SVI_diff[i] <- (minute_data$SVI[i] -  minute_data$SVI[i-1])/minute_data$SVI[i-1]
}

for (i in 2:nrow(minute_data)){
  minute_data$SVI_log_diff[i] <- log(minute_data$SVI[i] / minute_data$SVI[i-1])
}


# subsetting
minute_data <- minute_data[-1,]


save(minute_data,file="LTC_1minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
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
load(file = "ETH_week.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(week_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(week_data)[i]
  nr_nas_1[i,2] <- nrow(week_data)
  nr_nas_1[i,3] <- sum(is.na(week_data[,i])) + sum(week_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(week_data) <- c("time", "SVI", "price_all",
                         "price_ba","price_bm", "price_bl",
                         "price_sa","price_sm", "price_sl",
                         "volume_all",
                         "volume_ba","volume_bm","volume_bl",
                         "volume_sa","volume_sm","volume_sl")

week_data <- na.locf(week_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
week_data$return_price_all <-NA
week_data$logreturn_price_all <- NA
week_data$SVI_diff <- NA
week_data$SVI_log_diff <- NA

for (i in 2:nrow(week_data)){
  week_data$return_price_all[i] <- (week_data$price_all[i] -  week_data$price_all[i-1])/week_data$price_all[i-1]
}

for (i in 2:nrow(week_data)){
  week_data$logreturn_price_all[i] <- log(week_data$price_all[i] / week_data$price_all[i-1])
}

for (i in 2:nrow(week_data)){
  week_data$SVI_diff[i] <- (week_data$SVI[i] -  week_data$SVI[i-1])/week_data$SVI[i-1]
}

for (i in 2:nrow(week_data)){
  week_data$SVI_log_diff[i] <- log(week_data$SVI[i] / week_data$SVI[i-1])
}


# subsetting
week_data <- week_data[-1,]


save(week_data,file="ETH_week_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
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
load(file = "ETH_day.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(day_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(day_data)[i]
  nr_nas_1[i,2] <- nrow(day_data)
  nr_nas_1[i,3] <- sum(is.na(day_data[,i])) + sum(day_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(day_data) <- c("time", "SVI", "price_all",
                        "price_ba","price_bm", "price_bl",
                        "price_sa","price_sm", "price_sl",
                        "volume_all",
                        "volume_ba","volume_bm","volume_bl",
                        "volume_sa","volume_sm","volume_sl")

day_data <- na.locf(day_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
day_data$return_price_all <-NA
day_data$logreturn_price_all <- NA
day_data$SVI_diff <- NA
day_data$SVI_log_diff <- NA

for (i in 2:nrow(day_data)){
  day_data$return_price_all[i] <- (day_data$price_all[i] -  day_data$price_all[i-1])/day_data$price_all[i-1]
}

for (i in 2:nrow(day_data)){
  day_data$logreturn_price_all[i] <- log(day_data$price_all[i] / day_data$price_all[i-1])
}

for (i in 2:nrow(day_data)){
  day_data$SVI_diff[i] <- (day_data$SVI[i] -  day_data$SVI[i-1])/day_data$SVI[i-1]
}

for (i in 2:nrow(day_data)){
  day_data$SVI_log_diff[i] <- log(day_data$SVI[i] / day_data$SVI[i-1])
}


# subsetting
day_data <- day_data[-1,]


save(day_data,file="ETH_day_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
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
load(file = "ETH_60minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(hour_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(hour_data)[i]
  nr_nas_1[i,2] <- nrow(hour_data)
  nr_nas_1[i,3] <- sum(is.na(hour_data[,i])) + sum(hour_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(hour_data) <- c("time", "SVI", "price_all",
                         "price_ba","price_bm", "price_bl",
                         "price_sa","price_sm", "price_sl",
                         "volume_all",
                         "volume_ba","volume_bm","volume_bl",
                         "volume_sa","volume_sm","volume_sl")

hour_data <- na.locf(hour_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
hour_data$return_price_all <-NA
hour_data$logreturn_price_all <- NA
hour_data$SVI_diff <- NA
hour_data$SVI_log_diff <- NA

for (i in 2:nrow(hour_data)){
  hour_data$return_price_all[i] <- (hour_data$price_all[i] -  hour_data$price_all[i-1])/hour_data$price_all[i-1]
}

for (i in 2:nrow(hour_data)){
  hour_data$logreturn_price_all[i] <- log(hour_data$price_all[i] / hour_data$price_all[i-1])
}

for (i in 2:nrow(hour_data)){
  hour_data$SVI_diff[i] <- (hour_data$SVI[i] -  hour_data$SVI[i-1])/hour_data$SVI[i-1]
}

for (i in 2:nrow(hour_data)){
  hour_data$SVI_log_diff[i] <- log(hour_data$SVI[i] / hour_data$SVI[i-1])
}


# subsetting
hour_data <- hour_data[-1,]


save(hour_data,file="ETH_60minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
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
load(file = "ETH_30minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(thirty_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(thirty_minute_data)[i]
  nr_nas_1[i,2] <- nrow(thirty_minute_data)
  nr_nas_1[i,3] <- sum(is.na(thirty_minute_data[,i])) + sum(thirty_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(thirty_minute_data) <- c("time", "SVI", "price_all",
                                  "price_ba","price_bm", "price_bl",
                                  "price_sa","price_sm", "price_sl",
                                  "volume_all",
                                  "volume_ba","volume_bm","volume_bl",
                                  "volume_sa","volume_sm","volume_sl")

thirty_minute_data <- na.locf(thirty_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
thirty_minute_data$return_price_all <-NA
thirty_minute_data$logreturn_price_all <- NA
thirty_minute_data$SVI_diff <- NA
thirty_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$return_price_all[i] <- (thirty_minute_data$price_all[i] -  thirty_minute_data$price_all[i-1])/thirty_minute_data$price_all[i-1]
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$logreturn_price_all[i] <- log(thirty_minute_data$price_all[i] / thirty_minute_data$price_all[i-1])
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$SVI_diff[i] <- (thirty_minute_data$SVI[i] -  thirty_minute_data$SVI[i-1])/thirty_minute_data$SVI[i-1]
}

for (i in 2:nrow(thirty_minute_data)){
  thirty_minute_data$SVI_log_diff[i] <- log(thirty_minute_data$SVI[i] / thirty_minute_data$SVI[i-1])
}


# subsetting
thirty_minute_data <- thirty_minute_data[-1,]


save(thirty_minute_data,file="ETH_30minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
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
load(file = "ETH_15minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(fifteen_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(fifteen_minute_data)[i]
  nr_nas_1[i,2] <- nrow(fifteen_minute_data)
  nr_nas_1[i,3] <- sum(is.na(fifteen_minute_data[,i])) + sum(fifteen_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(fifteen_minute_data) <- c("time", "SVI", "price_all",
                                   "price_ba","price_bm", "price_bl",
                                   "price_sa","price_sm", "price_sl",
                                   "volume_all",
                                   "volume_ba","volume_bm","volume_bl",
                                   "volume_sa","volume_sm","volume_sl")

fifteen_minute_data <- na.locf(fifteen_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
fifteen_minute_data$return_price_all <-NA
fifteen_minute_data$logreturn_price_all <- NA
fifteen_minute_data$SVI_diff <- NA
fifteen_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$return_price_all[i] <- (fifteen_minute_data$price_all[i] -  fifteen_minute_data$price_all[i-1])/fifteen_minute_data$price_all[i-1]
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$logreturn_price_all[i] <- log(fifteen_minute_data$price_all[i] / fifteen_minute_data$price_all[i-1])
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$SVI_diff[i] <- (fifteen_minute_data$SVI[i] -  fifteen_minute_data$SVI[i-1])/fifteen_minute_data$SVI[i-1]
}

for (i in 2:nrow(fifteen_minute_data)){
  fifteen_minute_data$SVI_log_diff[i] <- log(fifteen_minute_data$SVI[i] / fifteen_minute_data$SVI[i-1])
}


# subsetting
fifteen_minute_data <- fifteen_minute_data[-1,]


save(fifteen_minute_data,file="ETH_15minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
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
load(file = "ETH_5minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(five_minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(five_minute_data)[i]
  nr_nas_1[i,2] <- nrow(five_minute_data)
  nr_nas_1[i,3] <- sum(is.na(five_minute_data[,i])) + sum(five_minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(five_minute_data) <- c("time", "SVI", "price_all",
                                "price_ba","price_bm", "price_bl",
                                "price_sa","price_sm", "price_sl",
                                "volume_all",
                                "volume_ba","volume_bm","volume_bl",
                                "volume_sa","volume_sm","volume_sl")

five_minute_data <- na.locf(five_minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
five_minute_data$return_price_all <-NA
five_minute_data$logreturn_price_all <- NA
five_minute_data$SVI_diff <- NA
five_minute_data$SVI_log_diff <- NA

for (i in 2:nrow(five_minute_data)){
  five_minute_data$return_price_all[i] <- (five_minute_data$price_all[i] -  five_minute_data$price_all[i-1])/five_minute_data$price_all[i-1]
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$logreturn_price_all[i] <- log(five_minute_data$price_all[i] / five_minute_data$price_all[i-1])
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$SVI_diff[i] <- (five_minute_data$SVI[i] -  five_minute_data$SVI[i-1])/five_minute_data$SVI[i-1]
}

for (i in 2:nrow(five_minute_data)){
  five_minute_data$SVI_log_diff[i] <- log(five_minute_data$SVI[i] / five_minute_data$SVI[i-1])
}


# subsetting
five_minute_data <- five_minute_data[-1,]


save(five_minute_data,file="ETH_5minute_returns.Rdata")

setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
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
load(file = "ETH_1minute.RData")

#1) filling NAs - taking last valid price - zoo na.locf
nr_nas_1 <- as.data.frame(matrix(1, nrow = ncol(minute_data), ncol = 3))
for(i in 1:16){
  nr_nas_1[i,1] <- colnames(minute_data)[i]
  nr_nas_1[i,2] <- nrow(minute_data)
  nr_nas_1[i,3] <- sum(is.na(minute_data[,i])) + sum(minute_data[,i] == 0, na.rm = TRUE)
}

colnames(nr_nas_1) <- c("variable", "#observations", "#NAs")

colnames(minute_data) <- c("time", "SVI", "price_all",
                           "price_ba","price_bm", "price_bl",
                           "price_sa","price_sm", "price_sl",
                           "volume_all",
                           "volume_ba","volume_bm","volume_bl",
                           "volume_sa","volume_sm","volume_sl")

minute_data <- na.locf(minute_data, na.rm = TRUE)

#2 ) creating returns and log returns and their squares
minute_data$return_price_all <-NA
minute_data$logreturn_price_all <- NA
minute_data$SVI_diff <- NA
minute_data$SVI_log_diff <- NA

for (i in 2:nrow(minute_data)){
  minute_data$return_price_all[i] <- (minute_data$price_all[i] -  minute_data$price_all[i-1])/minute_data$price_all[i-1]
}

for (i in 2:nrow(minute_data)){
  minute_data$logreturn_price_all[i] <- log(minute_data$price_all[i] / minute_data$price_all[i-1])
}

for (i in 2:nrow(minute_data)){
  minute_data$SVI_diff[i] <- (minute_data$SVI[i] -  minute_data$SVI[i-1])/minute_data$SVI[i-1]
}

for (i in 2:nrow(minute_data)){
  minute_data$SVI_log_diff[i] <- log(minute_data$SVI[i] / minute_data$SVI[i-1])
}


# subsetting
minute_data <- minute_data[-1,]


save(minute_data,file="ETH_1minute_returns.Rdata")


