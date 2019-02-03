setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Google/litecoin", sep = "")
setwd(wd) 
rm(list=ls())

library(plyr)
library(ggplot2)



#loading data
load(file = "litecoin_SVI.RData")
colnames(p1) <- c("time", "SVI")

# *** turning it to unixtime
p1$time <- as.numeric(as.POSIXct(p1$time, 'GMT'))

# *** loading prices
wd <- getwd()
wd <- substr(wd, start = 1, stop = nchar(wd)-15)
wd <- paste(wd, "/Kraken/LTC", sep = "")
setwd(wd)
load(file = "XLTCZEUR.RData")

# *** trasnforming into wide data
prices <- as.data.frame(vector(mode = "numeric",length= nrow(XLTCZEUR)))
colnames(prices) <- "time"
prices <- XLTCZEUR[-1,-6]

# **** preparing segmented data
temp_b <- prices[prices$`buy/sell`=='b',]
#prices <- merge(prices, temp[,1:3], by = "Timestamp", all = TRUE)


temp_s <- prices[prices$`buy/sell`=='s',]
#prices <- merge(prices, temp[,1:3], by = "Timestamp", all = TRUE)


temp_bm <- prices[prices$`buy/sell`=='b',]
temp_bm <- temp_bm[temp_bm$`market/limit`== "m", ]
#prices <- merge(prices, temp[,1:3], by = "Timestamp", all = TRUE)


temp_bl <- prices[prices$`buy/sell`=='b',]
temp_bl <- temp_bl[temp_bl$`market/limit`== "l", ]
#prices <- merge(prices, temp[,1:3], by = "Timestamp", all = TRUE)

temp_sm <- prices[prices$`buy/sell`=='s',]
temp_sm <- temp_sm[temp_sm$`market/limit`== "m", ]
#prices <- merge(prices, temp[,1:3], by = "Timestamp", all = TRUE)

temp_sl <- prices[prices$`buy/sell`=='s',]
temp_sl <- temp_sl[temp_sl$`market/limit`== "l", ]
#prices <- merge(prices, temp[,1:3], by = "Timestamp", all = TRUE)

#rounding to nearest minute (up)
prev <- (prices$Timestamp %/% 60 +1)* 60
prices$time_min <- prev

prev <- (temp_b$Timestamp %/% 60 +1)* 60
temp_b$time_min <- prev

prev <- (temp_s$Timestamp %/% 60 +1)* 60
temp_s$time_min <- prev

prev <- (temp_bm$Timestamp %/% 60 +1)* 60
temp_bm$time_min <- prev

prev <- (temp_bl$Timestamp %/% 60 +1)* 60
temp_bl$time_min <- prev

prev <- (temp_sm$Timestamp %/% 60 +1)* 60
temp_sm$time_min <- prev

prev <- (temp_sl$Timestamp %/% 60 +1)* 60
temp_sl$time_min <- prev

#agregating by minutes
min_prices <- as.data.frame(unique(prices$time_min))
min_prices[,2] <- aggregate(as.numeric(prices[,1])*as.numeric(prices[,2]), list(prices$time_min), sum, na.rm = TRUE)[,2]
min_prices[,3] <- aggregate(as.numeric(prices[,2]) , list(prices$time_min), sum, na.rm = TRUE)[,2] # sum (volume)
min_prices[,2] <- min_prices[,2] / min_prices[,3] # sum (price * volume) / sum (volume)
colnames(min_prices) <- c("time","price","volume")

min_temp_b <- as.data.frame(unique(temp_b$time_min))
min_temp_b[,2] <- aggregate(as.numeric(temp_b[,1])*as.numeric(temp_b[,2]), list(temp_b$time_min), sum, na.rm = TRUE)[,2]
min_temp_b[,3] <- aggregate(as.numeric(temp_b[,2]) , list(temp_b$time_min), sum, na.rm = TRUE)[,2] # sum (volume)
min_temp_b[,2] <- min_temp_b[,2] / min_temp_b[,3] # sum (price * volume) / sum (volume)
colnames(min_temp_b) <- c("time","price","volume")

min_temp_s <- as.data.frame(unique(temp_s$time_min))
min_temp_s[,2] <- aggregate(as.numeric(temp_s[,1])*as.numeric(temp_s[,2]), list(temp_s$time_min), sum, na.rm = TRUE)[,2]
min_temp_s[,3] <- aggregate(as.numeric(temp_s[,2]) , list(temp_s$time_min), sum, na.rm = TRUE)[,2] # sum (volume)
min_temp_s[,2] <- min_temp_s[,2] / min_temp_s[,3] # sum (price * volume) / sum (volume)
colnames(min_temp_s) <- c("time","price","volume")

min_temp_bm <- as.data.frame(unique(temp_bm$time_min))
min_temp_bm[,2] <- aggregate(as.numeric(temp_bm[,1])*as.numeric(temp_bm[,2]), list(temp_bm$time_min), sum, na.rm = TRUE)[,2]
min_temp_bm[,3] <- aggregate(as.numeric(temp_bm[,2]) , list(temp_bm$time_min), sum, na.rm = TRUE)[,2] # sum (volume)
min_temp_bm[,2] <- min_temp_bm[,2] / min_temp_bm[,3] # sum (price * volume) / sum (volume)
colnames(min_temp_bm) <- c("time","price","volume")

min_temp_bl <- as.data.frame(unique(temp_bl$time_min))
min_temp_bl[,2] <- aggregate(as.numeric(temp_bl[,1])*as.numeric(temp_bl[,2]), list(temp_bl$time_min), sum, na.rm = TRUE)[,2]
min_temp_bl[,3] <- aggregate(as.numeric(temp_bl[,2]) , list(temp_bl$time_min), sum, na.rm = TRUE)[,2] # sum (volume)
min_temp_bl[,2] <- min_temp_bl[,2] / min_temp_bl[,3] # sum (price * volume) / sum (volume)
colnames(min_temp_bl) <- c("time","price","volume")

min_temp_sm <- as.data.frame(unique(temp_sm$time_min))
min_temp_sm[,2] <- aggregate(as.numeric(temp_sm[,1])*as.numeric(temp_sm[,2]), list(temp_sm$time_min), sum, na.rm = TRUE)[,2]
min_temp_sm[,3] <- aggregate(as.numeric(temp_sm[,2]) , list(temp_sm$time_min), sum, na.rm = TRUE)[,2] # sum (volume)
min_temp_sm[,2] <- min_temp_sm[,2] / min_temp_sm[,3] # sum (price * volume) / sum (volume)
colnames(min_temp_sm) <- c("time","price","volume")

min_temp_sl <- as.data.frame(unique(temp_sl$time_min))
min_temp_sl[,2] <- aggregate(as.numeric(temp_sl[,1])*as.numeric(temp_sl[,2]), list(temp_sl$time_min), sum, na.rm = TRUE)[,2]
min_temp_sl[,3] <- aggregate(as.numeric(temp_sl[,2]) , list(temp_sl$time_min), sum, na.rm = TRUE)[,2] # sum (volume)
min_temp_sl[,2] <- min_temp_sl[,2] / min_temp_sl[,3] # sum (price * volume) / sum (volume)
colnames(min_temp_sl) <- c("time","price","volume")

minute_data <- p1
minute_data <- merge(minute_data, min_prices[,1:3], by = "time", all = TRUE)
colnames(minute_data) <- c("time","SVI","price_all","volume_all")

minute_data <- merge(minute_data, min_temp_b[,1:3], by = "time", all = TRUE)
colnames(minute_data) <- c("time","SVI","price_all","volume_all",  "price_ba", "volume_ba")

minute_data <- merge(minute_data, min_temp_bm[,1:3], by = "time", all = TRUE)
colnames(minute_data) <- c("time","SVI","price_all","volume_all",  "price_ba", "volume_ba","price_bm", "volume_bm")

minute_data <- merge(minute_data, min_temp_bl[,1:3], by = "time", all = TRUE)
colnames(minute_data) <- c("time","SVI","price_all","volume_all",  "price_ba", "volume_ba","price_bm", "volume_bm","price_bl", "volume_bl")

minute_data <- merge(minute_data, min_temp_s[,1:3], by = "time", all = TRUE)
colnames(minute_data) <- c("time","SVI","price_all","volume_all",  "price_ba", "volume_ba","price_bm", "volume_bm","price_bl", "volume_bl",
                           "price_sa", "volume_sa")

minute_data <- merge(minute_data, min_temp_sm[,1:3], by = "time", all = TRUE)
colnames(minute_data) <- c("time","SVI","price_all","volume_all",  "price_ba", "volume_ba","price_bm", "volume_bm","price_bl", "volume_bl",
                           "price_sa", "volume_sa","price_sm","volume_sm")

minute_data <- merge(minute_data, min_temp_sl[,1:3], by = "time", all = TRUE)
colnames(minute_data) <- c("time","SVI","price_all","volume_all",  "price_ba", "volume_ba","price_bm", "volume_bm","price_bl", "volume_bl",
                           "price_sa", "volume_sa","price_sm","volume_sm", "price_sl","volume_sl")

#subsetting with respect to shorter data - need to do manually, maybe some NA find function
minute_data <- subset(minute_data, minute_data$time < 1529629201)
minute_data <- subset(minute_data, minute_data$time > 1498003259)

minute_data <- minute_data[, c("time", "SVI", "price_all",
                               "price_ba","price_bm", "price_bl",
                               "price_sa","price_sm", "price_sl",
                               "volume_all",
                               "volume_ba","volume_bm","volume_bl",
                               "volume_sa","volume_sm","volume_sl")]

save(minute_data, file="LTC_1minute.RData")

require(ggplot2)

ggplot(minute_data, aes(minute_data$time)) +                                      # basic graphical object
  geom_line(aes(y=minute_data$SVI), colour="red") +             # first layer
  geom_line(aes(y=minute_data$price_all), colour="green")


#additional things like rolling avg, log returns or so
#minute_data$Avg_Search <- rolling_avg(minute_data$Search)
#minute_data$Avg_Volume <- rolling_avg(minute_data$Volume)

#for (i in 2:nrow(minute_data)){
#minute_data$Price_logret[i] <- log(minute_data$Price[i]/minute_data$Price[i-1])
#minute_data$Search_logret[i] <- log(minute_data$Search[i]/minute_data$Search[i-1])
#minute_data$Avg_Search_logret[i] <- log(minute_data$Avg_Search[i]/minute_data$Avg_Search[i-1])
#}
