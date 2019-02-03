setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/final_sample/Google/monero", sep = "")
setwd(wd) 
rm(list=ls())

library(plyr)
library(ggplot2)



#loading data
load(file = "monero_SVI.RData")
colnames(p1) <- c("time", "SVI")

# *** turning it to unixtime
p1$time <- as.numeric(as.POSIXct(p1$time, 'GMT'))

# *** loading prices
wd <- getwd()
wd <- substr(wd, start = 1, stop = nchar(wd)-14)
wd <- paste(wd, "/Kraken", sep = "")
setwd(wd)
load(file = "XXMRZEUR.RData")

# *** trasnforming into wide data
prices <- as.data.frame(vector(mode = "numeric",length= nrow(XXMRZEUR)))
colnames(prices) <- "time"
prices <- XXMRZEUR[-1,-6]

# **** preparing segmented data
temp <- as.data.frame(matrix(1, nrow = nrow(prices), ncol = 3))
temp <- prices[prices$`buy/sell`=='b',]
prices <- merge(prices, temp[,1:3], by = "Timestamp", all.x = TRUE)

temp <- prices[prices$`buy/sell`=='s',]
prices <- merge(prices, temp[,1:3], by = "Timestamp", all = TRUE)

temp <- prices[prices$`buy/sell`=='b',]
temp <- temp[temp$`market/limit`== "m", ]
prices <- merge(prices, temp[,1:3], by = "Timestamp", all = TRUE)

temp <- prices[prices$`buy/sell`=='b',]
temp <- temp[temp$`market/limit`== "l", ]
prices <- merge(prices, temp[,1:3], by = "Timestamp", all = TRUE)

temp <- prices[prices$`buy/sell`=='s',]
temp <- temp[temp$`market/limit`== "m", ]
prices <- merge(prices, temp[,1:3], by = "Timestamp", all = TRUE)

temp <- prices[prices$`buy/sell`=='s',]
temp <- temp[temp$`market/limit`== "l", ]
prices <- merge(prices, temp[,1:3], by = "Timestamp", all = TRUE)

colnames(prices) <- c("timestamp","price_all","volume_all",
                      "b/s", "m/l", 
                      "price_ba", "volume_ba",
                      "price_bm", "volume_bm",
                      "price_bl", "volume_bl",
                      "price_sa", "volume_sa",
                      "price_sm","volume_sm",
                      "price_sl","volume_sl")


#rounding to nearest minute (up)
prev <- (prices$timestamp %/% 60 +1)* 60
prices$time_min <- prev
prices <- prices[,c("timestamp","time_min", "price_all",
                    "price_ba","price_bm", "price_bl",
                    "price_sa","price_sm", "price_sl",
                    "volume_all",
                    "volume_ba","volume_bm","volume_bl",
                    "volume_sa","volume_sm","volume_sl")]

#generating the data structure
temp_minute_data <- as.data.frame(matrix(1, nrow = length(p1$time), ncol = 15))
colnames(temp_minute_data) <- c("time_min", "price_all",
                           "price_ba","price_bm", "price_bl",
                           "price_sa","price_sm", "price_sl",
                           "volume_all",
                           "volume_ba","volume_bm","volume_bl",
                           "volume_sa","volume_sm","volume_sl")

temp_minute_data[,1] <- unique(prices$time_min)   #timestamp

for(i in 1:7){
  temp_minute_data[,i+1] <- aggregate(prices[,i+2] * prices[,i+9] , list(prices$time_min), sum, na.rm = TRUE)[,2] # sum (price * volume)
  temp_minute_data[,i+8] <- aggregate(prices[,i+9] , list(prices$time_min), sum, na.rm = TRUE)[,2] # sum (volume)
  temp_minute_data[,i+1] <- temp_minute_data[,i+1] / temp_minute_data[,i+8] # sum (price * volume) / sum (volume)
}


#looping to match values of trading with search values
for (i in 1:nrow(minute_data)) {
  minute_data$search[i] <- p1$SVI[match(minute_data$time_min[i], p1$time)]
}

#subsetting with respect to shorter data - need to do manually, maybe some NA find function
minute_data <- subset(tail(minute_data,nrow(minute_data) - sum(is.na(minute_data$search))))

wd <- getwd()
wd <- substr(wd, start = 1, stop = nchar(wd)-nchar("/Price"))
setwd(wd)

save(minute_data, file="minute_data_bc.RData")

require(ggplot2)

ggplot(minute_data, aes(minute_data$time_min)) +                                      # basic graphical object
  geom_line(aes(y=minute_data$search*30 + 1500), colour="red") +             # first layer
  geom_line(aes(y=minute_data$price_all), colour="green")


#additional things like rolling avg, log returns or so
#minute_data$Avg_Search <- rolling_avg(minute_data$Search)
#minute_data$Avg_Volume <- rolling_avg(minute_data$Volume)

#for (i in 2:nrow(minute_data)){
#minute_data$Price_logret[i] <- log(minute_data$Price[i]/minute_data$Price[i-1])
#minute_data$Search_logret[i] <- log(minute_data$Search[i]/minute_data$Search[i-1])
#minute_data$Avg_Search_logret[i] <- log(minute_data$Avg_Search[i]/minute_data$Avg_Search[i-1])
#}
