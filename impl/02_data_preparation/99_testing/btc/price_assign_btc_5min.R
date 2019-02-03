setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/test_sample/bitcoin/2017_08_18_00-2017_09_29_21", sep = "")
setwd(wd) 
rm(list=ls())

#loading data
p1 <- read.csv("output.csv",colClasses=c("NULL",NA,NA))
colnames(p1) <- c("time", "SVI")

# *** turning it to unixtime
p1$time <- as.numeric(as.POSIXct(p1$time, 'GMT'))

# *** loading prices
wd <- getwd()
wd <- substr(wd, start = 1, stop = nchar(wd)-28)
wd <- paste(wd, "/Price", sep = "")
setwd(wd)
load(file = "XXBTZEUR.RData")

# *** trasnforming into wide data
prices <- as.data.frame(vector(mode = "numeric",length= nrow(XXBTZEUR)))
colnames(prices) <- "time"
prices <- XXBTZEUR[,-6]

# **** preparing segmented data
temp <- prices[prices$`buy/sell`=='b',]
prices <- merge(prices, temp[,1:3], by = "timestamp", all = TRUE)

temp <- prices[prices$`buy/sell`=='s',]
prices <- merge(prices, temp[,1:3], by = "timestamp", all = TRUE)

temp <- prices[prices$`buy/sell`=='b',]
temp <- temp[temp$`market/limit`== "m", ]
prices <- merge(prices, temp[,1:3], by = "timestamp", all = TRUE)

temp <- prices[prices$`buy/sell`=='b',]
temp <- temp[temp$`market/limit`== "l", ]
prices <- merge(prices, temp[,1:3], by = "timestamp", all = TRUE)

temp <- prices[prices$`buy/sell`=='s',]
temp <- temp[temp$`market/limit`== "m", ]
prices <- merge(prices, temp[,1:3], by = "timestamp", all = TRUE)

temp <- prices[prices$`buy/sell`=='s',]
temp <- temp[temp$`market/limit`== "l", ]
prices <- merge(prices, temp[,1:3], by = "timestamp", all = TRUE)

colnames(prices) <- c("timestamp","price_all","volume_all",
                      "b/s", "m/l", 
                      "price_ba", "volume_ba",
                      "price_bm", "volume_bm",
                      "price_bl", "volume_bl",
                      "price_sa", "volume_sa",
                      "price_sm","volume_sm",
                      "price_sl","volume_sl")


#rounding to nearest minute (up)
prev <- (prices$timestamp %/% 300 +1)* 300
prices$time_min <- prev
prices <- prices[,c("timestamp","time_min", "price_all",
                    "price_ba","price_bm", "price_bl",
                    "price_sa","price_sm", "price_sl",
                    "volume_all",
                    "volume_ba","volume_bm","volume_bl",
                    "volume_sa","volume_sm","volume_sl")]

#generating the data structure
minute_data <- as.data.frame(matrix(1, nrow = length(c(unique(prices$time_min))), ncol = 15))
colnames(minute_data) <- c("time_min", "price_all",
                           "price_ba","price_bm", "price_bl",
                           "price_sa","price_sm", "price_sl",
                           "volume_all",
                           "volume_ba","volume_bm","volume_bl",
                           "volume_sa","volume_sm","volume_sl")

minute_data[,1] <- unique(prices[,2])   #timestamp

for(i in 1:7){
  minute_data[,i+1] <- aggregate(prices[,i+2]*prices[,i+9] , list(prices$time_min), sum, na.rm = TRUE)[,2] # sum (price * volume)
  minute_data[,i+8] <- aggregate(prices[,i+9] , list(prices$time_min), sum, na.rm = TRUE)[,2] # sum (volume)
  minute_data[,i+1] <- minute_data[,i+1] / minute_data[,i+8] # sum (price * volume) / sum (volume)
}


#looping to match values of trading with search values
p1$SVI[5:nrow(p1)] <- rollsum(p1$SVI, k = 5, align = "right")
for (i in 1:nrow(minute_data)) {
  minute_data$search[i] <- p1$SVI[match(minute_data$time_min[i], p1$time)]
}

#subsetting with respect to shorter data - need to do manually, maybe some NA find function
minute_data <- subset(tail(minute_data,nrow(minute_data) - sum(is.na(minute_data$search))))

wd <- getwd()
wd <- substr(wd, start = 1, stop = nchar(wd)-nchar("/Price"))
setwd(wd)

five_minute_data <- minute_data
save(five_minute_data, file="five_minute_data_bc.RData")



