setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
setwd(wd) 
rm(list=ls())


#loading data
load(file = "ETH_1minute.RData")

#rounding to nearest minute (up)
prev <- (minute_data$time %/% 3600 +1)* 3600


#generating the data structure
hour_data <- as.data.frame(matrix(1, nrow = length(unique(prev)), ncol = 16))
hour_data[,1] <- as.data.frame(unique(prev))
colnames(hour_data) <- c("time", "SVI", "price_all",
                           "price_ba","price_bm", "price_bl",
                           "price_sa","price_sm", "price_sl",
                           "volume_all",
                           "volume_ba","volume_bm","volume_bl",
                           "volume_sa","volume_sm","volume_sl")
#looping to sum it

hour_data[,2] <- aggregate(minute_data[,2], list(prev), sum, na.rm = TRUE)[,2] # sum SVI

for(i in 1:7){
  hour_data[,i+2] <- aggregate(as.numeric(minute_data[,i+2])*as.numeric(minute_data[,i+9]) , list(prev), sum, na.rm = TRUE)[,2] # sum (price * volume)
  hour_data[,i+9] <- aggregate(as.numeric(minute_data[,i+9]) , list(prev), sum, na.rm = TRUE)[,2] # sum (volume)
  hour_data[,i+2] <- as.numeric(hour_data[,i+2]) / as.numeric(hour_data[,i+9]) # sum (price * volume) / sum (volume)
}

ggplot(hour_data, aes(hour_data$time)) +                                      # basic graphical object
  geom_line(aes(y=hour_data$SVI), colour="red") +             # first layer
  geom_line(aes(y=hour_data$price_all*10), colour="green")

save(hour_data, file="ETH_60minute.RData")



