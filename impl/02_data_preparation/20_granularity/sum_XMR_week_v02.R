setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
setwd(wd) 
rm(list=ls())


#loading data
load(file = "XMR_1minute.RData")

#rounding to nearest minute (up)
prev <- (minute_data$time %/% 604800 +1)* 604800


#generating the data structure
week_data <- as.data.frame(matrix(1, nrow = length(unique(prev)), ncol = 16))
week_data[,1] <- as.data.frame(unique(prev))
colnames(week_data) <- c("time", "SVI", "price_all",
                           "price_ba","price_bm", "price_bl",
                           "price_sa","price_sm", "price_sl",
                           "volume_all",
                           "volume_ba","volume_bm","volume_bl",
                           "volume_sa","volume_sm","volume_sl")
#looping to sum it

week_data[,2] <- aggregate(minute_data[,2], list(prev), sum, na.rm = TRUE)[,2] # sum SVI

for(i in 1:7){
  week_data[,i+2] <- aggregate(as.numeric(minute_data[,i+2])*as.numeric(minute_data[,i+9]) , list(prev), sum, na.rm = TRUE)[,2] # sum (price * volume)
  week_data[,i+9] <- aggregate(as.numeric(minute_data[,i+9]) , list(prev), sum, na.rm = TRUE)[,2] # sum (volume)
  week_data[,i+2] <- as.numeric(week_data[,i+2]) / as.numeric(week_data[,i+9]) # sum (price * volume) / sum (volume)
}

ggplot(week_data, aes(week_data$time)) +                                      # basic graphical object
  geom_line(aes(y=week_data$SVI), colour="red") +             # first layer
  geom_line(aes(y=week_data$price_all*4000), colour="green")

week_data <- week_data

save(week_data, file="XMR_week.RData")



