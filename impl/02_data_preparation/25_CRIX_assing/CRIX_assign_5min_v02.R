rm(list=ls())
base_wd <- "C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec" # thats for me to get to proprer level
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/CRIX", sep = "")
setwd(wd) 

library(plyr)
library(ggplot2)

#loading data
load(file = "CRIX_5minute.RData")


# *** XBT **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd)
load(file = "XBT_5minute.RData")

# *** Merging the databases
five_minute_data$crix <- 0

for (i in 1:nrow(five_minute_data)) {
  five_minute_data$crix[i] <- CRIX_5minute_data$Index[match(five_minute_data$time[i], CRIX_5minute_data$timestamp)]
  print(paste(i, "assigning"))
}
save(five_minute_data, file="XBT_5minute_CRIX.RData")

# *** ETH **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
setwd(wd)
load(file = "ETH_5minute.RData")

# *** Merging the databases
five_minute_data$crix <- 0

for (i in 1:nrow(five_minute_data)) {
  five_minute_data$crix[i] <- CRIX_5minute_data$Index[match(five_minute_data$time[i], CRIX_5minute_data$timestamp)]
  print(paste(i, "assigning"))
}
save(five_minute_data, file="ETH_5minute_CRIX.RData")

# *** LTC **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
setwd(wd)
load(file = "LTC_5minute.RData")

# *** Merging the databases
five_minute_data$crix <- 0

for (i in 1:nrow(five_minute_data)) {
  five_minute_data$crix[i] <- CRIX_5minute_data$Index[match(five_minute_data$time[i], CRIX_5minute_data$timestamp)]
  print(paste(i, "assigning"))
}
save(five_minute_data, file="LTC_5minute_CRIX.RData")

# *** XMR **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
setwd(wd)
load(file = "XMR_5minute.RData")

# *** Merging the databases
five_minute_data$crix <- 0

for (i in 1:nrow(five_minute_data)) {
  five_minute_data$crix[i] <- CRIX_5minute_data$Index[match(five_minute_data$time[i], CRIX_5minute_data$timestamp)]
  print(paste(i, "assigning"))
}
save(five_minute_data, file="XMR_5minute_CRIX.RData")
plot(five_minute_data$crix)
sum(is.na(five_minute_data$crix))/length(five_minute_data$crix)