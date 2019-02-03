rm(list=ls())
base_wd <- "C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec" # thats for me to get to proprer level
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/CRIX", sep = "")
setwd(wd) 

library(plyr)
library(ggplot2)

#loading data
load(file = "CRIX_30minute.RData")


# *** XBT **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd)
load(file = "XBT_30minute.RData")

# *** Merging the databases
thirty_minute_data$crix <- 0

for (i in 1:nrow(thirty_minute_data)) {
  thirty_minute_data$crix[i] <- CRIX_30minute_data$Index[match(thirty_minute_data$time[i], CRIX_30minute_data$timestamp)]
  print(paste(i, "assigning"))
}
save(thirty_minute_data, file="XBT_30minute_CRIX.RData")

# *** ETH **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
setwd(wd)
load(file = "ETH_30minute.RData")

# *** Merging the databases
thirty_minute_data$crix <- 0

for (i in 1:nrow(thirty_minute_data)) {
  thirty_minute_data$crix[i] <- CRIX_30minute_data$Index[match(thirty_minute_data$time[i], CRIX_30minute_data$timestamp)]
  print(paste(i, "assigning"))
}
save(thirty_minute_data, file="ETH_30minute_CRIX.RData")

# *** LTC **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
setwd(wd)
load(file = "LTC_30minute.RData")

# *** Merging the databases
thirty_minute_data$crix <- 0

for (i in 1:nrow(thirty_minute_data)) {
  thirty_minute_data$crix[i] <- CRIX_30minute_data$Index[match(thirty_minute_data$time[i], CRIX_30minute_data$timestamp)]
  print(paste(i, "assigning"))
}
save(thirty_minute_data, file="LTC_30minute_CRIX.RData")

# *** XMR **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
setwd(wd)
load(file = "XMR_30minute.RData")

# *** Merging the databases

thirty_minute_data$crix <- 0

for (i in 1:nrow(thirty_minute_data)) {
  thirty_minute_data$crix[i] <- CRIX_30minute_data$Index[match(thirty_minute_data$time[i], CRIX_30minute_data$timestamp)]
  print(paste(i, "assigning"))
}
save(thirty_minute_data, file="XMR_30minute_CRIX.RData")
plot(thirty_minute_data$crix)
sum(is.na(thirty_minute_data$crix))/length(thirty_minute_data$crix)