rm(list=ls())
base_wd <- "C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec" # thats for me to get to proprer level
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/CRIX", sep = "")
setwd(wd) 

library(plyr)
library(ggplot2)

#loading data
load(file = "CRIX_15minute.RData")

# *** XBT **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd)
load(file = "XBT_15minute.RData")

# *** Merging the databases

for (i in 1:nrow(fifteen_minute_data)) {
  fifteen_minute_data$crix[i] <- CRIX_15minute_data$value[match(fifteen_minute_data$time[i], CRIX_15minute_data$timestamp)]
  print(paste(i, "assigning"))
}

save(fifteen_minute_data, file="XBT_15minute_CRIX.RData")

# *** ETH **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
setwd(wd)
load(file = "ETH_15minute.RData")

# *** Merging the databases

for (i in 1:nrow(fifteen_minute_data)) {
  fifteen_minute_data$crix[i] <- CRIX$price[match(fifteen_minute_data$time[i], CRIX$round_unix)]
  print(paste(i, "assigning"))
}
save(fifteen_minute_data, file="ETH_15minute_CRIX.RData")

# *** LTC **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
setwd(wd)
load(file = "LTC_15minute.RData")

# *** Merging the databases

for (i in 1:nrow(fifteen_minute_data)) {
  fifteen_minute_data$crix[i] <- CRIX$price[match(fifteen_minute_data$time[i], CRIX$round_unix)]
  print(paste(i, "assigning"))
}
save(fifteen_minute_data, file="LTC_15minute_CRIX.RData")

# *** XMR **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
setwd(wd)
load(file = "XMR_15minute.RData")

# *** Merging the databases

for (i in 1:nrow(fifteen_minute_data)) {
  fifteen_minute_data$crix[i] <- CRIX$price[match(fifteen_minute_data$time[i], CRIX$round_unix)]
  print(paste(i, "assigning"))
}
save(fifteen_minute_data, file="XMR_15minute_CRIX.RData")