rm(list=ls())
base_wd <- "C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec" # thats for me to get to proprer level
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/CRIX", sep = "")
setwd(wd) 

library(plyr)
library(ggplot2)

#loading data
load(file = "CRIX_60minute.RData")


# *** XBT **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd)
load(file = "XBT_60minute.RData")

# *** Merging the databases

for (i in 1:nrow(hour_data)) {
  hour_data$crix[i] <- CRIX$price[match(hour_data$time[i], CRIX$round_unix)]
  print(paste(i, "assigning"))
}
save(hour_data, file="XBT_60minute_CRIX.RData")

# *** ETH **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
setwd(wd)
load(file = "ETH_60minute.RData")

# *** Merging the databases

for (i in 1:nrow(hour_data)) {
  hour_data$crix[i] <- CRIX$price[match(hour_data$time[i], CRIX$round_unix)]
  print(paste(i, "assigning"))
}
save(hour_data, file="ETH_60minute_CRIX.RData")

# *** LTC **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
setwd(wd)
load(file = "LTC_60minute.RData")

# *** Merging the databases

for (i in 1:nrow(hour_data)) {
  hour_data$crix[i] <- CRIX$price[match(hour_data$time[i], CRIX$round_unix)]
  print(paste(i, "assigning"))
}
save(hour_data, file="LTC_60minute_CRIX.RData")

# *** XMR **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
setwd(wd)
load(file = "XMR_60minute.RData")

# *** Merging the databases

for (i in 1:nrow(hour_data)) {
  hour_data$crix[i] <- CRIX$price[match(hour_data$time[i], CRIX$round_unix)]
  print(paste(i, "assigning"))
}
save(hour_data, file="XMR_60minute_CRIX.RData")