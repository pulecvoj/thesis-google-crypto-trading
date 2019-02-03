rm(list=ls())
base_wd <- "C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec" # thats for me to get to proprer level
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/CRIX", sep = "")
setwd(wd) 

library(plyr)
library(ggplot2)

#loading data
load(file = "CRIX_day.RData")


# *** XBT **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd)
load(file = "XBT_day.RData")

# *** Merging the databases

for (i in 1:nrow(day_data)) {
  day_data$crix[i] <- CRIX$price[match(day_data$time[i], CRIX$round_unix)]
  print(paste(i, "assigning"))
}
save(day_data, file="XBT_day_CRIX.RData")

# *** ETH **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/ETH", sep = "")
setwd(wd)
load(file = "ETH_day.RData")

# *** Merging the databases

for (i in 1:nrow(day_data)) {
  day_data$crix[i] <- CRIX$price[match(day_data$time[i], CRIX$round_unix)]
  print(paste(i, "assigning"))
}
save(day_data, file="ETH_day_CRIX.RData")

# *** LTC **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/LTC", sep = "")
setwd(wd)
load(file = "LTC_day.RData")

# *** Merging the databases

for (i in 1:nrow(day_data)) {
  day_data$crix[i] <- CRIX$price[match(day_data$time[i], CRIX$round_unix)]
  print(paste(i, "assigning"))
}
save(day_data, file="LTC_day_CRIX.RData")

# *** XMR **********************************************************
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XMR", sep = "")
setwd(wd)
load(file = "XMR_day.RData")

# *** Merging the databases

for (i in 1:nrow(day_data)) {
  day_data$crix[i] <- CRIX$price[match(day_data$time[i], CRIX$round_unix)]
  print(paste(i, "assigning"))
}
save(day_data, file="XMR_day_CRIX.RData")