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


# *** loading prices
setwd(base_wd) # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd)
load(file = "XBT_5minute.RData")

# *** Merging the databases

for (i in 1:nrow(five_minute_data)) {
  five_minute_data$crix <- CRIX$price[match(five_minute_data$time[i], CRIX$round_unix)]
  print(paste(i, "assigning"))
}


save(five_minute_data, file="XBT_5minute_CRIX.RData")
