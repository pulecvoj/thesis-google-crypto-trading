setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/CRIX", sep = "")
setwd(wd)
rm(list=ls())

library(plyr)

CRIX <- read.csv2(file = "Crix_HF.csv", header = TRUE, sep = ",")
CRIX$unix <- as.numeric(as.POSIXct(CRIX$Timestamp, 'GMT'))

CRIX$round_unix <- (CRIX$unix  %/% 300 +1)* 300