setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd) 
rm(list=ls())


load(file="49_105419_48_time_vector_five_minut_ARIMA_fit_XBT.Rdata")
load(file="49_105419_48_five_minut_ARIMA_fit_XBT.Rdata")

results <- five_minute_data[(five_minute_data$time %in% c(time_vector)),]

#MDA for results
mean(results$A_DA_5, na.rm = TRUE)
mean(results$A_DA_10, na.rm = TRUE)
mean(results$A_DA_15, na.rm = TRUE)
mean(results$A_DA_30, na.rm = TRUE)
mean(results$A_DA_60, na.rm = TRUE)


# getting returns from one side guess
results$grow_5 <- (results$R_5 > 1)*1
results$grow_10 <- (results$R_10 > 1)*1
results$grow_15 <- (results$R_15 > 1)*1
results$grow_30 <- (results$R_30 > 1)*1
results$grow_60 <- (results$R_60 > 1)*1

mean(results$grow_5, na.rm = TRUE)
mean(results$grow_10, na.rm = TRUE)
mean(results$grow_15, na.rm = TRUE)
mean(results$grow_30, na.rm = TRUE)
mean(results$grow_60, na.rm = TRUE)

results$fall_5 <- (results$R_5 < 1)*1
results$fall_10 <- (results$R_10 < 1)*1
results$fall_15 <- (results$R_15 < 1)*1
results$fall_30 <- (results$R_30 < 1)*1
results$fall_60 <- (results$R_60 < 1)*1

mean(results$fall_5, na.rm = TRUE)
mean(results$fall_10, na.rm = TRUE)
mean(results$fall_15, na.rm = TRUE)
mean(results$fall_30, na.rm = TRUE)
mean(results$fall_60, na.rm = TRUE)

#t testing
# conf level
a <- 0.99

# 5 minute
t.test(results$A_DA_5,results$grow_5,conf.level = a)
t.test(results$A_DA_5,results$fall_5,conf.level = a)

# 10 minute
t.test(results$A_DA_10,results$grow_10,conf.level = a)
t.test(results$A_DA_10,results$fall_10,conf.level = a)

# 15 minute
t.test(results$A_DA_15,results$grow_15,conf.level = a)
t.test(results$A_DA_15,results$fall_15,conf.level = a)

# 30 minute
t.test(results$A_DA_30,results$grow_30,conf.level = a)
t.test(results$A_DA_30,results$fall_30,conf.level = a)

# 60 minute
t.test(results$A_DA_60,results$grow_60,conf.level = a)
t.test(results$A_DA_60,results$fall_60,conf.level = a)