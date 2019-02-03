setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd) 
rm(list=ls())

# comparing one fit agains one side guess
#adjust to load proper files
load(file="49_35140_48_time_vector_15minut_ARIMA_fit_XBT.Rdata")
load(file="49_35140_48_15minut_ARIMA_fit_XBT.Rdata")

results <- fifteen_minute_data[(fifteen_minute_data$time %in% c(time_vector)),]

#MDA for results
mean(results$A_DA_1, na.rm = TRUE)
mean(results$A_DA_3, na.rm = TRUE)
mean(results$A_DA_5, na.rm = TRUE)
mean(results$A_DA_10, na.rm = TRUE)
mean(results$A_DA_15, na.rm = TRUE)


# getting returns from one side guess
results$grow_1 <- (results$R_1 > 1)*1
results$grow_3 <- (results$R_3 > 1)*1
results$grow_5 <- (results$R_5 > 1)*1
results$grow_10 <- (results$R_10 > 1)*1
results$grow_15 <- (results$R_15 > 1)*1

mean(results$grow_1, na.rm = TRUE)
mean(results$grow_3, na.rm = TRUE)
mean(results$grow_5, na.rm = TRUE)
mean(results$grow_10, na.rm = TRUE)
mean(results$grow_15, na.rm = TRUE)

results$fall_1 <- (results$R_1 < 1)*1
results$fall_3 <- (results$R_3 < 1)*1
results$fall_5 <- (results$R_5 < 1)*1
results$fall_10 <- (results$R_10 < 1)*1
results$fall_15 <- (results$R_15 < 1)*1

mean(results$fall_1, na.rm = TRUE)
mean(results$fall_3, na.rm = TRUE)
mean(results$fall_5, na.rm = TRUE)
mean(results$fall_10, na.rm = TRUE)
mean(results$fall_15, na.rm = TRUE)

#t testing
# conf level
a <- 0.95

# 1  steps
t.test(results$A_DA_1,results$grow_1,conf.level = a)
t.test(results$A_DA_1,results$fall_1,conf.level = a)

# 3 steps
t.test(results$A_DA_3,results$grow_3,conf.level = a)
t.test(results$A_DA_3,results$fall_3,conf.level = a)

# 5 steps
t.test(results$A_DA_5,results$grow_5,conf.level = a)
t.test(results$A_DA_5,results$fall_5,conf.level = a)

# 10 steps
t.test(results$A_DA_10,results$grow_10,conf.level = a)
t.test(results$A_DA_10,results$fall_10,conf.level = a)

# 15 steps
t.test(results$A_DA_15,results$grow_15,conf.level = a)
t.test(results$A_DA_15,results$fall_15,conf.level = a)

# *** comparing two fits among each other **************************************************************************************

#adjust to load proper files
load(file="49_35140_48_time_vector_15minut_VAR_daily_mix_quartiles_fit_XBT.Rdata") #load time vector of the shorter one
load(file="49_35140_48_15minut_VAR_daily_mix_quartiles_fit_XBT.Rdata") #load the shorter one

shorter <- fifteen_minute_data[(fifteen_minute_data$time %in% c(time_vector)),]

load(file="49_35140_48_15minut_ARIMA_fit_XBT.Rdata") #load the longer one

longer <- fifteen_minute_data[(fifteen_minute_data$time %in% c(time_vector)),]

#MDA for results
mean(shorter$A_DA_1, na.rm = TRUE)
mean(shorter$A_DA_3, na.rm = TRUE)
mean(shorter$A_DA_5, na.rm = TRUE)
mean(shorter$A_DA_10, na.rm = TRUE)
mean(shorter$A_DA_15, na.rm = TRUE)

mean(longer$A_DA_1, na.rm = TRUE)
mean(longer$A_DA_3, na.rm = TRUE)
mean(longer$A_DA_5, na.rm = TRUE)
mean(longer$A_DA_10, na.rm = TRUE)
mean(longer$A_DA_15, na.rm = TRUE)

#t testing
# conf level
a <- 0.95

# 1 steps
t.test(shorter$A_DA_1,longer$A_DA_1,conf.level = a)

# 3 steps
t.test(shorter$A_DA_3,longer$A_DA_3,conf.level = a)

# 5 steps
t.test(shorter$A_DA_5,longer$A_DA_5,conf.level = a)

# 10 steps
t.test(shorter$A_DA_10,longer$A_DA_10,conf.level = a)

# 15 steps
t.test(shorter$A_DA_15,longer$A_DA_15,conf.level = a)

