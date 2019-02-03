setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Kraken/XBT", sep = "")
setwd(wd) 
rm(list=ls())

library(vars)

load(file="49_35140_48_15minut_VAR_daily_mix_quartiles_fit_XBT.Rdata")

sbst <- as.matrix(fifteen_minute_data[, c(18,21,22,23)])

lags <- VARselect(sbst)
lags <- max(2,round(median(lags$selection)))

cajoclass <- ca.jo(sbst, type = "eigen", K = lags, spec = "transitory")
fit <- vec2var(cajoclass, r=1)

irf(fit, response = "logreturn_price_all")
plot(irf(fit, response = "logreturn_price_all", nsteps = 15))