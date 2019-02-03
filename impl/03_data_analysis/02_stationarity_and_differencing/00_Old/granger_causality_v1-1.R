sbst <- five_minute_data[,c(2,3)]
summary(ca.jo(sbst))

summary(grangertest(five_minute_data$SVI_log_diff,five_minute_data$logreturn_price_all, order = 3))

cor(sbst)