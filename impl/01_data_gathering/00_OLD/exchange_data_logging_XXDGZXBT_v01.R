setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec/impl/data_gathering")
#rm(list=ls())

library("Rbitcoin")

# preps before the loop itself
split <- data.frame(matrix(NA, nrow = 1000, ncol = 6))
colnames(split) <- c("Price", "Volume", "Timestamp", "buy/sell", "market/limit" )

XXDGZXBT <- data.frame(matrix((1:6), nrow = 1, ncol = 6))
colnames(XXDGZXBT) <- c("Price", "Volume", "Timestamp", "buy/sell", "market/limit" )

last <- paste('497992000', '824278669', sep="") # since when to loop

w <- paste('https://api.kraken.com/0/public/Trades?pair=XXDGZXBT&since=',last, sep="") #for initial round

threshold <- 1529629200  # till which data to loop
w_short <- 1 #just initial value
  
# loop for data gathering
while (threshold > w_short){

test <- market.api.query('kraken', url = w)
if ((length(test$result$XXDGZXBT)) > 999) {
  print("OK1")
} else {
  print("Wait")
  Sys.sleep(90)
  test <- market.api.query('kraken', url = w)
}

if ((length(test$result$XXDGZXBT)) > 999) {
  print("OK2")
} else {
  print("Wait2")
  Sys.sleep(900)
  test <- market.api.query('kraken', url = w)
}


for(i in 1:1000){
  for(j in 1:6){
split[i,j] <- test$result$XXDGZXBT[[i]][[j]]
  }
}

Sys.sleep(1)

XXDGZXBT <- rbind(XXDGZXBT, split)

last <- test$result$last
w <- paste('https://api.kraken.com/0/public/Trades?pair=XXDGZXBT&since=', last, sep="")

w_short <- substr(last, 1, 10)
print(w_short)
  }

save(XXDGZXBT, file = "XXDGZXBT.RData")
#write.csv(XXDGZXBT, file = paste('XXDGZXBT.csv')) 

