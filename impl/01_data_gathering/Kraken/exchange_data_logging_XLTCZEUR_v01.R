setwd("C:/Users/Vojt�ch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec/data/03_final_sample/Kraken")
#rm(list=ls())

library("Rbitcoin")

# preps before the loop itself
#split <- data.frame(matrix(NA, nrow = 1000, ncol = 6))
#colnames(split) <- c("Price", "Volume", "Timestamp", "buy/sell", "market/limit" )

#XLTCZEUR <- data.frame(matrix((1:6), nrow = 1, ncol = 6))
#colnames(XLTCZEUR) <- c("Price", "Volume", "Timestamp", "buy/sell", "market/limit" )

#last <- paste('1497991994', '824278669', sep="") # since when to loop

w <- paste('https://api.kraken.com/0/public/Trades?pair=XLTCZEUR&since=',last, sep="") #for initial round

threshold <- 1529629200  # till which data to loop
w_short <- 1 #just initial value
  
# loop for data gathering
while (threshold > w_short){

test <- market.api.query('kraken', url = w)
if ((length(test$result$XLTCZEUR)) > 999) {
  print("OK1")
} else {
  print("Wait")
  Sys.sleep(90)
  test <- market.api.query('kraken', url = w)
}

if ((length(test$result$XLTCZEUR)) > 999) {
  print("OK2")
} else {
  print("Wait2")
  Sys.sleep(900)
  test <- market.api.query('kraken', url = w)
}


for(i in 1:1000){
  for(j in 1:6){
split[i,j] <- test$result$XLTCZEUR[[i]][[j]]
  }
}

Sys.sleep(1)

XLTCZEUR <- rbind(XLTCZEUR, split)

last <- test$result$last
w <- paste('https://api.kraken.com/0/public/Trades?pair=XLTCZEUR&since=', last, sep="")

w_short <- substr(last, 1, 10)
print(w_short)
  }

save(XLTCZEUR, file = "XLTCZEUR.RData")
#write.csv(XLTCZEUR, file = paste('XLTCZEUR.csv')) 

