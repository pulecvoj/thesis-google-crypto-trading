setwd("C:/Users/Vojtìch Pulec/OneDrive/Dokumenty/HU/Thesis/master-thesis_Pulec") # thats for me to get to proprer level
wd <- getwd()
wd <- paste(wd, "/data/03_final_sample/Google/monero", sep = "")
setwd(wd)
rm(list=ls())

library("tcltk")


files <- list.files(path=getwd(), pattern="*.csv", full.names=T, recursive=FALSE)

nfiles <- length(as.vector(files))-1

# *** task bar
pb <- tkProgressBar(title = "progress bar", min = 0, max = nfiles, width = 500)

#p1 is the scale of period1 according to which others will be normalized
p1 <- read.csv(row.names = NULL, files[1])
p1 <- p1[-1,c(1,2)]
colnames(p1)=c("date",'rank')

#merge p1 and period2, save to p1, merge with period3, save to p1....
#nfiles-1
for (i in 1:nfiles)  {p2 <- read.csv(row.names = NULL, files[i+1])
colnames(p2) <- c("date",'rank')
p2 <- p2[-1,c(1,2)]
#merging two periods
merged=merge(p1,p2, by = 'date', match = "all", all = T)
merged$rank.x=as.numeric(paste(merged$rank.x))
merged$rank.y=as.numeric(paste(merged$rank.y))

#find average of col1/col2 w/o NA - base of rescaling
intersect=na.omit(merged)

intersect[intersect == 0] <- NA

intersect <- na.omit(intersect)

average=mean(na.omit((as.numeric(intersect[,2])/(as.numeric(intersect[,3])))))

#progress bar
setTkProgressBar(pb, i, label=paste( round(i/nrow(nfiles)*500 , 0),"% done"))

#fill NA values in col1 with rescaled values (col2*average)
missing=which(is.na(merged[,2]))
for (j in missing) {
  merged[j,2]= merged[j,3]*average
}
p1=merged[,-3] 
colnames(p1)=c("date","rank") 
}

p1[,2]= round(p1[,2])

write.csv(p1, file = paste('output.csv')) 
