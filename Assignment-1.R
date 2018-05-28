library(quantmod)
library(ggplot2)

setwd("D:/0go to duke/0RM/Assign1")

start_date <- "2000-01-01"
end_date <- "2017-12-31"

# retrieve MSI from Yahoo
tkr <- "MSI"
getSymbols(tkr,src="yahoo",from=start_date,to=end_date,auto.assign=TRUE)
head(MSI)
tail(MSI)

# calculate daily returns for MSI
MSI$MSI.yahoo <- diff(log(MSI$MSI.Adjusted))
MSI$MSI.yahoo <- exp(MSI$MSI.yahoo)-1

# read MSI daily returns from CRSP [using WRDS]
MSI2 <- read.csv("MSI.csv",
                 header=TRUE,sep=",",stringsAsFactors = FALSE)
Date <- as.Date(as.character(MSI2[,2]),format="%Y%m%d")
MSI.CRSP <- xts(as.numeric(MSI2[,3]),Date)

# combine crsp and yahoo returns
bothMSI <- merge(MSI.CRSP,MSI$MSI.yahoo,join="left")

# delete the first return, which is "NA"
bothMSI <- bothMSI[-1,]
names(bothMSI) <- c("crspMSI","yahooMSI")

# calculate mean returns for crsp and yahoo
colMeans(bothMSI,na.rm=TRUE)
mean(bothMSI$crspMSI, na.rm=TRUE)
mean(bothMSI$yahooMSI, na.rm=TRUE)

# calculate std dev for crsp and yahoo
sd(bothMSI[,1], na.rm = TRUE)
sd(bothMSI[,2], na.rm = TRUE)

# calculate correlation between crsp and yahoo
cor(bothMSI[,1],bothMSI[,2],use="complete.obs")
cor(bothMSI[,1],bothMSI[,2], use="complete.obs")

# scatter plot 
ggplot(bothMSI,aes(x=crspMSI,y=yahooMSI)) + 
  geom_point() +
  xlab("MSI crsp returns") +
  ylab("MSI yahoo returns") + ggtitle("MSI plot")

# find the max and min difference between crsp and yahoo
diffMSI <- bothMSI$crsp - bothMSI$yahoo
max(diffMSI,na.rm = TRUE)
min(diffMSI,na.rm = TRUE)
bothMSI[which.max(diffMSI),]
bothMSI[which.min(diffMSI),]

# retrieve SLB from Yahoo
tkr <- "SLB"
getSymbols(tkr,src="yahoo",from=start_date,to=end_date,auto.assign=TRUE)
head(SLB)
tail(SLB)

# calculate daily returns for SLB
SLB$SLB.yahoo <- diff(log(SLB$SLB.Adjusted))
SLB$SLB.yahoo <- exp(SLB$SLB.yahoo)-1

# read SLB daily returns from CRSP [using WRDS]
SLB2 <- read.csv("SLB.csv",
                 header=TRUE,sep=",",stringsAsFactors = FALSE)
Date <- as.Date(as.character(SLB2[,2]),format="%Y%m%d")
SLB.CRSP <- xts(as.numeric(SLB2[,3]),Date)

# combine crsp and yahoo returns
bothSLB <- merge(SLB.CRSP,SLB$SLB.yahoo,join="left")

# delete the first return, which is "NA"
bothSLB <- bothSLB[-1,]
names(bothSLB) <- c("crspSLB","yahooSLB")

# calculate mean returns for crsp and yahoo
colMeans(bothSLB)

# calculate std dev for crsp and yahoo
sd(bothSLB[,1])
sd(bothSLB[,2])

# calculate correlation between crsp and yahoo
cor(bothSLB[,1],bothSLB[,2])

# scatter plot 
ggplot(bothSLB,aes(x=crspSLB,y=yahooSLB)) + 
  geom_point() +
  xlab("SLB crsp returns") +
  ylab("SLB yahoo returns") + ggtitle("SLB plot")

# find the max and min difference between crsp and yahoo
diffSLB <- bothSLB$crsp - bothSLB$yahoo
max(diffSLB)
min(diffSLB)
bothSLB[which.max(diffSLB),]
bothSLB[which.min(diffSLB),]

# retrieve AZO from Yahoo
tkr <- "AZO"
getSymbols(tkr,src="yahoo",from=start_date,to=end_date,auto.assign=TRUE)
head(AZO)
tail(AZO)

# calculate daily returns for AZO
AZO$AZO.yahoo <- diff(log(AZO$AZO.Adjusted))
AZO$AZO.yahoo <- exp(AZO$AZO.yahoo)-1

# read AZO daily returns from CRSP [using WRDS]
AZO2 <- read.csv("AZO.csv",
                 header=TRUE,sep=",",stringsAsFactors = FALSE)
Date <- as.Date(as.character(AZO2[,2]),format="%Y%m%d")
AZO.CRSP <- xts(as.numeric(AZO2[,3]),Date)

# combine crsp and yahoo returns
bothAZO <- merge(AZO.CRSP,AZO$AZO.yahoo,join="left")

# delete the first return, which is "NA"
bothAZO <- bothAZO[-1,]
names(bothAZO) <- c("crspAZO","yahooAZO")

# calculate mean returns for crsp and yahoo
colMeans(bothAZO)

# calculate std dev for crsp and yahoo
sd(bothAZO[,1])
sd(bothAZO[,2])

# calculate correlation between crsp and yahoo
cor(bothAZO[,1],bothAZO[,2])

# scatter plot 
ggplot(bothAZO,aes(x=crspAZO,y=yahooAZO)) + 
  geom_point() +
  xlab("AZO crsp returns") +
  ylab("AZO yahoo returns") + ggtitle("AZO plot")

# find the max and min difference between crsp and yahoo
diffAZO <- bothAZO$crsp - bothAZO$yahoo
max(diffAZO)
min(diffAZO)
bothAZO[which.max(diffAZO),]
bothAZO[which.min(diffAZO),]

# retrieve K from Yahoo
tkr <- "K"
getSymbols(tkr,src="yahoo",from=start_date,to=end_date,auto.assign=TRUE)
head(K)
tail(K)

# calculate daily returns for K
K$K.yahoo <- diff(log(K$K.Adjusted))
K$K.yahoo <- exp(K$K.yahoo)-1

# read K daily returns from CRSP [using WRDS]
K2 <- read.csv("K.csv",
               header=TRUE,sep=",",stringsAsFactors = FALSE)
Date <- as.Date(as.character(K2[,2]),format="%Y%m%d")
K.CRSP <- xts(as.numeric(K2[,3]),Date)

# combine crsp and yahoo returns
bothK <- merge(K.CRSP,K$K.yahoo,join="left")

# delete the first return, which is "NA"
bothK <- bothK[-1,]
names(bothK) <- c("crspK","yahooK")

# calculate mean returns for crsp and yahoo
colMeans(bothK)

# calculate std dev for crsp and yahoo
sd(bothK[,1])
sd(bothK[,2])

# calculate correlation between crsp and yahoo
cor(bothK[,1],bothK[,2])

# scatter plot 
ggplot(bothK,aes(x=crspK,y=yahooK)) + 
  geom_point() +
  xlab("K crsp returns") +
  ylab("K yahoo returns") + ggtitle("K plot")

# find the max and min difference between crsp and yahoo
diffK <- bothK$crsp - bothK$yahoo
max(diffK)
min(diffK)
bothK[which.max(diffK),]
bothK[which.min(diffK),]