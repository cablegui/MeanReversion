
library(repmis)
library(zoo)
library(xts)
library(tseries)
library(dplyr)
library(plotrix)
library(ggplot2)


# UrlAddress <- paste0("https://github.com/cablegui/MeanReversion/blob/master/data/",
# "d440c333100d5a9592c0e1485107a0eb56a405a4",
# "/nym_ngatimeseries.csv")

UrlAddress <- paste0("https://raw.githubusercontent.com/cablegui/MeanReversion/master/data/",
                      "nym_ngatimeseries.csv")


#Download data using repmis package
nymexTS <- repmis::source_data(UrlAddress, sep = ",", header = TRUE, fill =  TRUE) 

#Display column names
colnames(nymexTS)

#Search for duplicate date entries
freq <- as.data.frame(table(nymexTS$Date))

#Catch duplicate time series where number of data is > 2
duplTS <- freq[which(freq$Freq >= 2),]
TS_clean <- nymexTS[!nymexTS$Date %in% as.vector(duplTS$Var1),]

#Replace NA's in the data with the data as of t-1
TS_clean <- na.locf(TS_clean)

TS_clean <- zoo(data.frame(apply(TS_clean[,2:ncol(TS_clean)],2,as.numeric)),as.Date(TS_clean[,1],"%d/%m/%Y"))

#Check if there are NA's present
which(is.na(TS_clean))


#Calculate logReturns
logReturns31day <- diff(log(TS_clean$NYM_NGA31D))

#Remove innovations which are 3 std away 
EventStd31day <- 3 * sd(coredata(logReturns31day))

# Time series where data cleaned of jump events. Removing outlier time series
TS_clean_jumps31day <- as.xts(TS_clean$NYM_NGA31D[which(abs(logReturns31day) < EventStd31day) + 1])

#Run adf test. Null hypothesis is non stationary
adfTest31day <- adf.test(na.omit(coredata(TS_clean_jumps31day)))
p_val31day <- adfTest$p.value
p_val31day

#Run adf test on log time series. Null hypothesis is non stationary
adfTest31dayLog <- adf.test(na.omit(coredata(log(TS_clean_jumps31day))))
p_val31dayLog <- adfTest$p.value
p_val31dayLog

