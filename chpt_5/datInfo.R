dev.set(dev.next())
rm(list=ls())
cat("\014")

# ---- Date Info ----

# Determines whether each day is a workday and what the temperature setpoint should be based on date info, for use as CSV input into EnergyPlus

# Author(s): Brodie W. Hobson
# Last Modified: Monday 29 August 2022 

# ----Load packages----

# Load rstudioapi package
if ("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
require(rstudioapi)
# Load other packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("BBmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("BBmisc")}
require(BBmisc)

# ---- Set working directory ----
setwd(dirname(getActiveDocumentContext()$path))

# Create date string for extracting scheduled hours
start_date <- as.POSIXct('2019-01-01 00:15', tz = "GMT")
end_date <- as.POSIXct('2019-12-31 24:00', tz = "GMT")
datStr <- seq.POSIXt(start_date,end_date,"15 mins")

# Identify holidays
holidays <- 
  c(which(yday(datStr)==1)[1]:tail(which(yday(datStr)==4),n=1), # January 1 to 4 break
    which(month(datStr)==2 & ceiling(day(datStr)/7)==3 & wday(datStr)==2), # Family Day (3rd Monday in Feb)
    which(month(datStr)==4 & ceiling(day(datStr)/7)==3 & wday(datStr)==6), # Good Friday (3rd Friday in Apr)
    which(month(datStr)==5 & ceiling(day(datStr)/7)==3 & wday(datStr)==2), # Victoria Day (3rd Monday in May)
    which(month(datStr)==7 & mday(datStr)==1), # Canada Day (July 1st)
    which(month(datStr)==8 & ceiling(day(datStr)/7)==1 & wday(datStr)==2), # Civic Holiday (1st Monday in Aug)
    which(month(datStr)==9 & ceiling(day(datStr)/7)==1 & wday(datStr)==2), # Labour Day (1st Monday in Sep)
    which(month(datStr)==10 & ceiling(day(datStr)/7)==2 & wday(datStr)==2),# Thanksgiving (2nd Monday in Oct)
    c(which(month(datStr)==12 & day(datStr)==24)[1]:tail(which(month(datStr)==12 & day(datStr)==31),n=1)))


# Create index for workdays and work hours
ind <- wday(datStr) %in% c(1:7) & hour(datStr) %in% c(6:18) & c(1:length(datStr)) %nin% holidays # operating schedule

# Create schedule for zone air temperature setpoints for comparison
T_set <- vector()
for(i in c(1:length(datStr))){
  if(month(datStr[i]) >= 5 & month(datStr[i]) <= 9 & ind[i] == T){
    T_set[length(T_set)+1] <- 23.5
  }else if(month(datStr[i]) >= 5 & month(datStr[i]) <= 9 & ind[i] == F){
    T_set[length(T_set)+1] <- 30
  }else if(ind[i]==T){
    T_set[length(T_set)+1] <- 22
  }else{
    T_set[length(T_set)+1] <- 15
  }
}

df <- cbind.data.frame(datStr,ind,T_set)

#write.csv(df,"./data/datInfo.csv",row.names = F)