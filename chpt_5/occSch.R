dev.set(dev.next())
rm(list=ls())
cat("\014")

# ---- Occupancy Schedule Generation ----

# Creates 10 unique occupancy schedules for the floor based off of collection of available zones

# Author(s): Brodie W. Hobson
# Last Modified: Tuesday 2 August 2022

#----Load packages----

# Load rstudioapi package
if ("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
require(rstudioapi)
# Load other required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("BBmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("BBmisc")}
require(BBmisc)
if ("DescTools" %in% rownames(installed.packages()) == FALSE) {install.packages("DescTools")}
require(DescTools)
if ("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}
require(matrixStats)

#----Set working directory----

# Set working directory----
setwd(dirname(getActiveDocumentContext()$path))

# ----Multi-occupant zone data----
  
# Load in occupancy data from Canal from sensor fusion paper
temp <- read.csv("./data/room_occupancies.csv")

start_date <- as.POSIXct('2019-04-24 00:00', tz = "GMT")
end_date <- as.POSIXct('2019-12-31 23:00', tz = "GMT")
datStr <- seq.POSIXt(start_date,end_date,"hour")

temp$Timestamp <- datStr

temp[which(hour(datStr) %in% c(0:5)),-1] <- 0

start_date <- as.POSIXct('2019-01-01 00:00', tz = "GMT")
end_date <- as.POSIXct('2019-04-23 23:00', tz = "GMT")
dumStr <- seq.POSIXt(start_date,end_date,"hour")

wday_ind <- c(wday(dumStr)[which(diff(wday(dumStr))!=0)],3)

spring_df <- cbind.data.frame(dumStr)

for(i in c(2:ncol(temp))){
  spring_sch <- vector()
  k <- length(wday_ind)
  for(j in wday_ind){
    k <- k-1
    set.seed(k)
    rand_yday <- sample(unique(yday(datStr[which(wday(datStr)==j)])),1)
    spring_sch <- c(spring_sch,temp[which(yday(datStr)==rand_yday),i])
  }
  spring_df <- cbind.data.frame(spring_df,spring_sch)
}

colnames(spring_df) <- colnames(temp)
temp <- rbind.data.frame(spring_df,temp)

# Identify holidays
holidays <- 
  c(which(yday(temp$Timestamp)==1)[1]:tail(which(yday(temp$Timestamp)==4),n=1), # January 1 to 4 break
  which(month(temp$Timestamp)==2 & ceiling(day(temp$Timestamp)/7)==3 & wday(temp$Timestamp)==2), # Family Day (3rd Monday in Feb)
  which(month(temp$Timestamp)==4 & ceiling(day(temp$Timestamp)/7)==3 & wday(temp$Timestamp)==6), # Good Friday (3rd Friday in Apr)
  which(month(temp$Timestamp)==5 & ceiling(day(temp$Timestamp)/7)==3 & wday(temp$Timestamp)==2), # Victoria Day (3rd Monday in May)
  which(month(temp$Timestamp)==7 & mday(temp$Timestamp)==1), # Canada Day (July 1st)
  which(month(temp$Timestamp)==8 & ceiling(day(temp$Timestamp)/7)==1 & wday(temp$Timestamp)==2), # Civic Holiday (1st Monday in Aug)
  which(month(temp$Timestamp)==9 & ceiling(day(temp$Timestamp)/7)==1 & wday(temp$Timestamp)==2), # Labour Day (1st Monday in Sep)
  which(month(temp$Timestamp)==10 & ceiling(day(temp$Timestamp)/7)==2 & wday(temp$Timestamp)==2),# Thanksgiving (2nd Monday in Oct)
  c(which(month(temp$Timestamp)==12 & day(temp$Timestamp)==24)[1]:tail(which(month(temp$Timestamp)==12 & day(temp$Timestamp)==31),n=1)))

temp[holidays,-1] <- 0

df <- temp

for(i in c(1:10)){
  
  set.seed(i)
  ind_multi <- sample(2:11,9,replace=F)
  ind_singl <- sample(2:36,17,replace=F)+10
  
  temp <- cbind.data.frame(df$Timestamp, 
                           df[,ind_singl[c(1:5)]],
                           df[,ind_multi[1]],
                           df[,ind_singl[c(6:10)]],
                           df[,ind_multi[2]],
                           df[,ind_singl[c(11:16)]],
                           df[,ind_multi[c(3:9)]],
                           df[,ind_singl[17]])
  
  colnames(temp) <- c('timestamp', c(1:26))
  
  row.names(temp) <- NULL
  
  
  #write.csv(temp,paste0("./data/occSch_",i,".csv"), row.names = F)
  
  nam <- paste0('occSch_',i)
  assign(nam,temp)
  rm(temp)
}