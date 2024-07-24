dev.off()
rm(list=ls())
cat("\014")

#----Building B Occupancy Schedule for Energy Simulation 2020----

# Use available Wi-Fi data from the first half of 2020 to extrapolate out a timeseries occupancy schedule for use in EnergyPlus simulation

# Date: Monday 26 July 2021
# Author(s): Brodie W. Hobson

#----Load packages----

# Load rstudioapi pacakge
if ("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
require(rstudioapi)
if ("DescTools" %in% rownames(installed.packages()) == FALSE) {install.packages("DescTools")}
require(DescTools)
if ("BBmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("BBmisc")}
require(BBmisc)
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)

#----Set working directory----

# Set working directory----
setwd(dirname(getActiveDocumentContext()$path)) 

#----Analysis----

#Specify date format used in .csv
date_format = "%Y-%m-%d %H:%M"

# Read in measured data
df <- read.csv("./data/buildingB_wifi_anon.csv")
df[,1] <- as.POSIXct(df[,1], format = date_format, tz = "GMT")

start_date <- as.POSIXct('2020-01-01 00:00', tz = "GMT")
lockdown_date <- as.POSIXct('2020-03-16 00:00', tz = "GMT")
end_date <- as.POSIXct('2020-03-30 23:00', tz = "GMT")

ind <- which(df[,1] %in% seq.POSIXt(start_date,end_date,"hour"))

df <- cbind.data.frame(df[ind,1], rowSums(df[ind,-1]))
row.names(df) <- NULL
colnames(df) <- c("Timestamp", "DC")

for(i in yday(df[,1])){
  
  ind <- which(yday(df[,1]) == i)
  temp <- df[ind,2]
  stag <- max(temp[1], temp[length(temp)])
  temp <- temp - stag
  temp[which(temp < 0)] <- 0
  df[ind,2] <- temp
  
}

df[,2] <- round(df[,2]/1.2, digits = 0)

colnames(df) <- c("timestamp", "occupants")

ind <- which(df[,1] %in% seq.POSIXt(lockdown_date,end_date,"hour") & wday(df[,1]) %in% c(2:6) & hour(df[,1]) %in% c(9:17))

max_occ_lockdown <- Quantile(df[ind,2], probs = 0.95)

plot(df[ind,2])

wday_df <- df[which(df[,1] %in% seq.POSIXt(lockdown_date,end_date,"hour") & wday(df[,1])),]

first_arrive <- vector()
last_arrive <- vector()
first_depart <- vector()
last_depart <- vector()

for(i in unique(yday(wday_df[,1]))){
  
  ind <- which(yday(wday_df[,1]) == i)
  
  temp <- wday_df[ind,2]
  first_arrive[length(first_arrive)+1] <- which(temp > 2)[1]
  last_arrive[length(last_arrive)+1] <- which(temp == max(temp))[1]
  last_temp <- which(temp == max(temp))[1]
  first_depart[length(first_depart)+1] <- which(temp == max(temp))[length(which(temp == max(temp)))]+1
  last_depart[length(last_depart)+1] <- which(temp > 2)[which(which(temp > 2) > last_temp)][1]
  
}

first_arrive[which(first_arrive > 11)] <- NA

last_depart[which(is.na(last_depart)==T)] <- first_depart[which(is.na(last_depart)==T)]

last_arrive[which(is.na(first_arrive)==T)] <- NA
first_depart[which(is.na(first_arrive)==T)] <- NA
last_depart[which(is.na(first_arrive)==T)] <- NA

last_depart[which(last_depart <= first_depart)] <- first_depart[which(last_depart <= first_depart)]

arrive_cdf <- cumsum(hist(first_arrive, breaks = seq(from = 0, to = 24, by = 1), plot = F)$counts)/length(first_arrive)
depart_cdf <- cumsum(hist(last_depart, breaks = seq(from = 0, to = 24, by = 1), plot = F)$counts)/length(last_depart)

arrive_cdf <- normalize(arrive_cdf, method = "range", range = c(0,1))
depart_cdf <- normalize(depart_cdf, method = "range", range = c(0,1))

plot(arrive_cdf)
plot(depart_cdf)

first_arrive_sch <- which(arrive_cdf > 0.05)[1]
last_arrive_sch <- which(arrive_cdf > 0.95)[1]
first_depart_sch <- which(depart_cdf > 0.05)[1]
last_depart_sch <- which(depart_cdf > 0.95)[1]

week_day_sch <- rep(0,24)
week_end_sch <- rep(0,24)

start_date <- as.POSIXct('2020-03-30 00:00', tz = "GMT")
end_date <- as.POSIXct('2020-12-31 23:00', tz = "GMT")
datStr <- seq.POSIXt(start_date,end_date,"hour")

occStr <- list()

for(i in wday(datStr)[seq(1,length(wday(datStr)),24)]){
  if(i %in% c(2:6)){
    occStr[[length(occStr)+1]] <- week_day_sch
  }else{
    occStr[[length(occStr)+1]] <- week_end_sch
  }
}


occStr <- unlist(occStr)
df_lockdown <- cbind.data.frame(datStr,occStr)
colnames(df_lockdown) <- colnames(df)

df <- rbind.data.frame(df,df_lockdown)

df$occupants <- df$occupants/max(df$occupants)