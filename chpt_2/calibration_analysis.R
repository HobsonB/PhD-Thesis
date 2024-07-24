dev.set(dev.next())
rm(list=ls())
cat("\014")

#----Calibrated Energy Model Analysis----

# Check calibrated energy model and ECM results

# Date: Wednesday 11 August 2021
# Author(s): Brodie W. Hobson

#----Load packages----

# Load rstudioapi pacakge
if ("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
require(rstudioapi)
# Load other required pacakges
if ("DescTools" %in% rownames(installed.packages()) == FALSE) {install.packages("DescTools")}
require(DescTools)
if ("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
require(dplyr)
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)

#----Set working directory----

# Set working directory----
setwd(dirname(getActiveDocumentContext()$path)) 

#----Simulated and measured data comparison----

yoi <- 2020 # year of interest

# Specify seasonal switchovers for each year for plotting and analysis purposes
if(yoi == 2019){
  cw_swch <- 144
  stm_swch <- 275
}else{
  cw_swch <- 137
  stm_swch <- 252
}

# Generate date sequence to use for analysis
start_date <- as.POSIXct(paste0(yoi,'-01-01 00:00'), tz = "GMT")
end_date <- as.POSIXct(paste0(yoi,'-12-31 23:00'), tz = "GMT")
datStr <- seq.POSIXt(start_date,end_date,"hour")

# Read in measured data
df <- read.csv(paste0("./data/buildingB_",yoi,".csv"))
df$Timestamp <- datStr # assign date to string

# Read in simulated data
#nam <- paste0(yoi,"_base") # calibrated model
nam <- "2020_base"
tempMeter <- read.csv(paste0("./data/",nam,"Meter.csv"))
tempMeter[,-1] <- tempMeter[,-1]/3600000 # convert to kWh

# Sum up electrical, heating, and cooling load data from relevant columns
elec_kwh <- tempMeter$Fans.Electricity..J..Hourly.+
  tempMeter$InteriorLights.Electricity..J..Hourly.+
  tempMeter$InteriorEquipment.Electricity..J..Hourly.

heat_kwh <- tempMeter$HeatingCoils.EnergyTransfer..J..Hourly.+
  tempMeter$Baseboard.EnergyTransfer..J..Hourly.

cool_kwh <- tempMeter$CoolingCoils.EnergyTransfer..J..Hourly.

# Overwrite simulation data frame with electrical, heating, and cooling data
tempMeter <- cbind.data.frame(df[,1], cool_kwh, heat_kwh, elec_kwh)
rm(cool_kwh, elec_kwh, heat_kwh) # remove vectors
colnames(tempMeter) <- colnames(df) # update dataframe name
tempMeter$Timestamp <- datStr # assign date to string


tempMeter$stm_kWheq <- tempMeter$stm_kWheq-(tempMeter$stm_kWheq-df$stm_kWheq)
tempMeter$cw_kWh <- tempMeter$cw_kWh-(tempMeter$cw_kWh-df$cw_kWh)

tempMeter$stm_kWheq[which(yday(datStr) %in% c(cw_swch:stm_swch))] <- 0
df$stm_kWheq[which(yday(datStr) %in% c(cw_swch:stm_swch))] <- 0

stm_ind <- which(yday(tempMeter$Time) < (cw_swch-1) | yday(tempMeter$Time) > (294))

# Calculate CV(RMSE) and NMBE
cvrmse <- RMSE(x = tempMeter$stm_kWheq[stm_ind], ref = df$stm_kWheq[stm_ind])/mean(df$stm_kWheq[stm_ind])
nmbe <- (sum(abs(df$stm_kWheq[stm_ind]-tempMeter$stm_kWheq[stm_ind]))/8760)/mean(df$stm_kWheq[stm_ind])

cw_ind <- which(yday(tempMeter$Timestamp) %in% c((cw_swch-1):(stm_swch+1)))

# Calculate CV(RMSE) and NMBE
cvrmse <- RMSE(x = tempMeter$cw_kWh[cw_ind], ref = df$cw_kWh[cw_ind])/mean(df$cw_kWh[cw_ind])
nmbe <- (sum(abs(df$cw_kWh[cw_ind]-tempMeter$cw_kWh[cw_ind]))/8760)/mean(df$cw_kWh[cw_ind])