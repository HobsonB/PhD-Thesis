dev.set(dev.next())
rm(list=ls())
cat("\014")

# ---- Occupancy Desegregation Function ----

# Determines room-level occupancies based on environmental sensors and floor-level Wi-Fi data using sensor fusion

# Author(s): Brodie W. Hobson
# Last Modified: Mon. Mar. 8, 2021

# ---- Packages ----

# Load required packages
if ("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
require(rstudioapi)
if ("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}
require(matrixStats)
if ("matrixcalc" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixcalc")}
require(matrixcalc)
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
require(stringr)

# ---- Working Directory ----

# Set working directory
setwd(dirname(getActiveDocumentContext()$path))

# ---- Read in data ----

# Specify date format used in .csv
date_format = "%Y-%m-%d %H:%M"

# Wi-Fi device count data
ap <- read.csv("./data/ap.csv") 
ap[,1] <- as.POSIXct(ap[,1], format = date_format, tz = "GMT")
colnames(ap) <- gsub("X", "",  colnames(ap))
# CO2 sensor data
co2 <- read.csv("./data/co2.csv")
co2[,1] <- as.POSIXct(co2[,1], format = date_format, tz = "GMT")

# Motion detector data
pir <- read.csv("./data/pir.csv")
pir[,1] <- as.POSIXct(pir[,1], format = date_format, tz = "GMT")
# Outdoor air damper data
oad <- read.csv("./data/oad.csv")
oad[,1] <- as.POSIXct(oad[,1], format = date_format, tz = "GMT")
# VAV flow station data
vav <- read.csv("./data/vav.csv")
vav[,1] <- as.POSIXct(vav[,1], format = date_format, tz = "GMT")

# ASHRAE Table 6.2.2.1 data
t6221 <- read.csv("./data/ashrae_t6221_vbzmin.csv")
# Wi-Fi access point metadata
ap_meta <- read.csv("./data/ap_meta.csv")
ap_meta <- ap_meta[which(ap_meta$Floor==4),]
# Room metadata
rm_meta <- read.csv("./data/rm_meta.csv")
rm_meta <- rm_meta[which(rm_meta$Floor==4),]

# ---- Clean data ----

# Find the Euclidean distance between each access point and room to determine which AP serves which room
for(i in c(1:nrow(rm_meta))){
  rm_meta$ap[i] <- ap_meta$AP[which.min(sqrt((rm_meta$x[i]-ap_meta$x)^2 + (rm_meta$y[i]-ap_meta$y)^2))]
}

# Occupant-count estimates at each access point
ap_occ <- data.frame(ap[,1])
for(i in c(2:ncol(ap))){
  temp <- as.data.frame(matrix(ap[,i], byrow=F, nrow =24))
  stagnant_fn <- function(day_dat){
    day_dat[c(1:6)] <- rep(0,6)
    if(length(unique(day_dat))<3){
      stagnant_rm <- rep(0,24)
    }else if(sum(day_dat > 0) != 0){
      first_arrive <- which(day_dat > 0)[1]
      end_dc <- day_dat[24]
      m <- (end_dc/(24-(first_arrive-1)))*c(1:(24-(first_arrive-1)))
      stagnant_rm <- c(day_dat[c(1:(first_arrive-1))], day_dat[c(first_arrive:24)]-m)
      stagnant_rm[which(stagnant_rm<0)] <- 0
    }else{
      stagnant_rm <- rep(0,24)
    }
    return(stagnant_rm)
  }
  for(j in c(1:ncol(temp))){
    temp[,j] <- round(stagnant_fn(temp[,j])/1.2)
  }
  ap_occ[,i] <- unlist(temp)
}
colnames(ap_occ) <- colnames(ap)

# Total occupant count at the floor level at each timestep
temp <- rm_meta
occ_tot <- rowSums(ap_occ[,-1])

# Recalibrate CO2 sensors weekly to atmospheric PPM
for(i in unique(week(co2$Timestamp))){
  temp <- data.matrix(co2[which(week(co2$Timestamp)==i),-1])
  mins <- colMins(temp)
  temp <- sweep(temp,2,mins-410)
  co2[which(week(co2$Timestamp)==i),-1] <- temp
}

# Reindex CO2, PIR, and VAV data based on room number to make future operations easier
temp_co2 <- data.frame(co2[,1])
colnames(temp_co2) <- c("Timestamp")
temp_pir <- data.frame(pir[,1])
colnames(temp_pir) <- c("Timestamp")
temp_vav <- data.frame(vav[,1])
colnames(temp_vav) <- c("Timestamp")

for(i in c(1:nrow(rm_meta))){
  # For PIR data
  if(rm_meta$md[i]!=""){
    temp <- pir[,grepl(rm_meta$md[i],colnames(pir))]
  }else{
    temp <- rep(NA,nrow(pir))
  }
  temp_pir <- cbind.data.frame(temp_pir,temp)
  colnames(temp_pir)[i+1] <- rm_meta$Room.Number[i]
  # For CO2 data
  if(rm_meta$co2[i]!=""){
    temp <- co2[,grepl(rm_meta$co2[i],colnames(co2))]
  }else{
    temp <- rep(NA,nrow(co2))
  }
  temp_co2 <- cbind.data.frame(temp_co2,temp)
  colnames(temp_co2)[i+1] <- rm_meta$Room.Number[i]
  # For VAV data
  if(rm_meta$vav[i]!=""){
    nam_mx <- str_split(rm_meta$vav[i]," ", simplify = T)
    total_vav <- rep(0,nrow(vav))
    for(j in c(1:length(nam_mx))){
      individual_vav <- vav[,grepl(nam_mx[j],colnames(vav))]
      total_vav <- total_vav + individual_vav
    }
    temp <- total_vav
  }else{
    temp <- rep(NA,nrow(vav))
  }
  temp_vav <- cbind.data.frame(temp_vav,temp)
  colnames(temp_vav)[i+1] <- rm_meta$Room.Number[i]
  
  rm(temp)
}

# Replace raw data with cleaned data
co2 <- temp_co2
rm(temp_co2)
pir <- temp_pir
rm(temp_pir)
vav <- temp_vav
rm(temp_vav)

# Function to be used for converting OAD to OAF (this particular one is the DA10 curve)
oaf_fn <- function(damper_positions){
  oaf <- exp(0.056*damper_positions-0.98)
  return(oaf)
}

# Convert outdoor air damper (OAD) position to outdoor airflow fraction (OAF)
oaf <- cbind.data.frame(oad[,1], oaf_fn(oad[,-1]))
colnames(oaf) <- colnames(oad)
vav_oaf <- vav
for(i in unique(na.omit(rm_meta$ahu))){
  vav_oaf[,which(rm_meta$ahu==i)+1] <- vav_oaf[,which(rm_meta$ahu==i)+1]*(oaf[,i+1]/100)
}

# ---- Occupant-count estimates using CO2, PIR, and Wi-Fi AP data ----

# Determine occupancy of zones that have CO2 sensors
co2_occ <- cbind.data.frame(co2[,1],floor(((vav_oaf[,-1])*(co2[,-1]-410))/(0.6*3600)))
colnames(co2_occ)[1] <- "Timestamp"

# Shift readings back by one hour to account for lag
temp <- co2_occ[1,]
co2_occ[,-1] <- shift.up(as.matrix(co2_occ[,-1]), rows = 1, fill = 0)
co2_occ[nrow(co2_occ),] <- temp

# Replace unoccupied rooms with 0
ind <- which(rm_meta$Design.Occupancy==0)+1
co2_occ[,ind] <- rep(0,nrow(co2_occ))

# Replace single occupant rooms with motion detector data
ind <- which(rm_meta$Design.Occupancy==1 & rm_meta$md!="")+1
co2_occ[,ind] <- pir[,ind]

# Replace single occupant rooms with CO2 data only 
ind <- which(rm_meta$Design.Occupancy==1 & rm_meta$co2!="" & rm_meta$md=="")+1
for(i in ind){
  co2_occ[which(co2_occ[,i] > 1),i] <- 1
}

remaining_ind <- which(is.na(colSums(as.matrix(co2_occ[,-1])))==1)

for(i in 2:ncol(ap_occ)){
  
  # Index which rooms belong to which AP
  ind <- which(grepl(colnames(ap_occ)[i],rm_meta$ap)==1)
  # Determine how many occupants are unaccounted based on other sensors
  accounted <- rowSums(co2_occ[,ind+1], na.rm = T)
  temp <- ap_occ[,i]
  unaccounted <- temp-accounted
  unaccounted[unaccounted<0] <- 0
  # Index which rooms do no have occupancy data yet
  ind <- ind[which(ind %in% remaining_ind)]
  # Fraction of remaining occuapnts each room accounts for
  f_occ <- rm_meta$Design.Occupancy[ind]/sum(rm_meta$Design.Occupancy[ind])
  
  # Fill single occupant rooms with one person where appropriate
  single_ind <- which(rm_meta$Design.Occupancy[ind]==1)
  for(j in single_ind){
    temp <- floor(f_occ[j]*unaccounted)
    temp[temp>1] <- 1
    co2_occ[,ind[j]+1] <- temp
    unaccounted <- unaccounted-temp
  }
  
  # Fill multi-occupant rooms with persons where appropriate
  multi_ind <- which(rm_meta$Design.Occupancy[ind]>1)
  for(j in multi_ind){
    temp <- floor(f_occ[j]*unaccounted)
    co2_occ[,ind[j]+1] <- temp
  }
}

# Reorder the columns from lowest room number to highest room number so each case can be compared
rm_occ <- co2_occ[,c(1,c(order(colnames(co2_occ[,-1]))+1))]

# ---- Observe rooms' CO2 concentrations ----

over_1000 <- vector()

for(i in 2:ncol(co2)){
  over_1000[length(over_1000)+1] <- length(which(co2[,i] > 1000))
}

# ---- Occupant-count estimates using PIR and Wi-Fi AP data ----

# Determine occupancy of single occupant rooms with PIR sensors
md_occ <- pir

# Replace unoccupied rooms with 0
ind <- which(rm_meta$Design.Occupancy==0)+1
md_occ[,ind] <- rep(0,nrow(md_occ))

remaining_ind <- which(is.na(colSums(as.matrix(md_occ[,-1])))==1)

for(i in 2:ncol(ap_occ)){
  
  # Index which rooms belong to which AP
  ind <- which(grepl(colnames(ap_occ)[i],rm_meta$ap)==1)
  # Determine how many occupants are unaccounted based on other sensors
  accounted <- rowSums(md_occ[,ind+1], na.rm = T)
  temp <- ap_occ[,i]
  unaccounted <- temp-accounted
  unaccounted[unaccounted<0] <- 0
  # Index which rooms do no have occupancy data yet
  ind <- ind[which(ind %in% remaining_ind)]
  # Fraction of remaining occuapnts each room accounts for
  f_occ <- rm_meta$Design.Occupancy[ind]/sum(rm_meta$Design.Occupancy[ind])
  
  # Fill single occupant rooms with one person where appropriate
  single_ind <- which(rm_meta$Design.Occupancy[ind]==1)
  for(j in single_ind){
    temp <- floor(f_occ[j]*unaccounted)
    temp[temp>1] <- 1
    md_occ[,ind[j]+1] <- temp
    unaccounted <- unaccounted-temp
  }
  
  # Fill multi-occupant rooms with persons where appropriate
  multi_ind <- which(rm_meta$Design.Occupancy[ind]>1)
  for(j in multi_ind){
    temp <- floor(f_occ[j]*unaccounted)
    md_occ[,ind[j]+1] <- temp
  }
}

# Reorder the columns from lowest room number to highest room number so each case can be compared
rm_occ <- md_occ[,c(1,c(order(colnames(md_occ[,-1]))+1))]

# ---- Occupant-count estimates using Wi-Fi AP data only ----

# Create  a blank data frame where all values are zero, using the PIR data as a baseline for simplicity
ap_only_occ <- pir
ap_only_occ[,-1][ap_only_occ[,-1]!=0] <- 0
ap_only_occ[is.na.data.frame(ap_only_occ)] <- 0


for(i in 2:ncol(ap_occ)){
  
  # Index which rooms belong to which AP
  ind <- which(grepl(colnames(ap_occ)[i],rm_meta$ap)==1)
  # Determine how many occupants their are in the range of that AP
  unaccounted <- ap_occ[,i]
  # Fraction of occupants each room accounts for
  f_occ <- rm_meta$Design.Occupancy[ind]/sum(rm_meta$Design.Occupancy[ind])
  
  # Fill single occupant rooms with one person where appropriate
  single_ind <- which(rm_meta$Design.Occupancy[ind]==1)
  for(j in single_ind){
    temp <- floor(f_occ[j]*unaccounted)
    temp[temp>1] <- 1
    ap_only_occ[,ind[j]+1] <- temp
    unaccounted <- unaccounted-temp
  }
  
  # Fill multi-occupant rooms with persons where appropriate
  multi_ind <- which(rm_meta$Design.Occupancy[ind]>1)
  for(j in multi_ind){
    temp <- floor(f_occ[j]*unaccounted)
    ap_only_occ[,ind[j]+1] <- temp
  }
}

# Reorder the columns from lowest room number to highest room number so each case can be compared
rm_occ <- ap_only_occ[,c(1,c(order(colnames(ap_only_occ[,-1]))+1))]

# ---- Occupant-count estimates using bulk Wi-Fi data only ----

# Create  a blank data frame where all values are zero, using the PIR data as a baseline for simplicity
bulk_occ <- pir
bulk_occ[,-1][ap_only_occ[,-1]!=0] <- 0
bulk_occ[is.na.data.frame(bulk_occ)] <- 0

# Fraction of occupants each room accounts for
f_occ <- rm_meta$Design.Occupancy/sum(rm_meta$Design.Occupancy)

unaccounted <- rowSums(ap_occ[,-1])
  
# Fill single occupant rooms with one person where appropriate
single_ind <- which(rm_meta$Design.Occupancy==1)
for(j in single_ind){
  temp <- floor(f_occ[j]*unaccounted)
  temp[temp>1] <- 1
  bulk_occ[,j+1] <- temp
  unaccounted <- unaccounted-temp
}
  
# Fill multi-occupant rooms with persons where appropriate
multi_ind <- which(rm_meta$Design.Occupancy>1)
for(j in multi_ind){
  temp <- floor(f_occ[j]*unaccounted)
  bulk_occ[,j+1] <- temp
}

# Reorder the columns from lowest room number to highest room number so each case can be compared
rm_occ <- bulk_occ[,c(1,c(order(colnames(bulk_occ[,-1]))+1))]