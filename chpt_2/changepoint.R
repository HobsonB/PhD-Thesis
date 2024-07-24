dev.set(dev.next())
rm(list=ls())
cat("\014")

#----Building A and B Energy & Wi-Fi Comparison----

# Determine how energy use changes with occupancy (focus on period after COVID-19 pandemic) 

# Date: Thursday 22 July 2021
# Author(s): Brodie W. Hobson

#----Load packages----

if ("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
require(rstudioapi)
if ("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
require(dplyr)
if ("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
require(data.table)
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}
require(matrixStats)
if ("DescTools" %in% rownames(installed.packages()) == FALSE) {install.packages("DescTools")}
require(DescTools)

#----Set working directory----

# Set working directory----
setwd(dirname(getActiveDocumentContext()$path))

#----Figures----

# Read in .csv files
buildingB <- read.csv("./data/buildingB_2019.csv")
buildingA <- read.csv("./data/buildingA_2019.csv")
oat <- read.csv("./data/oat_2019.csv")

#Specify date format used in .csv
date_format = "%Y-%m-%d %H:%M"

buildingB[,1] <- as.POSIXct(buildingB[,1], format = date_format, tz = "GMT")
buildingA[,1] <- as.POSIXct(buildingA[,1], format = date_format, tz = "GMT")
oat[,1] <- as.POSIXct(oat[,1], format = date_format, tz = "GMT")

buildingB_cw <- buildingB$cw_kWh
buildingB_stm <- buildingB$stm_kWheq
buildingA_cw <- buildingA$cw_kWh
buildingA_stm <- buildingA$stm_kWheq

oat <- oat$Tout
datStr <- buildingB$Timestamp
dat_ind <- which(hour(datStr) %in% c(9:17))

for(j in c(1:2)){
  
  if(j ==1){
    
    bldg_nam <- "buildingA"
    stm <- buildingA_stm[dat_ind]
    cw <- buildingA_cw[dat_ind]
    Tout <- oat[dat_ind]
    
  } else if(j == 2){
    
    bldg_nam <- "buildingB"
    stm <- buildingB_stm[dat_ind]
    cw <- buildingB_cw[dat_ind]
    Tout <- oat[dat_ind]
      
  }
  
  #----Heating Changepoint----
  
  r2s <- list()
  
  for(i in seq(5,18,0.1)){
    
    ind <- which(Tout < i)
    x <- Tout[ind]
    y <- stm[ind]
    mdl <- lm(I(y-0) ~ I(x-i)+0)
    r2s[[length(r2s)+1]] <- summary(mdl)$r.squared
    
  }
  rm(i,x,y,mdl,ind)
  
  #Find changepoint
  heat_chgpt <- seq(5,18,0.1)[which.max(unlist(r2s))]
  
  #Train fit
  ind <- which(Tout < heat_chgpt)
  x <- Tout[ind]
  y <- stm[ind]
  heat_mdl <- lm(I(y-0) ~ I(x-heat_chgpt)+0)
  
  #Extract fit parameters for plot
  heat_r2.fit <- round(summary(heat_mdl)$r.squared, digits = 2)
  heat_m.fit <- round(summary(heat_mdl)$coefficients[1], digits = 0)
  heat_y.fit <- predict(heat_mdl, newdata = data.frame(x = seq(-30,30,0.5)))
  heat_b.fit <- round(heat_y.fit[which(seq(-30,30,0.5)==0)], digits = 0)
  
  heat_mdl.fit <- predict(heat_mdl, newdata = data.frame(x = Tout[ind]))
  heat_mdl.fit[which(heat_mdl.fit<0)] <- 0
  
  heat_rmse <- round(((RMSE(x = heat_mdl.fit[which(y >10)], ref = y[which(y >10)])/mean(y[which(y >10)]))*100), digits = 0)
  
  #----Cooling changepoint----
  
  r2s <- list()
  
  for(i in seq(5,18,0.5)){
    
    ind <- which(Tout > i)
    x <- Tout[ind]
    y <- cw[ind]
    mdl <- lm(I(y-0) ~ I(x-i)+0)
    r2s[[length(r2s)+1]] <- summary(mdl)$r.squared
    
  }
  rm(i,x,y,mdl,ind)
  
  #Find changepoint
  cool_chgpt <- seq(5,18,0.5)[which.max(unlist(r2s))]
  
  #Train fit
  ind <- which(Tout > cool_chgpt)
  x <- Tout[ind]
  y <- cw[ind]
  cool_mdl <- lm(I(y-0) ~ I(x-cool_chgpt)+0)
  
  #Extract fit parameters for plot
  cool_r2.fit <- round(summary(cool_mdl)$r.squared, digits = 2)
  cool_m.fit <- round(summary(cool_mdl)$coefficients[1], digits = 0)
  cool_y.fit <- predict(cool_mdl, newdata = data.frame(x = seq(-30,30,0.5)))
  cool_b.fit <- round(cool_y.fit[which(seq(-30,30,0.5)==0)], digits = 0)
  
  cool_mdl.fit <- predict(cool_mdl, newdata = data.frame(x = Tout[ind]))
  cool_mdl.fit[which(cool_mdl.fit<0)] <- 0
  
  cool_rmse <- round(((RMSE(x = cool_mdl.fit[which(y >10)], ref = y[which(y >10)])/mean(y[which(y >10)]))*100), digits = 0)
}