dev.set(dev.next())
rm(list=ls())
cat("\014")

# ---- Median HVAC Energy Savings and CO2 Exceedances ----

# Determines the median HVAC energy savings and CO2 exceedances based on EnergyPlus results

# Author(s): Brodie W. Hobson
# Last Modified: Wednesday 14 September 2022

#----Load packages----

# Load rstudioapi package
if ("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
require(rstudioapi)
# Load other required packages

# Set working directory----
setwd(dirname(getActiveDocumentContext()$path))

fil_nam <- c("linear_2","linear_3","adjacent_2","adjacent_3")
baseline <- read.csv("./data/results_baseline.csv")
baseline_hvac <- rep(baseline$hc+baseline$cc+baseline$bb+baseline$fn,743)
baseline_co2 <- rep(rowSums(baseline[,grepl("co2",colnames(baseline))]),743)

med_ls <- list()
co2_ls <- list()

for(nam in fil_nam){
  temp <- read.csv(paste0("./data/results_", nam, ".csv"))
  assign(nam,temp)

temp_hvac <- temp$hc+temp$cc+temp$bb+temp$fn
norm_hvac <- (1-temp_hvac/baseline_hvac)*100
temp_co2 <- rowSums(temp[,grepl("co2",colnames(temp))])
norm_co2 <- (1-temp_co2/baseline_co2)*100
norm_co2[is.na(norm_co2)] <- 0

med_hvac <- vector()
occ_hvac <- vector()
med_co2 <- vector()
occ_co2 <- vector()

for(i in c(1:26)){
  for(j in c(1:10)){
    occ_hvac[length(occ_hvac)+1] <- mean(norm_hvac[which(temp$num_zones==i & temp$occSch == j)])
    occ_co2[length(occ_co2)+1] <- mean(norm_co2[which(temp$num_zones==i & temp$occSch == j)])
  }
  med_hvac[length(med_hvac)+1] <- median(occ_hvac)
  med_co2[length(med_co2)+1] <- median(occ_co2)
  occ_hvac <- vector()
  occ_co2 <- vector()
}

med_ls[[length(med_ls)+1]] <- med_hvac
co2_ls[[length(co2_ls)+1]] <- med_co2
rm(temp,med_hvac,norm_hvac,temp_hvac,temp_co2,occ_hvac,occ_co2,norm_co2)
}

med_hvac <- as.data.frame(matrix(unlist(med_ls),byrow=F,ncol=4))
colnames(med_hvac) <- fil_nam
med_co2 <- as.data.frame(matrix(unlist(co2_ls),byrow=F,ncol=4))
colnames(med_co2) <- fil_nam