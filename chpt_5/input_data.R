dev.set(dev.next())
rm(list=ls())
cat("\014")

# ---- Simulation Input Data ----

# Determines which zones will have which sensors based on RNG

# Author(s): Brodie W. Hobson
# Last Modified: Tuesday 23 August 2022

#----Load packages----

# Load rstudioapi package
if ("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
require(rstudioapi)
# Load other required packages

# Set working directory----
setwd(dirname(getActiveDocumentContext()$path))

rm_nams <- read.csv("./data/rm_nam.csv")

# Create random parameter values for all simulations
rand_sensor_ls <- list()

zone_count <- 26
iteration_count <- 30

for(num_zones in c(1:zone_count)){
  # Pseudo-random number generation for sensor placement
  # Less than 30 combinations for single zone sensed
  if(num_zones == 1){
    for(zone in c(1:zone_count)){
      rand_sensor <- c(rep(0,zone-1),2,rep(0,26-zone))
      rand_sensor_ls[[length(rand_sensor_ls)+1]] <- c(num_zones, zone, rand_sensor)
    }
  }else if(num_zones == 25){
    for(zone in c(1:zone_count)){
      rand_sensor <- c(rep(2,zone-1),0,rep(2,26-zone))
      rand_sensor_ls[[length(rand_sensor_ls)+1]] <- c(num_zones, zone, rand_sensor)
    }
  }else if(num_zones == 26){
    rand_sensor <- rep(2,26)
    rand_sensor_ls[[length(rand_sensor_ls)+1]] <- c(num_zones, 1, rand_sensor)
  }else{
    rand_all <- t(combn(26,num_zones))
    set.seed(length(rand_sensor_ls))
    rand_zone <- rand_all[sample(nrow(rand_all),iteration_count,replace=F),]
    for(num_iteration in c(1:iteration_count)){
      rand_sensor <- rep(0,26)
      rand_sensor[rand_zone[num_iteration,]] <- 2
      rand_sensor_ls[[length(rand_sensor_ls)+1]] <- c(num_zones,num_iteration,rand_sensor)
    }
  }
}
    
input_data <- as.data.frame(matrix(unlist(rand_sensor_ls),byrow=T,ncol=zone_count+2))

colnames(input_data) <- c("num_zones","num_iterations",rm_nams$x)

#write.csv(input_data,"./data/input_data.csv",row.names = F)