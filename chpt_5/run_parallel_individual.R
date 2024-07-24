rm(list=ls())
cat("\014")

# ---- Run Parallelized EnergyPlus Simulations  ----

# Runs parallelized EnergyPlus simulations for all ten occupancy scenarios using eplusr with individual zone control

# Author(s): Brodie W. Hobson
# Last Modified: Wednesday 19 July 2023

# ----Load packages----

if ("eplusr" %in% rownames(installed.packages()) == FALSE) {install.packages("eplusr")}
require(eplusr)
if ("BBmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("BBmisc")}
require(BBmisc)
if ("parallel" %in% rownames(installed.packages()) == FALSE) {install.packages("parallel")}
require(parallel)

# Number of simulations to run at the same time
# Default to number of CPU cores (e.g. 32 on VM)
num_cores <- detectCores()

# ---- Specify paths and static values ----

# Load in room names
rm_nam <- as.vector(t(read.csv("./rm_nam.csv")))
# Create index for operating hours (determined using datStr script based on simulation start, stop, and timestep size)
ind <- as.vector(t(read.csv("./datInfo.csv")[2]))
# Extract zone air temperature setpoints based on date data (determined using datStr script based on simulation start, stop, and timestep size)
T_set <- as.vector(t(read.csv("./datInfo.csv")[3]))
# Load in input data for sensor locations
input_data_df <- read.csv("./input_data.csv")
# Turn the input data back into a list format like Ryan had so I don't have to switch around the rest of the script
input_data <- vector(mode="list", length=nrow(input_data_df))
for(i in c(1:nrow(input_data_df))){
  input_datum <- list(num_zones = input_data_df$num_zones[i],
                      num_iterations = input_data_df$num_iterations[i],
                      rand_sensor = as.numeric(input_data_df[i,c(3:28)]))
  input_data[[i]] <- input_datum
}
# Remove the input data csv that has now been converted to a list
rm(input_data_df)

# Path of original IDF that serves as baseline model (not to be modified)
idf_path <- getwd()
# Path for weather file
epw_path <- "./ON_OTTAWA-INTL-ONT_716280_19.epw"
# Path where occupancy schedules are stored
occ_path <- "./occSch"

# ---- Loops for simulation iterations ----

run_simulations <- function(input_datum) {
  # Pre-calculated inputs for this simulation
  rand_sensor <- input_datum$rand_sensor
  num_zones <- input_datum$num_zones
  num_iterations <- input_datum$num_iterations
    
    # Unique temporary path for each simulation, so they can run at the same time
    temp_path <- paste0("./temp_", num_zones, "_", num_iterations)
  
    # Save original IDF as temporary file for overwriting
    idf <- Idf$new(paste0(idf_path,"/model.idf"))
    idf$save(path=paste0(temp_path,"/temp.idf"),copy_external = F,overwrite=T)
    
    # Open temporary IDF 
    temp_idf <- Idf$new(paste0(temp_path,"/temp.idf"))
    
    # Replace function in TrimResPG with relevant subroutine for evaluation
    for(i in c(1:length(rm_nam))){
      temp_idf$replace_value(paste0("#fun",rm_nam[i],"#"),paste0("TrimResFun_",rand_sensor[i]))
    }
    
    # Save changes to the temporary IDF before simulation is actually run
    temp_idf$save(path=paste0(temp_path,"/temp.idf"),copy_external = F,overwrite=T)
    
    # Loop for simulation for the 10 occupancy scenarios
    results <- matrix(, nrow=0, ncol=9+3*zone_count)
    
    for(occSch in c(1:10)){
      
      # ---- Simulation run ----
      
      # Replace the occupancy file in the folder with the correct occupancy file
      file.copy(from = paste0(occ_path,"/occSch_",occSch,".csv"),to = temp_path)
      # Rename the occupancy file (i.e., remove the number) so that EnergyPlus can recognize and run it
      file.rename(from = paste0(temp_path,"/occSch_",occSch,".csv"),to = paste0(temp_path,"/occSch.csv"))
      
      # Create job to run modified IDF and save results in temporary folder
      job <- temp_idf$run(weather=epw_path,dir=temp_path,copy_external=T)

      # ---- Results recording ----

      # Load in the results for analysis
      df <- read.csv(paste0(temp_path,"/temp.csv"))
      occ <- df[ind,grepl("Zone.People.Occupant.Count",colnames(df))] # Data frame with occupancy data
      occ[occ!=0] <- 1 # Change occupancy data to binary occupancy data for multiplication purposes

      row <- c(num_zones, # Record number of sensed zones
               num_iterations, # Record iteration number
               occSch, # Record occupancy schedule used
               sum(df$HeatingCoils.EnergyTransfer)/1e9, # Record total heating coil usage (GJ)
               sum(df$CoolingCoils.EnergyTransfer)/1e9, # Record total cooling coil usage (GJ)
               sum(df$Baseboard.EnergyTransfer)/1e9, # Record total baseboard usage (GJ)
               sum(df$Fans.Electricity)/1e9, # Record total fan usage (GJ)
               sum(df$InteriorLights.Electricity)/1e9, # Record total lighting usage (GJ)
               sum(df$InteriorEquipment.Electricity)/1e9, # Record total equipment (i.e., plug) usage (GJ)
               rand_sensor, # Record sensor placement
               as.vector(colSums((df[ind,grepl("Zone.Air.CO2.Concentration",colnames(df))]*occ)-1000 > 0)/4), # Record number of occupied hours CO2 > 1000 ppm in each zone
               as.vector(rowSums((abs(sweep(t(df[ind,grepl("Zone.Mean.Air.Temperature",colnames(df))]),2,T_set[ind]))*t(occ)>1.8)/4))) # Record number of occupied hours zone air temperature > zone air temperature setpoint by threshold in each zone

      # Append results row to matrix      
      results <- rbind(results, row)
    }
    
    # Delete temp folders to save disk space
    unlink(temp_path, recursive=TRUE)
    
    # Return 10 rows for this simulation
    return(results)
}

zone_count = 26

# # Do fewer simulations (TESTING ONLY)
#input_data <- sample(input_data, 1)

# Start parallel workers
cl <- makeCluster(num_cores, outfile="")

# Copy global constants to all parallel workers
clusterExport(cl, "idf_path")
clusterExport(cl, "epw_path")
clusterExport(cl, "occ_path")
clusterExport(cl, "rm_nam")
clusterExport(cl, "ind")
clusterExport(cl, "T_set")
clusterExport(cl, "zone_count")

# Load R packages on all parallel workers
clusterEvalQ(cl, library(eplusr))
clusterEvalQ(cl, library(BBmisc))

# Run simulations on parallel workers using the previously generated parameter values
results <- parLapply(cl, input_data, run_simulations)
results <- do.call(rbind, results)

# Stop parallel workers
stopCluster(cl)

# Write results to to results file after all simulations are complete
#write.table(results, paste0(idf_path,"/results.csv"),row.names = F,col.names = F,sep = ",",append = T)