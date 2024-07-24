dev.set(dev.next())
rm(list=ls())
cat("\014")

# ---- Run Baseline EnergyPlus Simulations ----

# Runs the baseline EnergyPlus simulations for all ten occupancy scenarios using eplusr

# Author(s): Brodie W. Hobson
# Last Modified: Monday 26 September 2022

# ----Load packages----

# Load rstudioapi package
if ("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
require(rstudioapi)
# Load other packages
if ("eplusr" %in% rownames(installed.packages()) == FALSE) {install.packages("eplusr")}
require(eplusr)
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("BBmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("BBmisc")}
require(BBmisc)
#RTools42 is required but has its own installer; this line ensures it is added correctly to PATH
#writeLines('PATH="${RTOOLS42_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

# ---- Set working directory ----
setwd(dirname(getActiveDocumentContext()$path))

# ---- Specify paths and static values ----

# Load in room names
rm_nam <- as.vector(t(read.csv("./data/rm_nam.csv")))
# Create index for operating hours (determined using datStr script based on simulation start, stop, and timestep size)
ind <- as.vector(t(read.csv("./data/datInfo.csv")[2]))
# Extract zone air temperature setpoints based on date data (determined using datStr script based on simulation start, stop, and timestep size)
T_set <- as.vector(t(read.csv("./data/datInfo.csv")[3]))

# Path of original IDF that serves as baseline model (not to be modified)
idf_path <- "./data"
# Path for weather file
epw_path <- "./data/ON_OTTAWA-INTL-ONT_716280_19.epw"
# Path for where original file is to be copied and ran (to be modified)
temp_path <- "C:/EnergyPlusV9-5-0/ExampleFiles"
# Path where occupancy schedules are stored
occ_path <- "C:/EnergyPlusV9-5-0/ExampleFiles"

# Load in original IDF
idf <- Idf$new(paste0(idf_path,"/baseline.idf"))

# ---- Loops for simulation iterations ----

# Save original IDF as temporary file for overwriting
idf$save(path=paste0(temp_path,"/temp.idf"),copy_external = F,overwrite=T)

# Open temporary IDF 
temp_idf <- Idf$new(paste0(temp_path,"/temp.idf"))

num_zones <- 0
num_iterations <- 1
rand_sensor <- rep(0,26)

# Loop for simulation for the 10 occupancy scenarios
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
  
  results <- c(num_zones, # Record number of sensed zones
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
  
  # Write transposed results to new line in results file after each and every simulation
  #write.table(t(results),paste0(idf_path,"/results.csv"),row.names = F, col.names = F,sep = ",",append = T)
} # close occupancy schedule loop