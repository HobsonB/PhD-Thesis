Name:	buildingB_occupancy_2020.R
Description:	Use available Wi-Fi data from the first half of 2020 to extrapolate out a timeseries occupancy schedule for use in EnergyPlus simulation of building B
Requires:	buildingA_wifi_anon.csv, buildingB_wifi_anon.csv
	
Name:	calibration.m
Description:	Upper and lower bounds for calibration of building B energy model
Requires:	buildingB_2020_hourly.csv
	
Name:	calibration_analysis.R
Description:	Check calibrated energy model and ECM results
Requires:	buildingA_2019.csv, buildingA_2020.csv, buildingB_2020.csv, buildingB_2019.csv, 2020_baseMeter.csv
	
Name:	changepoint.R
Description:	Determine how energy use changes with occupancy (focus on period after COVID-19 pandemic) 
Requires:	buildingA_2019.csv, buildingB_2019.csv, oat_2019.csv, buildingA_2020.csv, buildingB_2020.csv, oat_2020.csv
	
Name:	objective.m
Description:	Define genetic algorithm and cost function to minimize for energy model calibration
Requires:	ON_OTTAWA-INTL-ONT_716280_20.txt, model_tune.idf, calibration.m, parm_find.txt
