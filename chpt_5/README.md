Name:	datInfo.R
Description:	Determines whether each day is a workday and what the temperature setpoint should be based on date info, for use as CSV input into EnergyPlus
Requires:	
	
Name:	euc_mx.R
Description:	Determines the Euclidean distance between room area centroids for use in determining room adjacency for control
Requires:	rm_centroid.csv
	
Name:	input_data.csv
Description:	Determines which zones will have which sensors based on RNG
Requires:	rm_nam.csv
	
Name:	median_savings.R
Description:	Determines the median HVAC energy savings and CO2 exceedances based on EnergyPlus results
Requires:	results_linear_2.csv, resuls_linear_3.csv, results_adjacent_2.csv, results_adjacent_3.csv, results_baseline.csv
	
Name:	sample_sizing.R
Description:	Determines the number of simulations required to get an adequate sampling of all possible sensor combinations based on preliminary sizing run of 100 EnergyPlus simulations
Requires:	results.csv
	
Name:	occSch.R
Description:	Creates 10 unique occupancy schedules for the floor based off of collection of available zones
Requires:	room_occupancies.csv
	
Name:	pareto.R
Description:	Pareto front analysis based on priorities of energy use, CO2 exceedances, and number of sensors from bulk results
Requires:	results_linear_2.csv, resuls_linear_3.csv, results_adjacent_2.csv, results_adjacent_3.csv, results_baseline.csv
	
Name:	run_baseline.R
Description:	Runs the baseline EnergyPlus simulations for all ten occupancy scenarios using eplusr
Requires:	rm_nam.csv, datInfo.csv, occSch_1.csv, ccSch_2.csv, ccSch_3.csv, ccSch_4.csv, ccSch_5.csv, ccSch_6.csv, ccSch_7.csv, ccSch_8.csv, ccSch_9.csv, ccSch_10.csv, ON_OTTAWA-INTL-ONT_716280_19.txt, baseline.idf
	
Name:	run_parallel_adjacent.R
Description:	Runs parallelized EnergyPlus simulations for all ten occupancy scenarios using eplusr with adjacent zone control
Requires:	rm_nam.csv, datInfo.csv, euc_mx.csv, Vp.csv, rm_area.csv, input_data.csv, occSch_1.csv, ccSch_2.csv, ccSch_3.csv, ccSch_4.csv, ccSch_5.csv, ccSch_6.csv, ccSch_7.csv, ccSch_8.csv, ccSch_9.csv, ccSch_10.csv, ON_OTTAWA-INTL-ONT_716280_19.txt, model_adjacent.idf
	
Name:	run_parallel_individual.R
Description:	Runs parallelized EnergyPlus simulations for all ten occupancy scenarios using eplusr with individual zone control
Requires:	rm_nam.csv, datInfo.csv, input_data.csv, occSch_1.csv, ccSch_2.csv, ccSch_3.csv, ccSch_4.csv, ccSch_5.csv, ccSch_6.csv, ccSch_7.csv, ccSch_8.csv, ccSch_9.csv, ccSch_10.csv, ON_OTTAWA-INTL-ONT_716280_19.txt, model_individual.idf