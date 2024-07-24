function misfit = objective(x,meteredEnergy)

    % define an objective function for ga to minimize

    weather = {'ON_OTTAWA-INTL-ONT_716280_20'}; 

    thermalConductivity = 0.03; % W/m-K (taken from IDF material properties & a constant & change thickness in the IDF to change R-value)

    uValue = x(1); % #U-value# - search range 1.5 to 3 W/m2-K
    shgc = x(2); % #shgc# - search range 0.3 to 0.6 
    rValue =  x(3); % #thickness# - search range 2 to 5 m2-K/W
    infiltration = x(4); % #infiltration# - search range 0.1 to 2.5 L/s-m2 
    ventilation = x(5); % #ventilation# - search range 0.3 to 2.5 L/s-m2
    tSaHtg1 = x(6); % #tSaHtg1# - search range 15 to 22 degC
    tSaHtg2 = x(7); % #tSaHtg2# - search range 15 to 22 degC
    tSaClg1 = x(8); % #tSaClg1# - search range 10 to 15 degC
    tSaClg2 = x(9); % #tSaClg2# - search range 10 to 15 degC
    tOaLow1 = x(10); % #tOaLow1# - search range -10 to 2
    tOaLow2 = x(11); % #tOaLow2# - search range -10 to 2
    tOaHigh1 = x(12); % #tOaHigh1# - search range 5 to 12
    tOaHigh2 = x(13); % #tOaHigh2# - search range 5 to 12
    lightPlugMax = x(14); % #lightPlugMax# - search range 8 to 30 W/m2
    lightPlugMin = x(15); % #lightPlugMin# - search range 1 to 20 W/m2
    
    strDirectory = pwd; % get the current directory name
    
    cd (strDirectory)
        copyfile('model_tune.idf','\EnergyPlus\ExampleFiles\') % create a copy of the idf file in the EnergyPlus directory
    cd \EnergyPlus\ExampleFiles\ % change directory to EnergyPlus directory
        EnergyPlusBatch = strcat('C:\EnergyPlus\RunEPlus.bat',{' '},'temp',{' '} ,weather);
    cd \EnergyPlus\ExampleFiles\ % change directory to EnergyPlus directory
        fidInFile = fopen('model_tune.idf','r'); % open and read the idf file
        fidOutFile = fopen('temp.idf','w'); % create and write a new temporary idf file

    % Make changes in the EnergyPlus model
    nextLine = fgets([fidInFile]);
    while nextLine >= 0  %# Loop until getting -1 (end of file)       
        nextLine = strrep(nextLine,'#U-value#',num2str(uValue)); %1
        nextLine = strrep(nextLine,'#shgc#',num2str(shgc)); %2
        nextLine = strrep(nextLine,'#thickness#',num2str(rValue*thermalConductivity)); %3, R-value * k = #thickness# 
        nextLine = strrep(nextLine,'#infiltration#',num2str(infiltration/1000)); %4
        nextLine = strrep(nextLine,'#ventilation#',num2str(ventilation/1000)); %5
        nextLine = strrep(nextLine,'#tSaHtg1#',num2str(tSaHtg1)); %6
        nextLine = strrep(nextLine,'#tSaHtg2#',num2str(tSaHtg2)); %7
        nextLine = strrep(nextLine,'#tSaClg1#',num2str(tSaClg1)); %8
        nextLine = strrep(nextLine,'#tSaClg2#',num2str(tSaClg2)); %9  
        nextLine = strrep(nextLine,'#tOaLow1#',num2str(tOaLow1)); %10
        nextLine = strrep(nextLine,'#tOaLow2#',num2str(tOaLow2)); %11
        nextLine = strrep(nextLine,'#tOaHigh1#',num2str(tOaHigh1)); %12
        nextLine = strrep(nextLine,'#tOaHigh2#',num2str(tOaHigh2)); %13
        nextLine = strrep(nextLine,'#lightPlugMax#',num2str(lightPlugMax)); %14
        nextLine = strrep(nextLine,'#lightPlugMin#',num2str(lightPlugMin)); %15
        fprintf(fidOutFile,'%s',nextLine);        %# Write the line to the output file   
        nextLine = fgets(fidInFile);              %# Get the next line of input
    end

    fclose(fidInFile);                          %# Close the input file
    fclose(fidOutFile);                         %# Close the output file

    % Run the EnergyPlus Model
    cd \EnergyPlus
    dos(strjoin(EnergyPlusBatch));

    % Extract results from the EnergyPlus model
    cd \EnergyPlus\ExampleFiles\Outputs

    % extract EUI from the summary table
    num = xlsread('tempMeter.csv');

    heating = sum(num([1:3265 7056:8784],[7 9]),2)/3600000;
    cooling = num(3266:6049,8)/3600000;
    
    % calculate the average CV(RMSE)
    misfit = mean([sqrt(mean(power(meteredEnergy.heating - heating,2)))/mean(meteredEnergy.heating),...
                  sqrt(mean(power(meteredEnergy.cooling - cooling,2)))/mean(meteredEnergy.cooling)]);
    
    cd (strDirectory)
end