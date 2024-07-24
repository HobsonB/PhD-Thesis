%% read metered energy data
num = xlsread('RO_2020_hourly.csv');
meteredEnergy.cooling = num(3266:6049,2);
meteredEnergy.heating = num([1:3265 7056:8784],3);

%% define ga hyperparameters

    % [,1] #U-value# - search range 2 to 4 W/m2-K 
    % [,2] #shgc# - search range 0.3 to 0.8 
    % [,3] #thickness# - search range 2 to 6 m2-K/W
    % [,4] #infiltration# - search range 0.1 to 2.5 L/s-m2 
    % [,5] #ventilation# - search range 0.3 to 2.5 L/s-m2
    % [,6] #tSaHtg1# - search range 15 to 22 degC
    % [,7] #tSaHtg2# - search range 15 to 22 degC
    % [,8] #tSaClg1# - search range 10 to 15 degC
    % [,9] #tSaClg2# - search range 10 to 15 degC
    % [,10] #tOaLow1# - search range -10 to 2
    % [,11] #tOaLow2# - search range -10 to 2
    % [,12] #tOaHigh1# - search range 5 to 12
    % [,13] #tOaHigh2# - search range 5 to 12
    % [,14] #lightPlugMax# - search range 8 to 30 W/m2
    % [,15] #lightPlugMin# - search range 1 to 20 W/m2   
    
%      1    2    3    4    5    6   7   8   9   10   11   12  13   14   15    
lb = [2.0, 0.3, 2.0, 0.1, 0.3, 15, 15, 10, 10, -10, -10, 5.0, 5.0, 8.0, 1.0];
ub = [4.0, 0.8, 6.0, 2.5, 2.5, 22, 22, 15, 15, 2.0, 2.0, 12.0, 12.0, 30, 20];

opts = optimoptions('ga','PlotFcn',@gaplotbestf,'CrossoverFrac',0.5,'PopulationSize',30,'StallGen',14,'Generations',14);
[x,fval,exitflag] = ga(@(x)objective(x,meteredEnergy),length(lb),[],[],[],[],lb,ub,[],[],opts);