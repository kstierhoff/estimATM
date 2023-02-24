% Estimate the potential habitat for northern stock pacific sardine at the
% times and locations of each trawl cluster. To do so, download the nearest
% SST and Chlorophyll data, retain samples within 0.2-degrees latitude and
% longitude from the cluster location, compute the average SST and
% Chlorophyll, then derive the potential habitat.

%% Read trawl cluster info

CLUSTER = readtable("..\Data\Trawl\clusters_for_habitat_analysis.csv");

%% Create habitat interpolant

% Read look-up table
MDL = readtable("..\Data\Habitat\sardineHabitatModel_20230223.csv");

% Get unique SST and Chlorophyll values for gridded interpolant
MDL_sst = unique(MDL.temp);
MDL_logchl = unique(MDL.log_chl);

% Create grid
[X,Y] = ndgrid(MDL_sst, MDL_logchl);
Z = reshape(MDL.sardine_presence,size(X));

% Create gridded interpolant
F_extrap = griddedInterpolant(X,Y,Z);
F = griddedInterpolant(X,Y,Z,'linear','none');

%% Estimate habitat

HAB = nan(size(CLUSTER,1),1);
HAB_extrap = HAB;

% Cycle through each cluster
for i = 1:size(CLUSTER,1)

    % Get latitude and longitude bounds
    latBounds = [-0.2 0.2] + CLUSTER.lat(i);
    lonBounds = [-0.2 0.2] + CLUSTER.long(i) + 360;

    % Get SST data
    str = ['https://coastwatch.pfeg.noaa.gov/erddap/griddap/' ...
        'erdMWsstd8day.json' ...
        '?sst%5B(' CLUSTER.datetime{i} ')' ...
        '%5D%5B(0.0):1:(0.0)' ...
        '%5D%5B(' num2str(latBounds(1)) '):(' num2str(latBounds(2)) ')' ...
        '%5D%5B(' num2str(lonBounds(1)) '):(' num2str(lonBounds(2)) ')%5D'];
    data = webread(str);

    % Parse out SST
    SST = cell2mat(cellfun(@(x) x{5}, data.table.rows, 'UniformOutput', false));

    % Get Chlorophyll data
    str = ['https://coastwatch.pfeg.noaa.gov/erddap/griddap/' ...
        'erdMWchla8day.json' ...
        '?chlorophyll%5B(' CLUSTER.datetime{i} ')' ...
        '%5D%5B(0.0):1:(0.0)' ...
        '%5D%5B(' num2str(latBounds(1)) '):(' num2str(latBounds(2)) ')' ...
        '%5D%5B(' num2str(lonBounds(1)) '):(' num2str(lonBounds(2)) ')%5D'];
    data = webread(str);

    % Parse out SST
    CHL = cell2mat(cellfun(@(x) x{5}, data.table.rows, 'UniformOutput', false));

    % Compute mean SST and CHL
    mean_SST = mean(SST);
    mean_CHL = mean(CHL);

    % Estimate habitat
    HAB(i) = F(mean_SST, log(mean_CHL));
    HAB_extrap(i) = F_extrap(mean_SST, log(mean_CHL));

end

CLUSTER.Habitat = HAB;
CLUSTER.Habitat_extrap = HAB_extrap;

writetable(CLUSTER, "..\Data\Trawl\clusters_with_habitat_extrap.csv")