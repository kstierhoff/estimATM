% Use ERDDAP to download ship position data, then as the ship moves,
% download sardine habitat data around the ship and add to a map that
% updates as the ship moves. That is, the final sardine habitat throughout
% the survey area should be a moving average of the habitat within the ship
% location at that time.
%
% For the 2022 survey, use positions of Lasker, Lisa Marie, Long Beach
% Carnage, and Saildrone

clear; close all;

%% User Settings

% Define vessels
vessels = {'RL' 'LM'};

% Define boundary extents of habitat data to download
latBounds = [27 51];    % [30 40]
lonBounds = [230 247];  % [234 245]

% Define plot boundaries
latPlot = [25 50];
lonPlot = [232 246];

% Define +- decimal degrees for location bounding box
latExtent = 2;
lonExtent = 2;

% Create options structure for webread, specifying options
options = weboptions('Timeout', 60);

% Add path to m_map package and load habitat colormarp
addpath 'C:\Users\josiah.renfree\Documents\MATLAB\m_map'
addpath 'C:\Users\josiah.renfree\Documents\MATLAB\borders'
load('C:\Users\josiah.renfree\Documents\Website\Sardine habitat\Juan cmap.mat');

% Define timespan between ship positions
timespan = hours(6);

% Define animated gif name
surveyName = 'summer2022';

%% Get vessel positions using integrated CSV data

% Initialize variables to hold vessel info
times = cell(length(vessels),1);
lats = cell(length(vessels),1);
lons = cell(length(vessels),1);
groups = cell(length(vessels),1);

for i = 1:length(vessels)
    
    % Cycle through Lasker CSVs
    files = dir(['..\Data\Backscatter\' vessels{i} '\*nasc_cps.csv']);
    h = waitbar(0, ['Reading ' vessels{i} ' CSV files...']);
    for j = 1:length(files)

        % Create import options for that file
        opts = detectImportOptions(fullfile(files(j).folder, files(j).name));
        opts.SelectedVariableNames = ["Date_M", "Time_M", "Lat_M", "Lon_M"];
        opts = setvartype(opts, ["Date_M", "Time_M"], 'string');
        
        % Read CSV file
        NASC = readtable(fullfile(files(j).folder, files(j).name), opts);

        % Keep unique times (i.e., each time can have multiple rows)
        [uniqueTimes, IA] = unique(datetime(strcat(NASC.Date_M, NASC.Time_M), "InputFormat","uuuuMMddHH:mm:ss.SSSS"));
    
        % Extract and store times and positions
        times{i} = [times{i}; uniqueTimes];
        lats{i} = [lats{i}; NASC.Lat_M(IA)];
        lons{i} = [lons{i}; NASC.Lon_M(IA)];
        groups{i} = [groups{i}; j*ones(length(uniqueTimes),1)];
    
        waitbar(j/length(files), h)
    end
    close(h)

    % Sort times
    [times{i}, I] = sort(times{i});
    lats{i} = lats{i}(I);
    lons{i} = lons{i}(I);
    groups{i} = groups{i}(I);

    % Remove any 999s
    idx = lats{i} == 999 | lons{i} == 999;
    times{i}(idx) = [];
    lats{i}(idx) = [];
    lons{i}(idx) = [];
    groups{i}(idx) = [];
end

%% Download habitat data
% Download habitat data for every available time that spans all the times
% from the various vessels

% Get time limits from vessel data
startTime = min(cellfun(@min, times));
endTime = max(cellfun(@max, times));

% Download metadata to obtain last available date 
data = webread('https://coastwatch.pfeg.noaa.gov/erddap/griddap/sardine_habitat_modis_Lon0360.das?potential_habitat%5B(2018-07-31T19:00:00Z):1:(2021-08-17T12:00:00Z)%5D%5B(36.1):1:(36.1)%5D%5B(238.1):1:(238.1)%5D');
coverageEnd = regexp(data, 'time_coverage_end "([^"]*)', 'tokens', 'once');

% Download single pixel over time to get available time stamps
data = webread(['https://coastwatch.pfeg.noaa.gov/erddap/griddap/sardine_habitat_modis_Lon0360.json?potential_habitat%5B' ...
    '(' char(datetime(startTime, 'Format', 'uuuu-MM-dd')) 'T00:00:00Z):1:(' coverageEnd{1} ')%5D%5B' ...
    '(36.1):1:(36.1)%5D%5B' ...
    '(238.1):1:(238.1)%5D'], options);

% Get list of available habitat times
habTimes = cellfun(@(x) x{1}, data.table.rows, 'UniformOutput', false);
habTimes = datetime(habTimes, 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss''Z');

% Only keep times before end date
habTimes(habTimes > endTime) = [];

h = waitbar(0, 'Downloading habitat data...');
for i = 1:length(habTimes)

    % Generate filename
    filename = fullfile(pwd, 'surveyData', [char(datetime(habTimes(i), 'Format', 'uuuuMMdd''T''HHmmss''Z')) '.mat']);

    % If file doesn't exist, download it
    if exist(filename, 'file') == 0
        str = ['https://coastwatch.pfeg.noaa.gov/erddap/griddap/' ...
            'sardine_habitat_modis_Lon0360.mat' ...
            '?potential_habitat%5B(' char(datetime(habTimes(i), 'Format', 'uuuu-MM-dd''T''HH:mm:ss''Z')) ')' ...
            '%5D%5B(' num2str(latBounds(1)) '):(' num2str(latBounds(2)) ')' ...
            '%5D%5B(' num2str(lonBounds(1)) '):(' num2str(lonBounds(2)) ')' ...
            '%5D&.draw=surface&.vars=longitude%7Clatitude%7Cpotential_habitat&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff'];

        websave(filename, str, options);
    end

    waitbar(i/length(habTimes), h)
end
close(h)

%% Plot habitat data
% Need to plot habitat based on positions of potentially multiple survey
% vessels during acoustic transects. We don't want to "double count" the
% habitat for areas where two vessels are concurrently sampling, so need to
% bin them together into time bins then merge appropriately

% Create binned time vector
timeBin = dateshift(startTime,'start','hour'):timespan:dateshift(endTime,'end','hour');

BINS = cell(length(vessels), 1);

% Bin each vessel into that time vector
for i = 1:length(vessels)
    BINS{i} = discretize(times{i}, timeBin);
end

% Create latitude/longitude grid
grid_lat = latBounds(1):0.025:latBounds(2);
grid_lon = lonBounds(1):0.025:lonBounds(2);
[LAT, LON] = ndgrid(grid_lat, grid_lon);

% Initialize matrix for holding averaged habitat data that will be plotted.
% If no datapoints are available, want it to be NaN for plotting purposes.
HAB = zeros(size(LAT));
COUNTS = zeros(size(LAT));  % Used for calculating cumulative average

% Open new figure window
figure;

% Initialize variable to hold filename of currently-loaded habitat file
currFile = '';

% Cycle through ship positions
h = waitbar(0, 'Processing...');
for i = 1:length(timeBin)

    % Find habitat dataset closest in time
    [M,I] = min(abs(habTimes - timeBin(i)));

    % If that's not the currently-loaded habitat file, then load it
    if habTimes(I) ~= currFile
        habData = load(fullfile(pwd, 'surveyData', ...
            [char(datetime(habTimes(I), 'Format', 'uuuuMMdd''T''HHmmss''Z')) '.mat']));
        habData = struct2cell(habData);

        % Parse out data
        lat = habData{1}.latitude;
        lon = habData{1}.longitude;
        hab = squeeze(habData{1}.potential_habitat);

        [latgrid, longrid] = ndgrid(lat, lon);

        currFile = habTimes(I);
    end

    % Cycle through each vessel
    habidx = zeros(size(latgrid));
    for j = 1:length(vessels)

        % Find the intervals that correspond to that bin
        idx = BINS{j} == i;

        % Get average position from those times
        avgLat = mean(lats{j}(idx));
        avgLon = mean(lons{j}(idx));

        % Retain samples within a bounding box around that position
        habidx = habidx | abs(latgrid - avgLat) <= latExtent & ...
            abs(longrid - (360+avgLon)) <= lonExtent;% & ...
            %~isnan(hab);
    end

    % Compute new average for those samples
    HAB(habidx) = (hab(habidx) + COUNTS(habidx).*HAB(habidx)) ./ (COUNTS(habidx) + 1);
    COUNTS(habidx) = COUNTS(habidx) + 1;

    temp = HAB;
%     temp(temp == 0) = NaN;

    % Clear current figure
    clf

    % Create new map projection
    projection = 'Robinson';%'Mollweide'; % 'Lambert Conformal Conic'
    m_proj(projection, ...
        'longitudes', lonPlot, ...
        'latitudes', latPlot);

    % Plot habitat
    m_pcolor(LON, LAT, temp);
    shading flat;
    colormap(map)

    % Set longitude direction appropriately
    m_grid('xlabeldir', 'end'); hold on;

    % Draw outline of states
    [latstates,lonstates] = borders('continental us');
    for j = 1:length(latstates)
        m_plot(360+lonstates{j}, latstates{j}, 'k')
    end

    % Draw outline of Canada
    [latstates,lonstates] = borders('canada');
    m_plot(360+lonstates, latstates, 'k')

    % Draw outline of Mexico
    [latstates,lonstates] = borders('mexico');
    m_plot(360+lonstates, latstates, 'k')

    % Plot shiptrack from beginning to current position
    colors = 'kcmg';
    for j = 1:length(vessels)

        % Get index of intervals (times) up to the current time bin
        idx = find(times{j} <= timeBin(i)+timespan, 1, 'last');
        X = 360+lons{j}(1:idx);
        Y = lats{j}(1:idx);
        G = groups{j}(1:idx);

        % Plot all the transects up to the current time bin. Plot them
        % separately for each transect so it only shows transects
        [C,IA,IC] = unique(G, 'stable');
        for k = 1:length(C)
            m_plot(X(IC==k), Y(IC==k), 'Linewidth', 2, 'Color', colors(j))
        end
    end

    % Set color limit then add colorbar
    clim(gca, [0 1])                   % Set color limits to 0 and 1
    ax = gca;
    hcb = colorbar('peer', ax);         % Add colorbar

    % Set figure labels
    set(gcf,'color','w');           % Change background color to white
    set(gcf, 'PaperUnits', 'points', 'PaperPosition', [0 0 322.5 315]);
    set(gcf, 'Position', [0 0 1120 850]);
    set(ax, 'Position', [0 0.1 0.8 0.85])
    set(hcb, 'Position', [0.73 0.1 0.0476 0.85])

    set(hcb, 'YAxisLocation', 'left')
    set(hcb, 'YTick', [0 .048 .32 .45 1])
    set(hcb, 'YtickLabel', {'0' '1' '10' '20' '100'})
    ylabel(hcb, 'Cumulative sardine biomass (%)')
    annotation(gcf, 'textbox', [0.78 0.67 0.049 0.054], ...
        'String', {'Optimal'}, 'FitBoxToText', 'off', 'EdgeColor','none');
    annotation(gcf, 'textbox', [0.78 0.39 0.049 0.054], ...
        'String', {'Good'}, 'FitBoxToText', 'off', 'EdgeColor','none');
    annotation(gcf, 'textbox', [0.78 0.22 0.049 0.054], ...
        'String', {'Bad'}, 'FitBoxToText', 'off', 'EdgeColor','none');
    annotation(gcf, 'textbox', [0.78 0.08 0.049 0.054], ...
        'String', {'Unsuitable'}, 'FitBoxToText', 'off', 'EdgeColor','none');

    % Create new invisible axis for holding colorbar label text
    axes('position', [0 0 1 1], 'Visible', 'off');

    % Add label to colorbar
    text(.87, .45, 'Potential habitat', 'Rotation', 90)

    % Write to animated gif
    %     frame = getframe(gcf);
    %     im = frame2im(frame);
    %     [imind,cm] = rgb2ind(im,256);
    %
    %     if i == 1
    %         imwrite(imind, cm, fullfile(pwd, 'surveyData', [surveyName '.gif']),'gif','LoopCount',0,'DelayTime',.1);
    % %     elseif i == length(habTimes)
    % %         imwrite(imind, cm, fullfile(pwd, 'surveyData', gifName),'gif','WriteMode','append','DelayTime',2);
    %     else
    %         imwrite(imind, cm, fullfile(pwd, 'surveyData', [surveyName '.gif']),'gif','WriteMode','append','DelayTime',.1);
    %     end

    waitbar(i/length(timeBin), h, char(timeBin(i)))
end
close(h)

% Save final figure
print(fullfile(pwd, 'surveyData', [surveyName '.png']), '-dpng')