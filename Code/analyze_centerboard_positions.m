% Find list of log files
files = dir('..\Data\SCS\Centerboard\**\*.CALC.log');

cb_times = [];
cb_depths = [];

% Cycle through each file
for i = 1:length(files)

    % Read contents of file
    T = readtable(fullfile(files(i).folder, files(i).name));

    % Convert first column to datetime
    cb_times = [cb_times; datetime(T{:,1}, 'InputFormat','uuuu-MM-dd''T''HH:mm:ss.SSSZ', 'TimeZone','UTC')];
    cb_depths = [cb_depths; T{:,4}];
end

% Perform basic filtering
idx = cb_depths > 12 | cb_depths < 0;
cb_times(idx) = [];
cb_depths(idx) = [];

figure
plot(cb_times, cb_depths, '.-')