clear
close all

%% Might neet to set the path
% PATH = getenv('PATH');
% setenv('PATH', [PATH ':/opt/local/bin']);

dir = 'http://130.95.29.56:8080/thredds/fileServer/roms/ozroms_history/';
%ozroms_surface_Sep_2014.nc


yr = 2007:2014;

for m = 5
    for y = 1:length(yr)
        
        [~,mm] = month(datenum(yr(y),m,1));
        eval(['!wget ',dir,'ozroms_surface_',mm,'_',num2str(yr(y)),'.nc'])
        
    end
end
