clear
close all

% http://apdrc.soest.hawaii.edu/datadoc/ofes/ncep_0.1_global_3day.php

%% Get Limits

lat = ncread('http://apdrc.soest.hawaii.edu:80/dods/public_ofes/OfES/ncep_0.1_global_3day/temp','lat');
lon = ncread('http://apdrc.soest.hawaii.edu:80/dods/public_ofes/OfES/ncep_0.1_global_3day/temp','lon');


fi_lat = find(lat >= -50 & lat <= 10);
fi_lon = find(lon >= 90 & lon <= 180);

lat = lat(fi_lat);
lon = lon(fi_lon);

time = ncread('http://apdrc.soest.hawaii.edu:80/dods/public_ofes/OfES/ncep_0.1_global_3day/temp','time');
time = time + datenum(1,1,1,0,0,0) - 2;

depth = ncread('http://apdrc.soest.hawaii.edu:80/dods/public_ofes/OfES/ncep_0.1_global_3day/temp','lev');

save /Volumes/WD_MYBOOK/Satellite/OFES/Grids.mat depth lat lon time

clear depth



for i = 2605:length(time)
    disp(['Downloading ',datestr(time(i))])
    OFES.datenum = time(i);
    OFES.datestr = datestr(time(i));
    OFES.SSH = squeeze(ncread('http://apdrc.soest.hawaii.edu:80/dods/public_ofes/OfES/ncep_0.1_global_3day/eta','eta',[fi_lon(1) fi_lat(1) 1 i],[length(fi_lon) length(fi_lat) Inf 1]));
    OFES.BLayer = squeeze(ncread('http://apdrc.soest.hawaii.edu:80/dods/public_ofes/OfES/ncep_0.1_global_3day/hblt','hblt',[fi_lon(1) fi_lat(1) 1 i],[length(fi_lon) length(fi_lat) Inf 1]));
    OFES.MLayer = squeeze(ncread('http://apdrc.soest.hawaii.edu:80/dods/public_ofes/OfES/ncep_0.1_global_3day/hmxl','hmxl',[fi_lon(1) fi_lat(1) 1 i],[length(fi_lon) length(fi_lat) Inf 1]));
    OFES.Salt = squeeze(ncread('http://apdrc.soest.hawaii.edu:80/dods/public_ofes/OfES/ncep_0.1_global_3day/salt','salinity',[fi_lon(1) fi_lat(1) 1 i],[length(fi_lon) length(fi_lat) Inf 1]));
    OFES.Temp = squeeze(ncread('http://apdrc.soest.hawaii.edu:80/dods/public_ofes/OfES/ncep_0.1_global_3day/temp','temp',[fi_lon(1) fi_lat(1) 1 i],[length(fi_lon) length(fi_lat) Inf 1]));
    
    OFES.u = squeeze(ncread('http://apdrc.soest.hawaii.edu:80/dods/public_ofes/OfES/ncep_0.1_global_3day/uvel','uvel',[fi_lon(1) fi_lat(1) 1 i],[length(fi_lon) length(fi_lat) Inf 1]));
    OFES.v = squeeze(ncread('http://apdrc.soest.hawaii.edu:80/dods/public_ofes/OfES/ncep_0.1_global_3day/vvel','vvel',[fi_lon(1) fi_lat(1) 1 i],[length(fi_lon) length(fi_lat) Inf 1]));
    OFES.w = squeeze(ncread('http://apdrc.soest.hawaii.edu:80/dods/public_ofes/OfES/ncep_0.1_global_3day/wvel','wvel',[fi_lon(1) fi_lat(1) 1 i],[length(fi_lon) length(fi_lat) Inf 1]));

    eval(['save /Volumes/WD_MYBOOK/Satellite/OFES/',sprintf('%04d',year(OFES.datenum)),sprintf('%02d',month(OFES.datenum)),sprintf('%03d',day(OFES.datenum)),'.mat OFES'])
    clear OFES
    
end
