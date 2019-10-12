clear
close all
xtra_colors

res = 0.5;

Lat_range = [-38 -25];
Lon_range = [148 163];

XEDGES = Lon_range(1)-res/2:res:Lon_range(2)+res/2;
YEDGES = Lat_range(1)-res/2:res:Lat_range(2)+res/2;

txt = 8;

figure

files = sort(Dir2FileName('.nc','KatanaSept20_100m'));
rows = 3; cols = 3;

for num = 1:length(files)
    figprep
    
    h1 = plot_EastCoast(Lon_range(1),Lon_range(2),Lat_range(1),Lat_range(2),1,1,txt);
    uistack(h1.f, 'bottom')
    set(gca,'fontsize',txt)
    if num > 6
        setm(gca,'mlinelocation',5 ,'plinelocation',5,'plabellocation',5,'mlabellocation',5);
    else
        setm(gca,'mlinelocation',5 ,'plinelocation',5,'plabellocation',5,'meridianlabel','off');
    end
    
    if num ~= 1 && num ~= 4 && num ~= 7
        setm(gca,'parallellabel','on')
    else
        setm(gca,'parallellabel','off')
    end
        
    lat = ncread(files{num},'lat');
    lon = ncread(files{num},'lon');
    
    N = histcounts2(lon(25,:),lat(25,:),XEDGES,YEDGES);
    N(N==0) = NaN;
    N = N./nansum(N(:)).*100;
    
    pcolorm(Lat_range(1):res:Lat_range(2),Lon_range(1):res:Lon_range(2),N');
    
    colormap jet
    caxis([0 5])
    title(['Lat: ',num2str(roundn(lat(1,1),-1))],'fontsize',txt)
    plotm(lat(1),lon(1),'r.','markersize',20)
    colorbar
    
%     
end

% u = ncread(file,'u');
% u_rho = u2rho(u(:,:,30,1));


uistack(h1.f, 'top')
uistack(h1.hs, 'top')
set(gcf,'color','w')

export_fig Tailor_PrelimRelease -pdf
return

figure

files = sort(Dir2FileName('.nc','KatanaSept10_Lon0.02'));
rows = 4; cols = 3;
for num = 1:length(files)
    figprep
    
    h1 = plot_EastCoast(Lon_range(1),Lon_range(2),Lat_range(1),Lat_range(2),1,1,txt);
    uistack(h1.f, 'bottom')
    set(gca,'fontsize',txt)
    setm(gca,'mlinelocation',1,'plinelocation',1,'mlabellocation',1,'plabellocation',1);
    
    
    lat = ncread(files{num},'lat');
    lon = ncread(files{num},'lon');
    plotm(lat,lon,'o-','linewidth',2)
    
    
end

set(gcf,'color','w')
export_fig Tailor_Latitude_SubMap -pdf



return


