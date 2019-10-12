clear
close all

reprocess = 0;

release = readtable('Release_Sites_all.csv');

release_back = release(strcmp(release.Type,'Backwards')==1,:);
release_for = release(strcmp(release.Type,'Forward')==1,:);

model_bound = readtable('ROMS_Boundaries.csv');

txt = 12;
txt_font = 6;

x1 = 145; x2 = 163;
y1 = -45; y2 = -24;

%%
figure

s1 = subplot(1,2,1);
h = plot_EastCoast(x1,x2,y1,y2,2,1,txt);
setm(h.h,'mlabellocation', 5,'plabellocation', 5, 'mlinelocation', 5, 'mlinelocation', 5, 'grid','off')
set(h.hs,'linewidth',0.6)
hold on

p1 = plotm(release_back.Latitude,release_back.Longitude,'ok','markersize',4,'markerfacecolor','k','markeredgecolor','none');
p4 = plotm(release_for.Latitude,release_for.Longitude,'^k','markersize',4,'markerfacecolor','k','markeredgecolor','none');
p5 = plotm(model_bound.Lat, model_bound.Long,'k:');
uistack([p1 p4], 'top')

le = legend([p4,p1,p5],'Forward Release Location','Backward Release Location','Model Boundary','location','southeast');
set(le,'fontsize',txt_font, 'box', 'off')

textm(release_back.Latitude,release_back.Longitude + 1, release_back.Name,'fontsize',txt_font,'backgroundcolor','none');
textm(mean(release_for.Latitude(1:4)),mean(release_for.Longitude(1:4)) + 1, {'Southern Spawning'; 'Region'},'fontsize',txt_font,'backgroundcolor','none','HorizontalAlignment','left');
textm(mean(release_for.Latitude(5:8)),mean(release_for.Longitude(5:8)) + 1, {'Northern Spawning'; 'Region'},'fontsize',txt_font,'backgroundcolor','none','HorizontalAlignment','left');

% setm(gca,'GColor',[0.8 0.8 0.8],'glinewidth',0.2)

textm(-24.5,145.5,'\bfA)','fontsize',12)
uistack(h.h, 'bottom')
uistack(h.f, 'bottom')

%% Inset
y11 = -47; y22 = -1;
x11 = 130; x22 = 165;

ax = axes('position',[0.1 0.58 0.2 0.2]);

h2 = plot_EastCoast(x11,x22,y11,y22,2,0,txt); hold on
p6 = plotm([y1 y2 y2 y1 y1],[x1 x1 x2 x2 x1],'k','linewidth',1);
box on
setm(h2.h,'MeridianLabel','off','ParallelLabel','off','grid','off')
uistack(h2.f, 'bottom')

%% Plot 2
s2 = subplot(1,2,2);
h = plot_EastCoast(x1,x2,y1,y2,2,1,txt);
setm(h.h,'mlabellocation', 5,'plabellocation', 5, 'mlinelocation', 5, 'mlinelocation', 5, 'grid','off')

res = 10;
modis_datenum = datenum(2016,1,1);
s = struct('res', 'Month');
count = 1;
c_ax = [14 28]; 

SST = sat_IMOS(modis_datenum, [y1 y2], [x1 x2], 'SST', s, count);
SST.SST = squeeze(SST.SST);

[c,h.hc] = contourm(SST.Lat(1:res:end),SST.Lon(1:res:end),SST.SST(1:res:end,1:res:end),c_ax(1):0.5:c_ax(2),'fill','on','edgecolor','none');
caxis(c_ax);

colormap('jet')
uistack(h.f, 'top')
uistack(h.h, 'bottom')

clbr = colorbar('Position',[0.63 0.47 0.017 0.35]);
set(get(clbr,'Label'),'String','Sea Surface Temperature (^\circC)')

in.datenum = modis_datenum;
in.Lat = [y1 y2];
in.Lon = [x1 x2];
in.leg = 0;
in.bkgrnd = 0;
in.coast = 1;
in.res = 2;

Alt = sat_plot_Altimetry(in);

textm(-24.5,145.5,'\bfB)','fontsize',12)
uistack(h.f, 'bottom')
uistack(h.hc, 'bottom')
set(gcf,'color','w')
export_fig Tailor_SiteMap -png -r400