clear
close all

 load shelf_contour
 
 seek_lats = [-35.1 -33.8 -32 -36.2 -29.4 -28.1 -38 -37 -35.7 -31.4 -30.4 -28.8 -27.3];
 
 ix = dsearchn(c100(2,:)',seek_lats');
 shelf = c100(:,ix)';
 
 tbl = table(shelf(:,1)*0+100,shelf(:,1),shelf(:,2),'VariableNames',{'Depth','Lon','Lat'});
 
 writetable(tbl,"ReleaseLocations_Backward.csv")
 
 
 % Original
%  seek_lats = -26:-0.5:-30;
%  
%  ix = dsearchn(c100(2,:)',seek_lats');
%  shelf = c100(:,ix)';
%  
%  tbl = table(shelf(:,1)*0+100,shelf(:,1),shelf(:,2),'VariableNames',{'Depth','Lon','Lat'});
%  
%  writetable(tbl,"ReleaseLocations.csv")
 