# Script to show Annual variation
# Run on UNSW Katana HPC

library(tidyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(geosphere)
library(ggplot2)
#devtools::install_github("thomasp85/patchwork")
#install.packages("scales")
#devtools::install_github('thomasp85/gganimate')
#library(gganimate)
library(tools)
#install.packages("rgdal")
library(rgdal)
library(sp)
library(raster)
library(stringr)
library(tm)
library(data.table)
library(zoo)
library(cowplot)
library(scales)
#library(reshape)
#library(maptools)

# Spring NSW
full_data_endpoints <- readRDS("../../srv/scratch/z3374139/Combined Mortality_endpoints_ALL.rds")

#full_data_endpoints <- readRDS("Combined Mortality_endpoints_ALL.rds")

bounds <- read.csv("ROMS Boundaries.csv", header = T)
dots <- read.csv("forward_release_summary _table.csv", header = T)

#Load map data
Aus <- readOGR(dsn = "Shape files/australia",layer = "cstauscd_r")
#plot(Aus)
Aus_coast <- subset(Aus, FEAT_CODE != "sea")
#plot(Aus_coast)

min_lon <- 145
max_lon <- 160
min_lat <- -40
max_lat <- -24

geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)

Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))

coordinates(Sites.grid) <- ~ lon_bound + lat_bound

Aus_crop <- crop(Aus_coast, extent(Sites.grid)) #rgeos must be installed to run

shelf <- read.csv("200m_contour.csv", header = T)
shelf <- subset(shelf, Var2 >= -39)
shelf <- subset(shelf, Var2 <= -25.4)
shelf <- subset(shelf, Var1 > 145)

####
full_data_endpoints <- subset(full_data_endpoints, Release_Month != 1)
full_data_endpoints <- subset(full_data_endpoints, Release_Location != -28)

table(full_data_endpoints$Release_Location, full_data_endpoints$Release_Month)


full_data_endpoints$Spawning_Event <- "Other"
full_data_endpoints$Spawning_Event[full_data_endpoints$Release_Location < -28 & full_data_endpoints$Release_Month < 4] <- "Summer NSW"
full_data_endpoints$Spawning_Event[full_data_endpoints$Release_Location < -28 & full_data_endpoints$Release_Month > 7] <- "Spring NSW"
full_data_endpoints$Spawning_Event[full_data_endpoints$Release_Location > -28 & full_data_endpoints$Release_Month > 7] <- "Spring QLD"

table(full_data_endpoints$Spawning_Event)

#full_data_endpoints <- subset(full_data_endpoints, Spawning_Event != "Other")
full_data_endpoints <- subset(full_data_endpoints, Spawning_Event == "Spring NSW")

droplevels(full_data_endpoints)
mini_dat <- full_data_endpoints

coordinates(mini_dat) <- ~Longtitude+Latitude
mini_dat$count <- 1

#newcrs <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84
#+datum=WGS84 +no_defs +towgs84=0,0,0")

#mini_dat <- CRS(newcrs)
proj4string(mini_dat) <- "+proj=longlat +datum=WGS84"
mini_dat$Spawning_Event <- as.factor(as.character(mini_dat$Spawning_Event))

# Get list of release locations
mini_dat$Release_Year <- as.factor(as.character(mini_dat$Release_Year))
Years <- levels(mini_dat$Release_Year)

# make empty list
rast_list <- list()

# make raster for each level of release location
for (i in 1:length(Years)){
  rast <- raster(ncol = 100, nrow = 150, ) # raster resolution
  extent(rast) <- extent(mini_dat) #set extent of raster
  datX <- subset(mini_dat, Release_Year == Years[i]) # select one release site at a time
  nam <- paste("test_rast",Years[i], sep="") # name each one
  rast_list[[i]] <- assign(nam, rasterize(datX, rast, mini_dat$count, fun = sum)) # add each one to the list
}

# Make raster stack from list
full_rast <- stack(rast_list)

# calaculate area of each cell
full_rast_area <- area(full_rast)

# calculate density of larvae ( per km^2)
full_rast_den <- full_rast/full_rast_area

# check it
summary(full_rast_den)

# name layers
names(full_rast_den) <- Years

# Transofrm to dataframe for plotting
library(reshape)
library(maptools)

head(full_rast_den)

b.dat <- data.frame(coordinates(full_rast_den),as.data.frame(full_rast_den))
head(b.dat)

plt.dat <- melt(b.dat,id.vars = c("x","y"))
head(plt.dat)

summary(plt.dat$value)
str(plt.dat)

plt.dat$variable <- as.character(plt.dat$variable)
plt.dat$variable <- str_replace(plt.dat$variable, "X", "")
plt.dat$variable <- as.factor(plt.dat$variable)
plt.dat$Release_Year <- plt.dat$variable
str(plt.dat)




dots <- read.csv("forward_release_summary _table_NSW.csv", header = T)



p1 <- ggplot() + geom_tile(data =plt.dat, aes(x=x,y=y,fill=value)) + 
  facet_wrap(~Release_Year, nrow = 5) + # ,labeller=variable_labeller
  theme_classic() + 
  labs(x=expression(paste("Longitude (",degree, ")", sep="")), y=expression(paste("Latitude (", degree, ")"))) +
  scale_fill_viridis_c(option = "plasma", trans = "log10", na.value=NA, #breaks = c(0.01,0.1,1,10,50),
                       name=bquote("Larvae" ~(km^-2)), #limits = c(0.01,50), oob=squish,
                       guide = guide_colourbar(draw.ulim = FALSE, draw.llim = FALSE)) + 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  #scale_fill_distiller(palette = "Spectral", na.value=NA, trans="log10") +
  coord_map() + #coord_quickmap() + #  # this line could be very slow
  geom_path(data=bounds, aes(x=Long, y = Lat), colour = "black", lty="dashed", show.legend = FALSE) +
  geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray40", colour = "black")+
  geom_path(data=shelf, aes(x=Var1, y = Var2)) + 
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14),
        strip.background = element_rect(colour = "white"),
        legend.justification=c(1,0), legend.position=c(0.92,0.05), legend.direction = "horizontal",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"))+
  geom_point(data = dots, aes(x = Longitude, y = Latitude), size = 2, col = "black")

p1

ggsave(filename = "Output/Faceted_distribution_Annual Variation_Spring_NSW_Final.pdf", width = 21, height = 29.7, units = "cm")
ggsave(filename = "Output/Faceted_distribution_Annual Variation_Spring_NSW_Final.png", width = 21, height = 29.7, units = "cm", dpi = 600)

