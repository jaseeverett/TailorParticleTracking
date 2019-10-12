# Script to make Final distribution plots for backwards tracking and connectivity matrix


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

# Plots showing particle position on settlement day by month. Note Australia may have a funny shape because of the differing axes scales.
#total_data2 <- subset(full_data_endpoints, Release_Location <= -28.5)
#total_data3 <- subset(total_data2, Month == 2 | Month == 3)

# Load release point data and model domain
dots <- read.csv("backwards_release_summary _table.csv", header = T)
bounds <- read.csv("ROMS Boundaries.csv", header = T)


# To find release Lat/Long and count particles released - very slow
# mydata <- readRDS("katana output/Combined_Backwards.rds")
# mydata <- subset(mydata, DayofLife == 0)
# 
# dat <- mydata %>% group_by(Latitude, Longtitude) %>% summarise(count = n())
# dat
# write.csv(dat, file = "backwards_release_summary _table.csv")

full_data_endpoints <- readRDS("katana output/Backwards_endpoints.rds")
#full_data_endpoints <- readRDS("../../srv/scratch/z3374139/Backwards_endpoints.rds")
mini_dat <- subset(full_data_endpoints, Month == 2| Month == 3)#  Release_Month >= 8) # Release_Month == 2| Release_Month == 3)|
mini_dat <- subset(mini_dat, Release_Location < -31 & Release_Location != -35.7)

coordinates(mini_dat) <- ~Longtitude+Latitude
mini_dat$count <- 1

newcrs <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84
+datum=WGS84 +no_defs +towgs84=0,0,0")

mini_dat <- CRS(newcrs)
mini_dat$Release_Location <- as.factor(as.character(mini_dat$Release_Location))

# Get list of release locations
locations <- levels(mini_dat$Release_Location)

# make empty list
rast_list <- list()

# make raster for each level of release location
for (i in 1:7){
  rast <- raster(ncol = 100, nrow = 150, ) # raster resolution
  extent(rast) <- extent(mini_dat) #set extent of raster
  datX <- subset(mini_dat, Release_Location == locations[i]) # select one release site at a time
  nam <- paste("test_rast",locations[i], sep="") # name each one
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
names(full_rast_den) <- locations

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
plt.dat$variable <- str_replace(plt.dat$variable, "X.", "-")
plt.dat$variable <- as.factor(plt.dat$variable)
plt.dat$Release_Location <- plt.dat$variable
str(plt.dat)

variable_names <- list(
  "-31.4" = "a) Hastings River (-31.4°)" ,
  "-32" = "b) Wallis Lake (-32°)",
  "-33.8" = "c) Sydney Harbour (-33.8°)" ,
  "-35.1" = "d) Jervis Bay (-35.1°)",
  "-36.2" = "e) Wagonga Inlet (-36.2°)" ,
  "-37" = "f) Twofold Bay (-37°)",
  "-38" = "g) Gippsland Lakes (-38°)"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

p1 <- ggplot() + geom_tile(data =plt.dat, aes(x=x,y=y,fill=value)) + 
  facet_wrap(~Release_Location, nrow = 2,labeller=variable_labeller) +
  theme_classic() + labs(x="Longitude (°)", y="Latitude (°)") +
  scale_fill_viridis_c(option = "plasma", trans = "log10", na.value=NA, name=bquote("Larvae" ~(km^-2)),
                       guide = guide_colourbar(draw.ulim = FALSE, draw.llim = FALSE)) + 
  #scale_fill_distiller(palette = "Spectral", na.value=NA, trans="log10") +
  coord_map() + #coord_quickmap() + #  this line could be very slow  # coord_map() + #
  geom_path(data=bounds, aes(x=Long, y = Lat), colour = "black", lty="dashed", show.legend = FALSE) +
  geom_path(data=shelf, aes(x=Var1, y = Var2)) +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray40", colour = "black")+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold"),
        strip.background = element_rect(colour = "white"),
        legend.justification=c(1,0), legend.position=c(0.99,-0.06),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 12, face = "bold"))+
  geom_point(data = dots, aes(x = Longitude, y = Latitude),  
             shape = 21, fill = "black", colour = "white", stroke = 1, size = 1)

p1


ggsave(filename = "Output/Backwards endpoint faceted late summer.pdf", width = 21, height = 14.8, units = "cm")
ggsave(filename = "Output/Backwards endpoint faceted late summer.png", width = 21, height = 14.8, units = "cm", dpi = 600)

### Try heatmap style plot showing only on Continental shelf
full_data_endpoints <- readRDS("katana output/Backwards_endpoints.rds")
#full_data_endpoints <- readRDS("../../srv/scratch/z3374139/Backwards_endpoints.rds")
mini_dat <- subset(full_data_endpoints, Month == 2| Month == 3)#  Release_Month >= 8) # Release_Month == 2| Release_Month == 3)|
mini_dat <- subset(mini_dat, Release_Location < -31 & Release_Location != -35.7)

head(mini_dat)
mini_dat <- subset(mini_dat, Bathymetry <= 200)
mini_dat$Settlement_Lat <- cut(mini_dat$Latitude, breaks = seq(-43,-23, 1))

settlement_summary <- mini_dat %>% group_by(Release_Location, Settlement_Lat) %>% summarise(Count = n())
head(settlement_summary)

settlement_summary$Release_Location <- as.factor(as.character(settlement_summary$Release_Location))

combined_settlement_data <- settlement_summary

combined_settlement_data <- separate(data = combined_settlement_data, into = c("Max_Latitude", "Min_Latitude"), col = Settlement_Lat, sep=",")
combined_settlement_data$Max_Latitude <- as.numeric( str_sub(combined_settlement_data$Max_Latitude, start=2))
combined_settlement_data$Min_Latitude <- as.numeric( str_sub(combined_settlement_data$Min_Latitude, start=1, end =3))
combined_settlement_data$Min_Latitude <- as.numeric( str_sub(combined_settlement_data$Min_Latitude, start=1, end =3))

combined_settlement_data2 <- combined_settlement_data %>% group_by(Release_Location, Max_Latitude) %>%
  mutate(Mid_Latitude = mean(c(Max_Latitude, Min_Latitude)))
head(combined_settlement_data2)

combined_settlement_data3 <- combined_settlement_data2 %>%
  group_by(Release_Location) %>%
  mutate(countT= sum(Count)) %>%
  group_by(Release_Location, Mid_Latitude, add=TRUE) %>%
  mutate(per=100*Count/countT)
head(combined_settlement_data3)
sum(combined_settlement_data3$per[1:13])




head(combined_settlement_data2)

p1_heatmap <- ggplot(combined_settlement_data3, aes(Mid_Latitude, Release_Location,  fill = per)) + 
  geom_tile() + theme_classic() + labs(title = " ") +
  scale_fill_viridis_c(option = "plasma", na.value=NA, name=bquote("Percent (%)")) +
  scale_y_discrete(limits = rev(levels(combined_settlement_data3$Release_Location)),expand = c(0,0)) +
  scale_x_reverse(breaks=seq(-26,-38,-2), expand = c(0,0))+ xlab("Sink Latitude") + ylab("Source Latitude") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold"),
        strip.background = element_rect(colour = "white"),
        legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 12, face = "bold"))
p1_heatmap
  
# Portrait style

# p4 <- ggplot() + geom_tile(data =plt.dat, aes(x=x,y=y,fill=value)) + 
#   facet_wrap(~Release_Location, nrow = 4,labeller=variable_labeller) +
#   theme_classic() + labs(x="Longitude (°)", y="Latitude (°)") +
#   scale_fill_viridis_c(option = "plasma", trans = "log10", na.value=NA, name=bquote("Larvae" ~(km^-2)),
#                        guide = guide_colourbar(draw.ulim = FALSE, draw.llim = FALSE)) + 
#   #scale_fill_distiller(palette = "Spectral", na.value=NA, trans="log10") +
#   coord_map() + #coord_quickmap() + # this line could be very slow  # coord_map() + #
#   geom_path(data=bounds, aes(x=Long, y = Lat), colour = "black", lty="dashed", show.legend = FALSE) +
#   geom_path(data=shelf, aes(x=Var1, y = Var2)) +
#   scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray40", colour = "black")+
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
#         axis.text.x  = element_text(colour="black", size = 12), 
#         axis.title.y = element_text(face="bold", colour="black", size = 18),
#         axis.text.y  = element_text(colour="black", size = 14),
#         axis.ticks = element_line(colour="black"),
#         strip.text = element_text(colour="black", face = "bold"),
#         strip.background = element_rect(colour = "white"),
#         legend.justification=c(1,0), legend.position=c(0.99,0),
#         panel.border = element_rect(colour = "black", fill=NA, size = 1),
#         legend.key.size = unit(1, "cm"),
#         legend.title = element_text(face = "bold", size = 16),
#         legend.text = element_text(size = 12, face = "bold"))+
#   geom_point(data = dots, aes(x = Longitude, y = Latitude), col = "black")
# 
# p4
# 
# 
# ggsave(filename = "Output/Backwards endpoint faceted late summer portrait.pdf", width = 21, height = 29.7, units = "cm")
# ggsave(filename = "Output/Backwards endpoint faceted late summer portrait.png", width = 21, height = 29.7, units = "cm", dpi = 600)
# 

#fwrite(mini_dat, "katana output/test.csv")


#str(mini_dat)

## To get Release Longitude as well as Latitude(called Release_Location)
# full_data_endpoints$Release_Location <- round(full_data_endpoints$Release_Location, digits = 1)
# table(full_data_endpoints$Release_Location)
# full_data_endpoints$Release_Longitude <- 0
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -38] <- 148.9147949
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -37] <- 150.1600037
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -36.2] <- 150.2451019
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -35.7] <- 150.3833008
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -35.1] <- 150.8549957
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -33.8] <- 151.4167023
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -32] <- 152.8444061
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -31.4] <- 153.0957947
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -30.4] <- 153.3182068
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -29.4] <- 153.7312927
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -28.8] <- 153.8036041
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -28.1] <- 153.7861023
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -27.3] <- 153.6421967
# table(full_data_endpoints$Release_Location, full_data_endpoints$Release_Longitude)
# saveRDS(full_data_endpoints, file = "katana output/Backwards_endpoints.rds")



# # late summer southern plot
# g3 <- ggplot(full_data_endpoints) + coord_map("mercator") + 
#   geom_bin2d(aes(x = Longtitude, y = Latitude, fill = ..density..), binwidth=0.5) + 
#   theme_classic() + scale_fill_distiller(palette= rev("Spectral"), limits = c(0.0000001,0.09), breaks = c(0.001, 0.05, 0.1),
#                                          guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE), trans = "sqrt") + #, trans = "log" (in fill_distiller())
#   geom_point(data = dots, aes(x = Longitude, y = Latitude), size = 3, col = "darkred") +
#   xlab("Longitude") +
#   geom_path(data=bounds, aes(x=Long, y = Lat)) +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30") + #+ facet_wrap(~Month)
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
#         axis.text.x  = element_text(colour="black", size = 16), 
#         axis.title.y = element_text(face="bold", colour="black", size = 20),
#         axis.text.y  = element_text(colour="black", size = 16)) +
#   geom_path(data=shelf, aes(x=Var1, y = Var2)) +
#   facet_wrap(~Release_Location)
# g3

#ggsave(filename = "Output/Backwards release faceted.pdf", width = 8, height = 8)
#ggsave(filename = "Output/Backwards release faceted.png", width = 8, height = 8, dpi = 600)

#paste("This script finished with mortality applied")

# m <- ggplot(full_data_endpoints, aes(x = Longtitude, y = Latitude)) +
#   geom_density_2d() +
#   geom_path(data=bounds, aes(x=Long, y = Lat)) +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30")
# m
# 
# d <- ggplot(mini_dat, aes(x = Longtitude, y = Latitude)) +
#   #stat_density_2d(aes(fill = stat((nlevel))), geom = "polygon") + # nlevel is normalising each subplot to a maximum of 1 for the particle density
#   #geom_point(alpha=0.01)+
#   stat_density_2d(geom = "raster", aes(fill = stat(ndensity)), contour = FALSE) +
#   geom_path(data=bounds, aes(x=Long, y = Lat, colour = "white"), show.legend = FALSE) +
#   scale_colour_manual(values = "white") + labs(x="Longitude (°)", y="Latitude (°)") +
#   geom_path(data=shelf, aes(x=Var1, y = Var2)) +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray40", colour = "black") +
#   facet_wrap(~Release_Location, nrow = 2) + scale_fill_viridis_c(option = "plasma", trans = "sqrt") + theme_classic() +
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
#         axis.text.x  = element_text(colour="black", size = 14), 
#         axis.title.y = element_text(face="bold", colour="black", size = 18),
#         axis.text.y  = element_text(colour="black", size = 14),
#         strip.text = element_text(colour="black", face = "bold"),
#         legend.justification=c(1,0), legend.position=c(0.92,0.08)) +
#   geom_point(data = dots, aes(x = Longitude, y = Latitude), size = 2, col = "white")
# d
# 

#### Spring spawning now
mini_dat <- subset(full_data_endpoints, Month >= 8)#  Release_Month >= 8) # Release_Month == 2| Release_Month == 3)|
mini_dat <- subset(mini_dat, Release_Location < -31 & Release_Location != -35.7)

## To get Release Longitude as well as Latitude(called Release_Location)
# full_data_endpoints$Release_Location <- round(full_data_endpoints$Release_Location, digits = 1)
# table(full_data_endpoints$Release_Location)
# full_data_endpoints$Release_Longitude <- 0
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -38] <- 148.9147949
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -37] <- 150.1600037
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -36.2] <- 150.2451019
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -35.7] <- 150.3833008
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -35.1] <- 150.8549957
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -33.8] <- 151.4167023
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -32] <- 152.8444061
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -31.4] <- 153.0957947
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -30.4] <- 153.3182068
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -29.4] <- 153.7312927
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -28.8] <- 153.8036041
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -28.1] <- 153.7861023
# full_data_endpoints$Release_Longitude[full_data_endpoints$Release_Longitude  == -27.3] <- 153.6421967
# table(full_data_endpoints$Release_Location, full_data_endpoints$Release_Longitude)
# saveRDS(full_data_endpoints, file = "katana output/Backwards_endpoints.rds")

coordinates(mini_dat) <- ~Longtitude+Latitude
mini_dat$count <- 1

newcrs <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84
+datum=WGS84 +no_defs +towgs84=0,0,0")

mini_dat <- CRS(newcrs)
mini_dat$Release_Location <- as.factor(as.character(mini_dat$Release_Location))

# Get list of release locations
locations <- levels(mini_dat$Release_Location)

# make empty list
rast_list <- list()

# make raster for each level of release location
for (i in 1:7){
  rast <- raster(ncol = 100, nrow = 150, ) # raster resolution
  extent(rast) <- extent(mini_dat) #set extent of raster
  datX <- subset(mini_dat, Release_Location == locations[i]) # select one release site at a time
  nam <- paste("test_rast",locations[i], sep="") # name each one
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
names(full_rast_den) <- locations

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
plt.dat$variable <- str_replace(plt.dat$variable, "X.", "-")
plt.dat$variable <- as.factor(plt.dat$variable)
plt.dat$Release_Location <- plt.dat$variable
str(plt.dat)

variable_names <- list(
  "-31.4" = "a) Hastings River (-31.4°)" ,
  "-32" = "b) Wallis Lake (-32°)",
  "-33.8" = "c) Sydney Harbour (-33.8°)" ,
  "-35.1" = "d) Jervis Bay (-35.1°)",
  "-36.2" = "e) Wagonga Inlet (-36.2°)" ,
  "-37" = "f) Twofold Bay (-37°)",
  "-38" = "g) Gippsland Lakes (-38°)"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

p2 <- ggplot() + geom_tile(data =plt.dat, aes(x=x,y=y,fill=value)) + 
  facet_wrap(~Release_Location, nrow = 2,labeller=variable_labeller) +
  theme_classic() + labs(x="Longitude (°)", y="Latitude (°)") +
  scale_fill_viridis_c(option = "plasma", trans = "log10", na.value=NA, breaks = c(0.01,0.1,1,10,50),
                       name=bquote("Larvae" ~(km^-2)), limits = c(0.01,50),
                       guide = guide_colourbar(draw.ulim = FALSE, draw.llim = FALSE)) +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  #scale_fill_distiller(palette = "Spectral", na.value=NA, trans="log10") +
  coord_map() + #coord_quickmap() + # this line could be very slow  # coord_map() + #
  geom_path(data=bounds, aes(x=Long, y = Lat), colour = "black", lty="dashed", show.legend = FALSE) +
  geom_path(data=shelf, aes(x=Var1, y = Var2)) +
  geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray40", colour = "black")+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold"),
        strip.background = element_rect(colour = "white"),
        legend.justification=c(1,0), legend.position=c(0.99,-0.06),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 12, face = "bold"))+
  geom_point(data = dots, aes(x = Longitude, y = Latitude),  
             shape = 21, fill = "black", colour = "white", stroke = 1, size = 1)
p2

ggsave(filename = "Output/Backwards endpoint faceted spring.pdf", width = 21, height = 14.8, units = "cm")
ggsave(filename = "Output/Backwards endpoint faceted spring.png", width = 21, height = 14.8, units = "cm", dpi = 600)

# p3 <- ggplot() + geom_tile(data =plt.dat, aes(x=x,y=y,fill=value)) + 
#   facet_wrap(~Release_Location, nrow = 4,labeller=variable_labeller) +
#   theme_classic() + labs(x="Longitude (°)", y="Latitude (°)") +
#   scale_fill_viridis_c(option = "plasma", trans = "log10", na.value=NA, breaks = c(0.01,0.1,1,10,50),
#                        name=bquote("Larvae" ~(km^-2)), limits = c(0.01,50),
#                        guide = guide_colourbar(draw.ulim = FALSE, draw.llim = FALSE)) +
#   scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
#   #scale_fill_distiller(palette = "Spectral", na.value=NA, trans="log10") +
#   coord_map() + #coord_quickmap() + # this line could be very slow  # coord_map() + #
#   geom_path(data=bounds, aes(x=Long, y = Lat), colour = "black", lty="dashed", show.legend = FALSE) +
#   geom_path(data=shelf, aes(x=Var1, y = Var2)) +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray40", colour = "black")+
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
#         axis.text.x  = element_text(colour="black", size = 12), 
#         axis.title.y = element_text(face="bold", colour="black", size = 18),
#         axis.text.y  = element_text(colour="black", size = 14),
#         axis.ticks = element_line(colour="black"),
#         strip.text = element_text(colour="black", face = "bold"),
#         strip.background = element_rect(colour = "white"),
#         legend.justification=c(1,0), legend.position=c(0.99,0),
#         panel.border = element_rect(colour = "black", fill=NA, size = 1),
#         legend.key.size = unit(1, "cm"),
#         legend.title = element_text(face = "bold", size = 16),
#         legend.text = element_text(size = 12, face = "bold"))+
#   geom_point(data = dots, aes(x = Longitude, y = Latitude), col = "black")
# 
# p3
# 
# ggsave(filename = "Output/Backwards endpoint faceted spring portrait.pdf", width = 21, height = 29.7, units = "cm")
# ggsave(filename = "Output/Backwards endpoint faceted spring portrait.png", width = 21, height = 29.7, units = "cm", dpi = 600)


### Now heatmap
full_data_endpoints <- readRDS("katana output/Backwards_endpoints.rds")

mini_dat2 <- subset(full_data_endpoints, Month >= 8)#  Release_Month >= 8) # Release_Month == 2| Release_Month == 3)|
mini_dat2 <- subset(mini_dat, Release_Location < -31 & Release_Location != -35.7)

head(mini_dat2)
mini_dat2 <- subset(mini_dat2, Bathymetry <= 200)
mini_da2t$Settlement_Lat <- cut(mini_dat2$Latitude, breaks = seq(-43,-23, 1))

settlement_summary2 <- mini_dat2 %>% group_by(Release_Location, Settlement_Lat) %>% summarise(Count = n())
head(settlement_summary2)

settlement_summary2$Release_Location <- as.factor(as.character(settlement_summary2$Release_Location))

combined_settlement_dataX <- settlement_summary2

combined_settlement_dataX <- separate(data = combined_settlement_dataX, into = c("Max_Latitude", "Min_Latitude"), 
                                      col = Settlement_Lat, sep=",")
combined_settlement_dataX$Max_Latitude <- as.numeric( str_sub(combined_settlement_dataX$Max_Latitude, start=2))
combined_settlement_dataX$Min_Latitude <- as.numeric( str_sub(combined_settlement_dataX$Min_Latitude, start=1, end =3))
combined_settlement_dataX$Min_Latitude <- as.numeric( str_sub(combined_settlement_dataX$Min_Latitude, start=1, end =3))

combined_settlement_data2X <- combined_settlement_dataX %>% group_by(Release_Location, Max_Latitude) %>%
  mutate(Mid_Latitude = mean(c(Max_Latitude, Min_Latitude)))
head(combined_settlement_data2X)

combined_settlement_data3X <- combined_settlement_data2X %>%
  group_by(Release_Location) %>%
  mutate(countT= sum(Count)) %>%
  group_by(Release_Location, Mid_Latitude, add=TRUE) %>%
  mutate(per=100*Count/countT)
head(combined_settlement_data3X)
sum(combined_settlement_data3X$per[1:13])


p2_heatmap <- ggplot(combined_settlement_data3X, aes(Mid_Latitude, Release_Location,  fill = per)) + 
  geom_tile() + theme_classic() + labs(title = " ") +
  scale_fill_viridis_c(option = "plasma", na.value=NA, name=bquote("Percent (%)")) +
  scale_y_discrete(limits = rev(levels(combined_settlement_data3$Release_Location)),expand = c(0,0)) +
  scale_x_reverse(breaks=seq(-26,-38,-2), expand = c(0,0))+ xlab("Sink Latitude") + ylab("Source Latitude") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold"),
        strip.background = element_rect(colour = "white"),
        legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 12, face = "bold"))
p2_heatmap

# m <- ggplot(full_data_endpoints, aes(x = Longtitude, y = Latitude)) +
#   geom_density_2d() +
#   geom_path(data=bounds, aes(x=Long, y = Lat)) +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30")
# m

# d <- ggplot(mini_dat, aes(x = Longtitude, y = Latitude)) +
#   #stat_density_2d(aes(fill = stat((nlevel))), geom = "polygon") + # nlevel is normalising each subplot to a maximum of 1 for the particle density
#   #geom_point(alpha=0.01)+
#   stat_density_2d(geom = "raster", aes(fill = stat(ndensity)), contour = FALSE) +
#   geom_path(data=bounds, aes(x=Long, y = Lat, colour = "white"), show.legend = FALSE) +
#   scale_colour_manual(values = "white") + labs(x="Longitude (°)", y="Latitude (°)") +
#   geom_path(data=shelf, aes(x=Var1, y = Var2)) +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray40", colour = "black") +
#   facet_wrap(~Release_Location, nrow = 2) + scale_fill_viridis_c(option = "plasma", trans = "sqrt") + theme_classic() +
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
#         axis.text.x  = element_text(colour="black", size = 14), 
#         axis.title.y = element_text(face="bold", colour="black", size = 18),
#         axis.text.y  = element_text(colour="black", size = 14),
#         strip.text = element_text(colour="black", face = "bold"),
#         legend.justification=c(1,0), legend.position=c(0.92,0.08)) +
#   geom_point(data = dots, aes(x = Longitude, y = Latitude), size = 2, col = "white")
# d
# ggsave(filename = "Output/Backwards endpoint faceted spring.pdf", width = 21, height = 14.8, units = "cm")
# ggsave(filename = "Output/Backwards endpoint faceted spring.png", width = 21, height = 14.8, units = "cm", dpi = 600)

ggarrange(p1_heatmap, p2_heatmap, common.legend = TRUE, legend="bottom",
          labels = c("a) Summer Spawning", "b) Spring Spawning" ))

# Attempt with Facetting
combined_settlement_data3X$Spawning_Event <- "a) Spring Spawning"
combined_settlement_data3$Spawning_Event <- "b) Summer Spawning"

tot_dat <- rbind(combined_settlement_data3X, combined_settlement_data3)

pTot_heatmap <- ggplot(tot_dat, aes(Mid_Latitude, Release_Location,  fill = per)) + 
  geom_tile() + theme_classic() + facet_wrap(~Spawning_Event)+
  scale_fill_viridis_c(option = "plasma", na.value=NA, name=bquote("Percent (%)")) +
  scale_y_discrete(limits = rev(levels(combined_settlement_data3$Release_Location)),expand = c(0,0)) +
  scale_x_reverse(breaks=seq(-26,-38,-2), expand = c(0,0))+ xlab("Sink Latitude (°)") + ylab("Source Latitude(°)") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 16),
        strip.background = element_rect(colour = NA),
        legend.justification=c(1,0), legend.position="bottom",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 12, face = "bold"))
pTot_heatmap

ggsave(filename = "Output/Backwards endpoint faceted heatmaps.pdf", width = 21, height = 14.8, units = "cm")
ggsave(filename = "Output/Backwards endpoint faceted heatmaps.png", width = 21, height = 14.8, units = "cm", dpi = 600)
