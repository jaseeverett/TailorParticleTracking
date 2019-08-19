# Study site map for particle tracking
library(sp)
library(raster)
library(ggplot2)
library(rgdal)

Aus <- readOGR(dsn = "Shape files/australia",layer = "cstauscd_r")
plot(Aus)
Aus_coast <- subset(Aus, FEAT_CODE != "sea")
#plot(Aus_coast)

# limits of coastline to subset
min_lon <- 145
max_lon <- 155
min_lat <- -45
max_lat <- -24

geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)

Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))

coordinates(Sites.grid) <- ~ lon_bound + lat_bound

Aus_crop <- crop(Aus_coast, extent(Sites.grid)) #rgeos must be installed to run

# Add continential shelf
shelf <- read.csv("Hayden_200m_contour.csv", header = T)
shelf <- subset(shelf, Var2 >= -45)
shelf <- subset(shelf, Var2 <= -24)
shelf <- subset(shelf, Var1 >= 145)

# check it works
plot(Aus_crop)
lines(shelf)

# sites
dots <- read.csv("Release Sites_all.csv", header = T)
#model boundary - note this goes wider than the original subset of the coastline
bounds <- read.csv("ROMS Boundaries.csv", header = T)


p1 <- ggplot() + 
  geom_path(data=bounds, aes(x=Long, y = Lat), show.legend = FALSE, colour = "black", lty = "dashed") +
  geom_polygon(data = Aus_crop, aes(x=long, y=lat, group=group), fill="grey40", colour = "black") +
  geom_point(data = dots, aes(x = Longitude, y = Latitude, shape = Type), cex=2) +
  geom_path(data=shelf, aes(x=Var1, y = Var2)) +
  scale_shape_manual(values = c(21, 17)) +
  coord_map(projection = "mercator") +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x="Longitude (°)", y="Latitude (°)") +
  theme_classic() +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 16),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 16),
        axis.text.y  = element_text(colour="black", size = 14),
        legend.justification=c(1,0), legend.position=c(0.8,0.01),
        legend.title=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA))

p1

# The site names were added after exporting - if you reexport you need to do again.
#ggsave(filename = "Output/Site Map_updated.pdf", width = 14.8, height = 21, units = "cm")
##ggsave(filename = "Output/Site Map.png", width = 14.8, height = 21, units = "cm", dpi = 600)

