# Script to make example degree days and path plot

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
library(ggpubr)

total_data <- readRDS("D:/Reproduction/katana output/2019 Run/Daily/2003_Lat-30.0_For_with_mortality.rds")

total_data <- subset(total_data, degree_days <=530)

str(total_data)

single <- subset(total_data, Particle =="2003_Lat-30.0_For.155678")
single <- subset(total_data, Particle =="2003_Lat-30.0_For.214756")


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








g <- ggplot(total_data) + geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "grey60")  +  
  geom_path(data = single, aes(x = Longtitude, y = Latitude, col=Temperature),  size = 1) + theme_classic() + coord_map("mercator") +
  coord_cartesian(xlim = c(145, 160), ylim = c(-20, -40), expand = TRUE)

g

p1 <- ggplot(total_data) +   geom_path(data = single, aes(x = Longtitude, y = Latitude, col=Temperature),  size = 1) +
  theme_classic() + 
  labs(x=expression(paste("Longitude (",degree, ")", sep="")), y=expression(paste("Latitude (", degree, ")"))) +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  #scale_fill_distiller(palette = "Spectral", na.value=NA, trans="log10") +
  coord_map() + #coord_quickmap() + #  # this line could be very slow
  geom_path(data=bounds, aes(x=Long, y = Lat), colour = "black", lty="dashed", show.legend = FALSE) +
  geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray40", colour = "black")+
  #geom_point(data = dots, aes(x = Longitude, y = Latitude), size = 2, col = "black")+
  geom_path(data=shelf, aes(x=Var1, y = Var2)) + 
  scale_color_gradient(low="blue", high = "red")+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14),
        strip.background = element_rect(colour = "white"),
        legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"))

p1

head(single)

p2 <- ggplot(single, aes(DayofLife, degree_days, col = Temperature)) + geom_line(size=2) + theme_classic() +
      scale_color_gradient(low="blue", high = "red") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14),
        strip.background = element_rect(colour = "white"),
        legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold")) +
  ylab("Degree-days") + xlab("Days")
p2

ggarrange(p1, p2, 
          labels = "auto",
          ncol = 2, label.y = 1, label.x = 0, common.legend = TRUE, widths = c(1.5,1))


ggsave("Output/Degree-day real example Plot.pdf", width = 21, height = 14.8, units = "cm")
ggsave("Output/Degree-day real example Plot.png", width = 21, height = 14.8, units = "cm")

# How to identify particle to plot
dat <- total_data %>% group_by(Particle) %>% summarise(Temp_range = (max(Temperature) -min(Temperature)))
dat <- sort(dat, Temp_range)
