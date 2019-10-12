# Final distribution plots
# Run on UNSW Katana HPC

library(ncdf4)
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


full_data_endpoints <- readRDS("../../srv/scratch/z3374139/Combined Mortality_endpoints_ALL.rds")
#full_data_endpoints <- readRDS("Combined Mortality_endpoints_ALL.rds")

#full_data_endpoints <- subset(full_data_endpoints, Year == 2010)

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

# # Plots showing particle position on settlement day by month. Note Australia may have a funny shape because of the differing axes scales.
# total_data2 <- subset(full_data_endpoints, Release_Location <= -28.5)
# total_data3 <- subset(total_data2, Month == 2 | Month == 3)
# 
# # late summer southern plot
# # g3 <- ggplot(total_data3) + coord_map("mercator") + 
# #   geom_bin2d(data = total_data3, aes(x = Longtitude, y = Latitude, fill = ..density..), binwidth=1) + 
# #   theme_classic() + scale_fill_distiller(palette= rev("Spectral"), limits = c(0.0000001,0.15), breaks = c(0.001, 0.05, 0.15, 0.25),
# #                                          guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE), trans = "sqrt") + #, trans = "log" (in fill_distiller())
# #   geom_point(aes(x = 153.7955, y = -28.5), size = 2, col = "black") +
# #   geom_point(aes(x = 153.779, y = -29), size = 2, col = "black") +
# #   geom_point(aes(x = 153.7062, y = -29.5), size = 2, col = "black") +
# #   geom_point(aes(x = 153.5131, y = -30), size = 2, col = "black") +
# #   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30") + #+ facet_wrap(~Month)
# #   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
# #         axis.text.x  = element_text(colour="black", size = 16), 
# #         axis.title.y = element_text(face="bold", colour="black", size = 20),
# #         axis.text.y  = element_text(colour="black", size = 16)) +
# #   geom_path(data=shelf, aes(x=Var1, y = Var2))
# # g3
# 
# g3 <- ggplot(total_data3, aes(x = Longtitude, y = Latitude)) +
#   #stat_density_2d(aes(fill = stat((nlevel))), geom = "polygon") + # nlevel is normalising each subplot to a maximum of 1 for the particle density
#   #geom_point(alpha=0.01)+
#   stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) +
#   geom_path(data=bounds, aes(x=Long, y = Lat, colour = "white"), show.legend = TRUE) +
#   geom_point(aes(x = 153.7955, y = -28.5), size = 2, col = "black") +
#   geom_point(aes(x = 153.779, y = -29), size = 2, col = "black") +
#   geom_point(aes(x = 153.7062, y = -29.5), size = 2, col = "black") +
#   geom_point(aes(x = 153.5131, y = -30), size = 2, col = "black") +
#   geom_path(data=shelf, aes(x=Var1, y = Var2)) +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30") +
#   scale_fill_viridis_c(option = "plasma", trans = "sqrt") + theme_classic() +
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
#         axis.text.x  = element_text(colour="black", size = 16), 
#         axis.title.y = element_text(face="bold", colour="black", size = 20),
#         axis.text.y  = element_text(colour="black", size = 16),
#         strip.text = element_text(colour="black", face = "bold")) # + coord_map("mercator")
# g3
# 
# ggsave(filename = "Output/Late summer NSW release combined_not scaled.pdf", width = 8, height = 8)
# ggsave(filename = "Output/Late summer NSW release combined_not scaled.png", width = 8, height = 8, dpi = 600)
# 
# 
# # Plots showing particle position on settlement day by month. Note Australia may have a funny shape because of the differing axes scales.
# total_data2 <- subset(full_data_endpoints, Release_Location <= -28.5)
# total_data3 <- subset(total_data2, Month == 8| Month == 9 | Month == 10 | Month == 11 | Month == 12)
# 
# # spring southern plot
# # g2 <- ggplot(total_data3) + coord_map("mercator") + 
# #   geom_bin2d(data = total_data3, aes(x = Longtitude, y = Latitude, fill = ..density..), binwidth=1) + 
# #   theme_classic() + scale_fill_distiller(palette= rev("Spectral"), limits = c(0.0000001,0.15), breaks = c(0.001, 0.05, 0.15, 0.25),
# #                                          guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE), trans = "sqrt") + #, trans = "log" (in fill_distiller())
# #   geom_point(aes(x = 153.7955, y = -28.5), size = 2, col = "black") +
# #   geom_point(aes(x = 153.779, y = -29), size = 2, col = "black") +
# #   geom_point(aes(x = 153.7062, y = -29.5), size = 2, col = "black") +
# #   geom_point(aes(x = 153.5131, y = -30), size = 2, col = "black") +
# #   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30") + #+ facet_wrap(~Month)
# #   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
# #         axis.text.x  = element_text(colour="black", size = 16), 
# #         axis.title.y = element_text(face="bold", colour="black", size = 20),
# #         axis.text.y  = element_text(colour="black", size = 16)) +
# #   geom_path(data=shelf, aes(x=Var1, y = Var2))
# # g2
# 
# g2 <- ggplot(total_data3, aes(x = Longtitude, y = Latitude)) +
#   #stat_density_2d(aes(fill = stat((nlevel))), geom = "polygon") + # nlevel is normalising each subplot to a maximum of 1 for the particle density
#   #geom_point(alpha=0.01)+
#   stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) +
#   geom_path(data=bounds, aes(x=Long, y = Lat, colour = "white"), show.legend = TRUE) +
#   geom_point(aes(x = 153.7955, y = -28.5), size = 2, col = "black") +
#   geom_point(aes(x = 153.779, y = -29), size = 2, col = "black") +
#   geom_point(aes(x = 153.7062, y = -29.5), size = 2, col = "black") +
#   geom_point(aes(x = 153.5131, y = -30), size = 2, col = "black") +
#   geom_path(data=shelf, aes(x=Var1, y = Var2)) +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30") +
#   scale_fill_viridis_c(option = "plasma", trans = "sqrt") + theme_classic() +
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
#         axis.text.x  = element_text(colour="black", size = 16), 
#         axis.title.y = element_text(face="bold", colour="black", size = 20),
#         axis.text.y  = element_text(colour="black", size = 16),
#         strip.text = element_text(colour="black", face = "bold")) #+ coord_map("mercator")
# g2
# 
# ggsave(filename = "Output/Spring NSW release combined_not scaled.pdf", width = 8, height = 8)
# ggsave(filename = "Output/Spring NSW release combined_not scaled.png", width = 8, height = 8, dpi = 600)
# 
# 
# # Plots showing particle position on settlement day by month. Note Australia may have a funny shape because of the differing axes scales.
# total_data2 <- subset(full_data_endpoints, Release_Location > -28)
# total_data3 <- subset(total_data2, Month == 8| Month == 9 | Month == 10 | Month == 11 | Month == 12)
# 
# # # spring QLD plot
# # g1 <- ggplot(total_data3) + coord_map("mercator") + 
# #   geom_bin2d(data = total_data3, aes(x = Longtitude, y = Latitude, fill = ..density..), binwidth=1) + 
# #   theme_classic() + scale_fill_distiller(palette= rev("Spectral"), limits = c(0.0000001,0.15), breaks = c(0.001, 0.05, 0.15, 0.25),
# #                                          guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE), trans = "sqrt") + #, trans = "log" (in fill_distiller())
# #   geom_point(aes(x = 153.8072, y = -26), size = 2, col = "black") +
# #   geom_point(aes(x = 153.5873, y = -26.5), size = 2, col = "black") +
# #   geom_point(aes(x = 153.546, y = -27), size = 2, col = "black") +
# #   geom_point(aes(x = 153.6929, y = -27.5), size = 2, col = "black") +
# #   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30") + #+ facet_wrap(~Month)
# #   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
# #         axis.text.x  = element_text(colour="black", size = 16), 
# #         axis.title.y = element_text(face="bold", colour="black", size = 20),
# #         axis.text.y  = element_text(colour="black", size = 16)) +
# #   geom_path(data=shelf, aes(x=Var1, y = Var2))
# # g1
# 
# g1 <- ggplot(total_data3, aes(x = Longtitude, y = Latitude)) +
#   #stat_density_2d(aes(fill = stat((nlevel))), geom = "polygon") + # nlevel is normalising each subplot to a maximum of 1 for the particle density
#   #geom_point(alpha=0.01)+
#   stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) +
#   geom_path(data=bounds, aes(x=Long, y = Lat, colour = "white"), show.legend = TRUE) +
#   geom_point(aes(x = 153.8072, y = -26), size = 2, col = "black") +
#   geom_point(aes(x = 153.5873, y = -26.5), size = 2, col = "black") +
#   geom_point(aes(x = 153.546, y = -27), size = 2, col = "black") +
#   geom_point(aes(x = 153.6929, y = -27.5), size = 2, col = "black") +
#   geom_path(data=shelf, aes(x=Var1, y = Var2)) +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30") +
#   scale_fill_viridis_c(option = "plasma", trans = "sqrt") + theme_classic() +
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
#         axis.text.x  = element_text(colour="black", size = 16), 
#         axis.title.y = element_text(face="bold", colour="black", size = 20),
#         axis.text.y  = element_text(colour="black", size = 16),
#         strip.text = element_text(colour="black", face = "bold")) #+ coord_map("mercator")
# g1
# 
# ggsave(filename = "Output/Spring QLD release combined_not scaled.pdf", width = 8, height = 8)
# ggsave(filename = "Output/Spring QLD release combined_not scaled.png", width = 8, height = 8, dpi = 600)
# 
# 
# # Make Legend
# gleg <- ggplot(total_data3) + coord_map("mercator") + 
#   geom_bin2d(data = total_data3, aes(x = Longtitude, y = Latitude, fill = ..density..), binwidth=1) + 
#   theme_classic() + scale_fill_distiller(name="Density", palette= rev("Spectral"), limits = c(0.0000001,0.15), breaks = c(0.001, 0.05, 0.1),
#                                          guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE), trans = "sqrt") + #, trans = "log" (in fill_distiller())
#   #geom_point(aes(x = 153.89, y = -26), size = 5, col = "red") +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group)) + #+ facet_wrap(~Month)
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
#         axis.text.x  = element_text(colour="black", size = 16), 
#         axis.title.y = element_text(face="bold", colour="black", size = 20),
#         axis.text.y  = element_text(colour="black", size = 16),
#         legend.title = element_text(colour="black", size=16, face="bold"),
#         legend.text = element_text(colour="black", size = 14, face = "bold"))
# gleg


# 
# #Now try to plot 3 plots together
# 
# #leg <- get_legend(gleg)
# p1 <- g1 + guides(fill=FALSE)
# p2 <- g2 + guides(fill=FALSE)
# p3 <- g3 + guides(fill=FALSE)
# 
# 
# all_plot <- plot_grid(p1,p2,p3,leg, labels = c("  A) Spring QLD", "  B) Spring NSW", "C) Late Summer NSW", NULL), ncol=2)
# all_plot
# ggsave("Output/all_plot.pdf", plot = all_plot, height = 9, width = 8, units = "in")
# ggsave("Output/all_plot.png", plot = all_plot, height = 9, width = 8, units = "in", dpi = 400)
# 
# paste("This script finished with mortality applied")

# ### Now with No Mortality ###
# 
# full_data_endpointsX <- readRDS("../../srv/scratch/z3374139/Combined_No_Mortality_endpoints.rds")
# 
# # Plots showing particle position on settlement day by month. Note Australia may have a funny shape because of the differing axes scales.
# total_data2X <- subset(full_data_endpointsX, Release_Location <= -28.5)
# total_data3X <- subset(total_data2X, Month == 2 | Month == 3)
# 
# # late summer southern plot
# g3X <- ggplot(total_data3X) + coord_map("mercator") + 
#   geom_bin2d(data = total_data3X, aes(x = Longtitude, y = Latitude, fill = ..density..), binwidth=1) + 
#   theme_classic() + scale_fill_distiller(name="Density",palette= rev("Spectral"), limits = c(0.0000001,0.15), breaks = c(0.001, 0.05, 0.15, 0.25),
#                                          guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE), trans = "sqrt") + #, trans = "log" (in fill_distiller())
#   geom_point(aes(x = 153.7955, y = -28.5), size = 2, col = "black") +
#   geom_point(aes(x = 153.779, y = -29), size = 2, col = "black") +
#   geom_point(aes(x = 153.7062, y = -29.5), size = 2, col = "black") +
#   geom_point(aes(x = 153.5131, y = -30), size = 2, col = "black") +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30") + #+ facet_wrap(~Month)
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
#         axis.text.x  = element_text(colour="black", size = 16), 
#         axis.title.y = element_text(face="bold", colour="black", size = 20),
#         axis.text.y  = element_text(colour="black", size = 16)) +
#   geom_path(data=shelf, aes(x=Var1, y = Var2))
# g3X
# 
# ggsave(filename = "Output/Late summer NSW release combined_No Mortality.pdf", width = 8, height = 8)
# 
# # Plots showing particle position on settlement day by month. Note Australia may have a funny shape because of the differing axes scales.
# total_data2X <- subset(full_data_endpointsX, Release_Location <= -28.5)
# total_data3X <- subset(total_data2X, Month == 8| Month == 9 | Month == 10 | Month == 11 | Month == 12)
# 
# # late summer southern plot
# g2X <- ggplot(total_data3X) + coord_map("mercator") + 
#   geom_bin2d(data = total_data3X, aes(x = Longtitude, y = Latitude, fill = ..density..), binwidth=1) + 
#   theme_classic() + scale_fill_distiller(name="Density",palette= rev("Spectral"), limits = c(0.00000001,0.15), breaks = c(0.001, 0.05, 0.15, 0.25),
#                                          guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE), trans = "sqrt") + #, trans = "log" (in fill_distiller())
#   geom_point(aes(x = 153.7955, y = -28.5), size = 2, col = "black") +
#   geom_point(aes(x = 153.779, y = -29), size = 2, col = "black") +
#   geom_point(aes(x = 153.7062, y = -29.5), size = 2, col = "black") +
#   geom_point(aes(x = 153.5131, y = -30), size = 2, col = "black") +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30") + #+ facet_wrap(~Month)
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
#         axis.text.x  = element_text(colour="black", size = 16), 
#         axis.title.y = element_text(face="bold", colour="black", size = 20),
#         axis.text.y  = element_text(colour="black", size = 16)) +
#   geom_path(data=shelf, aes(x=Var1, y = Var2))
# g2X
# 
# ggsave(filename = "Output/Spring NSW release combined_No_Mortality.pdf", width = 8, height = 8)
# 
# 
# # Plots showing particle position on settlement day by month. Note Australia may have a funny shape because of the differing axes scales.
# total_data2X <- subset(full_data_endpointsX, Release_Location > -28)
# total_data3X <- subset(total_data2X, Month == 8| Month == 9 | Month == 10 | Month == 11 | Month == 12)
# 
# # spring southern plot
# g1X <- ggplot(total_data3X) + coord_map("mercator") + 
#   geom_bin2d(data = total_data3X, aes(x = Longtitude, y = Latitude, fill = ..density..), binwidth=1) + 
#   theme_classic() + scale_fill_distiller(name="Density",palette= rev("Spectral"), limits = c(0.00000001,0.15), breaks = c(0.001, 0.05, 0.15, 0.25),
#                                          guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE), trans = "sqrt") + #, trans = "log" (in fill_distiller())
#   geom_point(aes(x = 153.8072, y = -26), size = 2, col = "black") +
#   geom_point(aes(x = 153.5873, y = -26.5), size = 2, col = "black") +
#   geom_point(aes(x = 153.546, y = -27), size = 2, col = "black") +
#   geom_point(aes(x = 153.6929, y = -27.5), size = 2, col = "black") +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30") + #+ facet_wrap(~Month)
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
#         axis.text.x  = element_text(colour="black", size = 16), 
#         axis.title.y = element_text(face="bold", colour="black", size = 20),
#         axis.text.y  = element_text(colour="black", size = 16)) +
#   geom_path(data=shelf, aes(x=Var1, y = Var2))
# g1X
# 
# ggsave(filename = "Output/Spring QLD release combined_No_Mortality.pdf", width = 8, height = 8)
# 
# # Make Legend
# glegX <- ggplot(total_data3X) + coord_map("mercator") + 
#   geom_bin2d(data = total_data3X, aes(x = Longtitude, y = Latitude, fill = ..density..), binwidth=1) + 
#   theme_classic() + scale_fill_distiller(name="Density", palette= rev("Spectral"), limits = c(0.00000001,0.15), breaks = c(0.001, 0.05, 0.1),
#                                          guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE), trans = "sqrt") + #, trans = "log" (in fill_distiller())
#   #geom_point(aes(x = 153.89, y = -26), size = 5, col = "red") +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group)) + #+ facet_wrap(~Month)
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
#         axis.text.x  = element_text(colour="black", size = 16), 
#         axis.title.y = element_text(face="bold", colour="black", size = 20),
#         axis.text.y  = element_text(colour="black", size = 16),
#         legend.title = element_text(colour="black", size=16, face="bold"),
#         legend.text = element_text(colour="black", size = 14, face = "bold"))
# glegX
# 
# 
# 
# #Now try to plot 3 plots together
# 
# legX <- get_legend(glegX)
# p1X <- g1X + guides(fill=FALSE)
# p2X <- g2X + guides(fill=FALSE)
# p3X <- g3X + guides(fill=FALSE)
# 
# 
# all_plotNM <- plot_grid(p1X,p2X,p3X,legX, labels = c("  A) Spring QLD", "  B) Spring NSW", "C) Late Summer NSW", NULL), ncol=2)
# all_plotNM
# ggsave("Output/all_plot_no_mortality.pdf", plot = all_plotNM, height = 9, width = 8, units = "in")
# ggsave("Output/all_plot_no_mortality.png", plot = all_plotNM, height = 9, width = 8, units = "in", dpi = 400)
# 
# ### Now to subtract 1 from the other and plot it ###
# 
# # Late Summer NSW with mortality
# g3_data <- ggplot_build(g3)
# g3_data2 <- as.data.frame(g3_data$data[[1]])
# 
# # Spring NSW with mortality
# g2_data <- ggplot_build(g2)
# g2_data2 <- as.data.frame(g2_data$data[[1]])
# 
# # Spring QLD with mortality
# g1_data <- ggplot_build(g1)
# g1_data2 <- as.data.frame(g1_data$data[[1]])
# 
# # Late Summer NSW without mortality
# g3X_data <- ggplot_build(g3X)
# g3X_data2 <- as.data.frame(g3X_data$data[[1]])
# 
# # Spring NSW without mortality
# g2X_data <- ggplot_build(g2X)
# g2X_data2 <- as.data.frame(g2X_data$data[[1]])
# 
# # Spring QLD without mortality
# g1X_data <- ggplot_build(g1X)
# g1X_data2 <- as.data.frame(g1X_data$data[[1]])
# 
# # Calculate differences
# diff_dat3 <- full_join(g3X_data2, g3_data2, by = c("PANEL", "xmin", "ymin", "xmax", "ymax"), suffix=c("N", "M"))
# 
# diff_dat3$densityM[is.na(diff_dat3$densityM)] <- 0
# 
# diff_dat3 <- mutate(diff_dat3, Density_Difference = (densityN - densityM))
# head(diff_dat3)
# 
# g3XD <- ggplot(diff_dat3)  + 
#   coord_fixed(1.3) + coord_map("mercator") + 
#   geom_rect(data = diff_dat3, aes(xmin = xmin, ymin = ymin, xmax =xmax, ymax=ymax, fill = Density_Difference)) + 
#   theme_classic() + scale_fill_distiller(name="Density Difference",palette= rev("Spectral"), limits = c(-0.015,0.01), breaks = c(-0.015, -0.01, -0.05, 0, 0.05, 0.01),
#                                          guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE)) +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30") +
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
#         axis.text.x  = element_text(colour="black", size = 16), 
#         axis.title.y = element_text(face="bold", colour="black", size = 20),
#         axis.text.y  = element_text(colour="black", size = 16),
#         legend.title = element_text(colour="black", size=16, face="bold"),
#         legend.text = element_text(colour="black", size = 14, face = "bold"))
# g3XD
# 
# diff_dat2 <- full_join(g2X_data2, g2_data2, by = c("PANEL", "xmin", "ymin", "xmax", "ymax"), suffix=c("N", "M"))
# 
# diff_dat2$densityM[is.na(diff_dat2$densityM)] <- 0
# 
# diff_dat2 <- mutate(diff_dat2, Density_Difference = (densityN - densityM))
# head(diff_dat2)
# 
# g2XD <- ggplot(diff_dat2)  + 
#   coord_fixed(1.3) + coord_map("mercator") + 
#   geom_rect(data = diff_dat2, aes(xmin = xmin, ymin = ymin, xmax =xmax, ymax=ymax, fill = Density_Difference)) + 
#   theme_classic() + scale_fill_distiller(name="Density Difference",palette= rev("Spectral"), limits = c(-0.015,0.01), breaks = c(-0.015, -0.01, -0.05, 0, 0.05, 0.01),
#                                          guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE)) +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30") +
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
#       axis.text.x  = element_text(colour="black", size = 16), 
#       axis.title.y = element_text(face="bold", colour="black", size = 20),
#       axis.text.y  = element_text(colour="black", size = 16),
#       legend.title = element_text(colour="black", size=16, face="bold"),
#       legend.text = element_text(colour="black", size = 14, face = "bold"))
# g2XD
# 
# diff_dat1 <- full_join(g1X_data2, g1_data2, by = c("PANEL", "xmin", "ymin", "xmax", "ymax"), suffix=c("N", "M"))
# 
# diff_dat1$densityM[is.na(diff_dat1$densityM)] <- 0
# 
# diff_dat1 <- mutate(diff_dat1, Density_Difference = (densityN - densityM))
# head(diff_dat1)
# 
# g1XD <- ggplot(diff_dat1)  + 
#   coord_fixed(1.3) + coord_map("mercator") + 
#   geom_rect(data = diff_dat1, aes(xmin = xmin, ymin = ymin, xmax =xmax, ymax=ymax, fill = Density_Difference)) + 
#   theme_classic() + scale_fill_distiller(name="Density Difference",palette= rev("Spectral"), limits = c(-0.015,0.01), breaks = c(-0.015, -0.01, -0.05, 0, 0.05, 0.01),
#                                          guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE)) +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray30") +
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
#         axis.text.x  = element_text(colour="black", size = 16), 
#         axis.title.y = element_text(face="bold", colour="black", size = 20),
#         axis.text.y  = element_text(colour="black", size = 16),
#         legend.title = element_text(colour="black", size=16, face="bold"),
#         legend.text = element_text(colour="black", size = 14, face = "bold"))
# g1XD
# 
# legXD <- get_legend(g1XD)
# p1XD <- g1XD + guides(fill=FALSE)
# p2XD <- g2XD + guides(fill=FALSE)
# p3XD <- g3XD + guides(fill=FALSE)
# 
# all_plotX <- plot_grid(p1XD,p2XD,p3XD,legXD, labels = c("  A) Spring QLD", "  B) Spring NSW", "C) Late Summer NSW", NULL), ncol=2)
# all_plotX
# ggsave("Output/all_plot_mortality_difference.pdf", plot = all_plotX, height = 9, width = 8, units = "in")
# ggsave("Output/all_plot_mortality_difference.png", plot = all_plotX, height = 9, width = 8, units = "in", dpi = 400)


####
full_data_endpoints <- subset(full_data_endpoints, Release_Month != 1)
full_data_endpoints <- subset(full_data_endpoints, Release_Location != -28)

table(full_data_endpoints$Release_Location, full_data_endpoints$Release_Month)


full_data_endpoints$Spawning_Event <- "Other"
full_data_endpoints$Spawning_Event[full_data_endpoints$Release_Location < -28 & full_data_endpoints$Release_Month < 4] <- "Summer NSW"
full_data_endpoints$Spawning_Event[full_data_endpoints$Release_Location < -28 & full_data_endpoints$Release_Month > 7] <- "Spring NSW"
full_data_endpoints$Spawning_Event[full_data_endpoints$Release_Location > -28 & full_data_endpoints$Release_Month > 7] <- "Spring QLD"

table(full_data_endpoints$Spawning_Event)

full_data_endpoints <- subset(full_data_endpoints, Spawning_Event != "Other")
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
locations <- levels(mini_dat$Spawning_Event)

# make empty list
rast_list <- list()

# make raster for each level of release location
for (i in 1:length(locations)){
  rast <- raster(ncol = 100, nrow = 150, ) # raster resolution
  extent(rast) <- extent(mini_dat) #set extent of raster
  datX <- subset(mini_dat, Spawning_Event == locations[i]) # select one release site at a time
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
plt.dat$variable <- str_replace(plt.dat$variable, "\\.", " ")
plt.dat$variable <- as.factor(plt.dat$variable)
plt.dat$Spawning_Event <- plt.dat$variable
str(plt.dat)


variable_names <- list(
  "Spring NSW" = "Mid-latitude Spring" ,
  "Spring QLD" = "Northern Spring",
  "Summer NSW" = "Mid-latitude Summer" 
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

p1 <- ggplot() + geom_tile(data =plt.dat, aes(x=x,y=y,fill=value)) + 
  facet_wrap(~Spawning_Event, nrow = 2,labeller=variable_labeller) +  
  theme_classic() + 
  labs(x=expression(paste("Longitude (",degree, ")", sep="")), y=expression(paste("Latitude (", degree, ")"))) +
  scale_fill_viridis_c(option = "plasma", trans = "log10", na.value=NA, breaks = c(0.01,0.1,1,10,50),
                       name=bquote("Larvae" ~(km^-2)), limits = c(0.01,50), oob=squish,
                       guide = guide_colourbar(draw.ulim = FALSE, draw.llim = FALSE)) + 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
    #scale_fill_distiller(palette = "Spectral", na.value=NA, trans="log10") +
  coord_map() + # coord_quickmap() + #  # this line could be very slow
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
        legend.justification=c(1,0), legend.position=c(0.94,0.07),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 12, face = "bold"))+
  geom_point(data = dots, aes(x = Longitude, y = Latitude), size = 1.5, col = "white", shape=21, fill = "black", stroke =1)

p1


# g1 <- ggplot(full_data_endpoints, aes(x = Longtitude, y = Latitude)) +
#   #stat_density_2d(aes(fill = stat((level))), geom = "polygon") + # nlevel is normalising each subplot to a maximum of 1 for the particle density
#   #geom_point(alpha=0.01) +
#   stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) +
#   geom_path(data=bounds, aes(x=Long, y = Lat), show.legend = TRUE, colour = "white") +
#   geom_path(data=shelf, aes(x=Var1, y = Var2)) + labs(x="Longitude (°)", y="Latitude (°)") +
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray40", colour = "black") +
#   geom_point(data = dots, aes(x = Longitude, y = Latitude), size = 1, col = "white", shape =1) +
#   scale_fill_viridis_c(option = "plasma", trans = "sqrt",  limits = c(0,0.4), oob=squish) + theme_classic() + # , breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
#   facet_wrap(~Spawning_Event, ncol = 2, scales = "free") +
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
#               axis.text.x  = element_text(colour="black", size = 14), 
#               axis.title.y = element_text(face="bold", colour="black", size = 18),
#               axis.text.y  = element_text(colour="black", size = 14),
#               strip.text = element_text(colour="black", face = "bold"),
#               legend.justification=c(1,0), legend.position=c(0.7, 0.2))
# g1

ggsave(filename = "Output/Faceted_distribution_Final.pdf", width = 8, height = 8)
ggsave(filename = "Output/Faceted_distribution_Final.png", width = 8, height = 8, dpi = 600)

