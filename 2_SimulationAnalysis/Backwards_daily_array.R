# R script for UNSW katana HPC

### This scripts takes the parcels output files and merges them into one long file for R

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

ID <- as.integer(Sys.getenv('PBS_ARRAYID'))

file_names <- list.files("../../srv/scratch/z3374139/Back/", pattern=".nc")
file_names2 <- paste("../../srv/scratch/z3374139/Back/", file_names, sep="")
#file_names2

save_names <- file_path_sans_ext(file_names)
save_names2 <- paste0("../../srv/scratch/z3374139/Back/Output/", save_names, sep="")

file_names2[ID]
#for(i in 1:length(file_names2)){
mydata <- nc_open(file_names2[ID])

  #mydata <- nc_open("../../srv/scratch/z3374139/Back/Tailor_Parcels_Output_Year1994_Back.nc")
  
  # Get lat and long and check dimensions
  lon <- ncvar_get(mydata,"lon")
  nlon <- dim(lon)
  #head(lon)
  lat <- ncvar_get(mydata,"lat")
  nlat <- dim(lat)
  #head(lat)
  #print(head(c(nlon,nlat)))
  
  
  #Get time and time units and check time dimensions
  time <- ncvar_get(mydata,"time")
  #time
  tunits <- ncatt_get(mydata,"time","units")
  #tunits
  nt <- dim(time)
  #nt
  
  
  
  #Get the the variable (temp) and its attributes from the netcdf File, and verify the size of #the array.
  temp_array <- ncvar_get(mydata,"temp")
  dlname <- ncatt_get(mydata,"temp","long_name")
  dunits <- ncatt_get(mydata,"temp","units")
  fillvalue <- ncatt_get(mydata,"temp","_FillValue") #fills empty cells with specified fill value (NaN)
  dim(temp_array)
  
  temp_df <- as.data.frame.array(temp_array)
  head(temp_df)
  colnames(temp_df) <- paste("Particle", 1:ncol(temp_df)) #name columns for each particle
  rownames(temp_df) <- paste(0:(nrow(temp_df)-1)) # name rows for each time step
  temp_df$DayofLife <- as.numeric(as.character(rownames(temp_df))) 
  ### now try to make long data
  long_temp_df <- melt(temp_df,id.vars = "DayofLife", variable.name= "Particle", value.name = "Temperature")
  head(long_temp_df)
  
  # Get the the variable (lat) and its attributes, and verify the size of the array.
  # Then make it into a long format datatable
  lat_array <- ncvar_get(mydata,"lat")
  dlname <- ncatt_get(mydata,"lat","long_name")
  dunits <- ncatt_get(mydata,"lat","units")
  fillvalue <- ncatt_get(mydata,"lat","_FillValue")
  dim(lat_array)
  lat_df <- as.data.frame.array(lat_array)
  head(lat_df)
  colnames(lat_df) <- paste("Particle", 1:ncol(lat_df)) #name columns for each particle
  rownames(lat_df) <- paste(0:(nrow(lat_df)-1)) # name rows for each time step
  lat_df$DayofLife <- as.numeric(as.character(rownames(lat_df))) 
  ### now try to make long data
  long_lat_df <- melt(lat_df,id.vars = "DayofLife", variable.name= "Particle", value.name = "Latitude")
  head(long_lat_df)
  
  
  # Get the the variable (long) and its attributes, and verify the size of the array.
  
  long_array <- ncvar_get(mydata,"lon")
  dlname <- ncatt_get(mydata,"lon","long_name")
  dunits <- ncatt_get(mydata,"lon","units")
  fillvalue <- ncatt_get(mydata,"lon","_FillValue")
  dim(long_array)
  long_df <- as.data.frame.array(long_array)
  head(long_df)
  colnames(long_df) <- paste("Particle", 1:ncol(long_df))
  rownames(long_df) <- paste(0:(nrow(long_df)-1))
  long_df$DayofLife <- as.numeric(as.character(rownames(long_df)))
  ### now try to make long data
  long_long_df <- melt(long_df,id.vars = "DayofLife", variable.name= "Particle", value.name = "Longtitude")
  head(long_long_df)
  
  ### Get the Bathymetry layer
  bathy_array <- ncvar_get(mydata,"bathy")
  dlname <- ncatt_get(mydata,"bathy","long_name")
  dunits <- ncatt_get(mydata,"bathy","units")
  fillvalue <- ncatt_get(mydata,"bathy","_FillValue")
  dim(bathy_array)
  bathy_df <- as.data.frame.array(bathy_array)
  head(bathy_df)
  colnames(bathy_df) <- paste("Particle", 1:ncol(bathy_df))
  rownames(bathy_df) <- paste(0:(nrow(bathy_df)-1))
  bathy_df$DayofLife <- as.numeric(as.character(rownames(bathy_df)))
  ### now try to make long data
  long_bathy_df <- melt(bathy_df,id.vars = "DayofLife", variable.name= "Particle", value.name = "Bathymetry")
  head(long_bathy_df)
  
  # Merge Lat/Long Data Frames
  
  total_data <- merge(long_long_df ,long_lat_df ,by=c("Particle","DayofLife"))
  head(total_data)
  
  # Merge Lat/Long with Temp Data Frames
  total_data <- merge(total_data ,long_temp_df ,by=c("Particle","DayofLife"))
  head(total_data)
  
  # Merge Lat/Long with Bathy Data Frames
  total_data <- merge(total_data ,long_bathy_df ,by=c("Particle","DayofLife"))
  head(total_data)
  
  
  # Make Time variable
  time_array <- ncvar_get(mydata,"time")
  dlname <- ncatt_get(mydata,"time","long_name")
  dunits <- ncatt_get(mydata,"time","units")
  fillvalue <- ncatt_get(mydata,"time","_FillValue")
  dim(time_array)
  time_df <- as.data.frame.array(time_array)
  head(time_df)
  colnames(time_df) <- paste("Particle", 1:ncol(time_df))
  rownames(time_df) <- paste(0:(nrow(time_df)-1))
  time_df$DayofLife <- as.numeric(as.character(rownames(time_df))) ### now try to make long data
  long_time_df <- melt(time_df,id.vars = "DayofLife", variable.name= "Particle", value.name = "time")
  head(long_time_df)
  
  time_origin <- mydata$var$time$units
  time_origin2 <- str_sub(time_origin, 15, 24)
  time_origin3 <- str_sub(time_origin, 26, 33)
  time_origin <- paste(time_origin2, " ", time_origin3)
  time_origin
  
  # Merge total and Data Frames with time and remove NA values
  total_data <- merge(total_data ,long_time_df ,by=c("Particle","DayofLife"))
  head(total_data)
  total_data <- na.omit(total_data)
  
  
  #Get the global attributes. The attributes can be listed, buy simply typing an attribute name at the command line. varid = 0 is the global file reference. Not really sure why it's needed but was in the tutorial
  NCProperties <- ncatt_get(mydata,0,"_NCProperties")
  feature_type <- ncatt_get(mydata,0,"feature_type")
  ncei_template_version <- ncatt_get(mydata,0,"ncei_template_version")
  parcels_mesh <- ncatt_get(mydata,0,"parcels_mesh")
  Conventions <- ncatt_get(mydata,0,"Conventions")
  
  
  #Close NetCDF file - saves data from being lost
  nc_close(mydata)
  
  
  # convert time from seconds since origin. Orgin is specified in the units for time. This probably needs to be automated as the origin may change next time.
  total_data$Time <- as.POSIXct(total_data$time, origin = time_origin) # from info in netCDF file
  total_data$Month <- month(total_data$Time) #, label = TRUE
  head(total_data)
  
  # Now sort data by particle and day of life columns
  total_data <- total_data[with(total_data, order(Particle, DayofLife)), ]
  
  is.na(total_data$Temperature) <- total_data$Temperature == 0 # change 0 to NA
  total_data$Temperature <- na.locf(total_data$Temperature, na.rm=FALSE) # replace NA with last non-NA observation carried forward
  total_data$Temperature <- na.locf(total_data$Temperature, na.rm=FALSE, fromLast = TRUE)
  
  # Calculate cumlative temperature (degree days) for each particle
  total_data <- total_data %>% group_by(Particle) %>% mutate(degree_days = cumsum(Temperature))
  head(total_data)
  
  # Remove Particle word and make into a deciminal number
  total_data$Particle <- as.character(total_data$Particle)
  total_data$Particle <- removeWords(total_data$Particle, "Particle ") 
  total_data$Particle <- paste0(ID,".", total_data$Particle )
  total_data$Particle <- as.factor(total_data$Particle)
  
  total_data$Release_Location <- NA
  total_data$Release_Month <- NA
  total_data$Cohort <- NA
  
  for (i in 1:nrow(total_data)) {
    if (total_data$DayofLife[i] == 0) {
      total_data$Release_Location[i] <- total_data$Latitude[i]
      total_data$Release_Month[i] <- total_data$Month[i]
      total_data$Cohort[i] <- paste(total_data$Latitude[i], total_data$time[i]) 
    }
  }
  
  total_data$Release_Location <- na.locf(total_data$Release_Location, na.rm=FALSE) # replace NA with last non-NA observation carried forward
  total_data$Release_Month <- na.locf(total_data$Release_Month, na.rm=FALSE) # replace NA with last non-NA observation carried forward
  total_data$Cohort <- na.locf(total_data$Cohort, na.rm=FALSE)
  
  #Add file name as a category
#  total_data$Release_Location <- i
  
  #Subset total data to set degree days for each particle to get finish location. Note this may miss particles that died early
  
#  total_data2 <- subset(total_data, degree_days >= 500)
#  total_data <- subset(total_data, degree_days <= 526) # gives path up to death
  
  #total_data2 <- subset(total_data2, Month == "Aug" | Month == "Sep" | Month == "Oct" | Month == "Nov" | Month == "Dec")
#  total_data2 <- na.omit(total_data2)
#  total_data2 <- total_data2 %>% group_by(Particle) %>% top_n(-1, degree_days) # final position all particles
  
#  total_data3 <- subset(total_data2, Bathymetry <= 200) # final position on shelf only
#  total_data3 <- na.omit(total_data3)
  
  # add dataframes to list
#  list_full_data[[i]] <- total_data
  
#  list_full_data_endpoints[[i]] <- total_data2
#  list_shelf_data_endpoints[[i]] <- total_data3
#}

#full_release_region_data <- rbindlist(list_full_data)
#full_release_region_data_endpoints <- rbindlist(list_full_data_endpoints)
#full_release_shelf_endpoints <- rbindlist(list_shelf_data_endpoints)
# Stop the clock
#proc.time() - ptm

saveRDS(total_data, file = paste(save_names2[ID], ".rds", sep=""))
  
#saveRDS(total_data, "../../srv/scratch/z3374139/Backwards Combined release dataframe_daily 1994.rds")
#saveRDS(full_release_region_data_endpoints, "Output/Combined release dataframe_endpoints.rds")
#saveRDS(full_release_shelf_endpoints, "Output/Combined release shelf_endpoints.rds")

#gc()
#rm(list=ls())


#full_data <- readRDS("../../srv/scratch/z3374139/Combined release dataframe_daily.rds")
#shelf_data <- readRDS("Output/Combined release shelf_endpoints.rds")
#full_data_endpoints <- readRDS("Output/Combined release dataframe_endpoints.rds")

#write.csv(total_data, "../../srv/scratch/z3374139/Backwards Combined release dataframe_daily 1994.csv", row.names = F)
#write.csv(shelf_data, "Output/Combined release dataframe_shelf_endpoints.csv", row.names = F)
#write.csv(full_data_endpoints, "Output/Combined release dataframe_endpoints.csv", row.names = F)

paste("This script finished")