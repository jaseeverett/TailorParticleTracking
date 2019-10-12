# Script to calculate settlement in latitude bins from each spawning event

library(dplyr)
library(tidyr)
library(stringr)

#total_data2 <- readRDS("katana output/Combined Mortality_endpoints_ALL.rds")
total_data2 <- readRDS("katana output/Combined_No_Mortality_endpoints.rds")

total_data2 <- subset(total_data2, Release_Month != 1 & Release_Month != 4)
total_data2 <- subset(total_data2, Release_Location != -28)

table(total_data2$Release_Location, total_data2$Release_Month)


total_data2$Spawning_Event <- "Other"
total_data2$Spawning_Event[total_data2$Release_Location < -28 & total_data2$Release_Month < 4] <- "Summer NSW"
total_data2$Spawning_Event[total_data2$Release_Location < -28 & total_data2$Release_Month > 7] <- "Spring NSW"
total_data2$Spawning_Event[total_data2$Release_Location > -28 & total_data2$Release_Month > 7] <- "Spring QLD"

table(total_data2$Spawning_Event)

total_data2 <- subset(total_data2, Spawning_Event != "Other")
table(total_data2$Spawning_Event)

total_data2 <- subset(total_data2, Bathymetry <=200)


### Now try to bin into settlement latitudes
total_data2$Settlement_Lat <- cut(total_data2$Latitude, breaks = seq(-43,-23, 1))
#plot(total_data2$Settlement_Lat)
#binned <- hist(total_data2$Latitude)
#data.frame(binned$counts, binned$mids)
settlement_summary <- total_data2 %>% group_by(Spawning_Event, Settlement_Lat) %>% summarise(Count = n())
settlement_summary


combined_settlement_data <- settlement_summary

# This now makes better lat column boundaries and orders the rows by latitude
combined_settlement_data <- separate(data = combined_settlement_data, into = c("Max_Latitude", "Min_Latitude"), col = Settlement_Lat, sep=",")
combined_settlement_data$Max_Latitude <- as.numeric( str_sub(combined_settlement_data$Max_Latitude, start=2))
combined_settlement_data$Min_Latitude <- as.numeric( str_sub(combined_settlement_data$Min_Latitude, start=1, end =3))
combined_settlement_data

table(total_data2$Spawning_Event)
#combined_settlement_data <- combined_settlement_data[order(-combined_settlement_data$Max_Latitude),]

#write.csv(combined_settlement_data, "Output/Test.csv", row.names = FALSE)
write.csv(combined_settlement_data, "Output/Test_No_Mortality.csv", row.names = FALSE)


table(total_data2$Dead)


# by year too...

total_data2 <- readRDS("katana output/Combined Mortality_endpoints_ALL.rds")
total_data2 <- subset(total_data2, Release_Month != 1 & Release_Month != 4)
total_data2 <- subset(total_data2, Release_Location != -28)

table(total_data2$Release_Location, total_data2$Release_Month)


total_data2$Spawning_Event <- "Other"
total_data2$Spawning_Event[total_data2$Release_Location < -28 & total_data2$Release_Month < 4] <- "Summer NSW"
total_data2$Spawning_Event[total_data2$Release_Location < -28 & total_data2$Release_Month > 7] <- "Spring NSW"
total_data2$Spawning_Event[total_data2$Release_Location > -28 & total_data2$Release_Month > 7] <- "Spring QLD"

table(total_data2$Spawning_Event)

total_data2 <- subset(total_data2, Spawning_Event != "Other")
table(total_data2$Spawning_Event)

total_data2 <- subset(total_data2, Bathymetry <=200)


### Now try to bin into settlement latitudes
total_data2$Settlement_Lat <- cut(total_data2$Latitude, breaks = seq(-43,-23, 1))
#plot(total_data2$Settlement_Lat)
#binned <- hist(total_data2$Latitude)
#data.frame(binned$counts, binned$mids)

# Financial Year
library(lubridate)
library(data.table)

fiscal_start_month = 7


total_data2[, Fiscal_Year := ifelse(month(Time) >= fiscal_start_month, year(Time) + 1, year(Time))]

settlement_summary <- total_data2 %>% group_by(Spawning_Event, Fiscal_Year, Settlement_Lat) %>% summarise(Count = n())
head(settlement_summary)


combined_settlement_data <- settlement_summary

# This now makes better lat column boundaries and orders the rows by latitude
combined_settlement_data <- separate(data = combined_settlement_data, into = c("Max_Latitude", "Min_Latitude"), col = Settlement_Lat, sep=",")
combined_settlement_data$Max_Latitude <- as.numeric( str_sub(combined_settlement_data$Max_Latitude, start=2))
combined_settlement_data$Min_Latitude <- as.numeric( str_sub(combined_settlement_data$Min_Latitude, start=1, end =3))
head(combined_settlement_data)

table(total_data2$Spawning_Event)
#combined_settlement_data <- combined_settlement_data[order(-combined_settlement_data$Max_Latitude),]

write.csv(combined_settlement_data, "Output/Test2.csv", row.names = FALSE)
