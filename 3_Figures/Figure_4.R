# Script to make Figure 4 (settlement distance)

# Plots for Moninya

# Stats of release numbers and settlement
library(ggplot2)
library(scales)
library(dplyr)

#full_data_endpoints <- readRDS("../../srv/scratch/z3374139/Combined Mortality_endpoints_ALL.rds")
full_data_endpoints <- readRDS("katana output/Combined Mortality_endpoints_ALL.rds")
table(full_data_endpoints$Dead)

####
full_data_endpoints <- subset(full_data_endpoints, Release_Month != 1)
full_data_endpoints <- subset(full_data_endpoints, Release_Location != -28)

table(full_data_endpoints$Release_Location, full_data_endpoints$Release_Month)


full_data_endpoints$Spawning_Event <- "Other"
full_data_endpoints$Spawning_Event[full_data_endpoints$Release_Location < -28 & full_data_endpoints$Release_Month < 4] <- "Mid-latitude Summer"
full_data_endpoints$Spawning_Event[full_data_endpoints$Release_Location < -28 & full_data_endpoints$Release_Month > 7] <- "Mid-latitude Spring"
full_data_endpoints$Spawning_Event[full_data_endpoints$Release_Location > -28 & full_data_endpoints$Release_Month > 7] <- "Northern Spring"

# Drop "Other"
full_data_endpoints <- subset(full_data_endpoints, Spawning_Event != "Other")

# number surviving
table(full_data_endpoints$Spawning_Event)

# number on shelf
full_data_endpoints_shelf <- subset(full_data_endpoints, Bathymetry <= 200)
table(full_data_endpoints_shelf$Release_Location)
full_data_endpoints_shelf$Release_Long <- 0
full_data_endpoints_shelf$Release_Long[full_data_endpoints_shelf$Release_Location == -26] <- 153.8072
full_data_endpoints_shelf$Release_Long[full_data_endpoints_shelf$Release_Location == -26.5] <- 153.5873
full_data_endpoints_shelf$Release_Long[full_data_endpoints_shelf$Release_Location == -27] <- 153.5460
full_data_endpoints_shelf$Release_Long[full_data_endpoints_shelf$Release_Location == -27.5] <- 153.6929
full_data_endpoints_shelf$Release_Long[full_data_endpoints_shelf$Release_Location == -28.5] <- 153.7955
full_data_endpoints_shelf$Release_Long[full_data_endpoints_shelf$Release_Location == -29] <- 153.7790
full_data_endpoints_shelf$Release_Long[full_data_endpoints_shelf$Release_Location == -29.5] <- 153.7062
full_data_endpoints_shelf$Release_Long[full_data_endpoints_shelf$Release_Location == -30] <- 153.5131

table(full_data_endpoints_shelf$Release_Long)
table(full_data_endpoints_shelf$Release_Year)

str(full_data_endpoints_shelf)

library(geosphere)
#test <- distm( c(153.8072, -26), c(153.5873, -26.5), fun = distHaversine)/1000 # result is in km
full_data_endpoints_shelf$Dist_straight_line <- 0

# This is very slow
for (i in 1:nrow(full_data_endpoints_shelf)) {
  full_data_endpoints_shelf$Dist_straight_line[i] <- 
    distm( c(full_data_endpoints_shelf$Release_Long[i],full_data_endpoints_shelf$Release_Location[i]),
           c(full_data_endpoints_shelf$Longtitude[i], full_data_endpoints_shelf$Latitude[i]), fun = distHaversine)/1000
}

saveRDS(full_data_endpoints_shelf, "katana output/Combined Mortality_endpoints_shelf_with_dists.rds")


### Start from here

mydata <- readRDS("katana output/Combined Mortality_endpoints_shelf_with_dists.rds")



table(mydata$Release_Location, mydata$Release_Month)
table(mydata$Spawning_Event, mydata$Release_Month)

### Now try to bin into settlement latitudes
mydata$Settlement_Lat <- cut(mydata$Latitude, breaks = seq(-43,-23, 1))
#plot(total_data2$Settlement_Lat)
#binned <- hist(total_data2$Latitude)
#data.frame(binned$counts, binned$mids)

# Financial Year
library(lubridate)
library(data.table)

fiscal_start_month = 7


mydata[, Fiscal_Year := ifelse(month(Time) >= fiscal_start_month, year(Time) + 1, year(Time))]

settlement_summary <- mydata %>% group_by(Fiscal_Year, Spawning_Event, Settlement_Lat) %>% summarise(n = n()) %>%
  mutate(freq = n / sum(n) *100)
head(settlement_summary)

# check sum of one group
sum(settlement_summary$freq[1:9])


settlement_summary_average <- settlement_summary %>% group_by(Spawning_Event, Settlement_Lat) %>% 
  summarise(Settlement = mean(freq), Settlement_sd = sd(freq), n = n(), Settlement_se = Settlement_sd/(sqrt(n)))

head(settlement_summary_average)

combined_settlement_data <- settlement_summary_average

# This now makes better lat column boundaries and orders the rows by latitude
library(tidyr)
library(stringr)
combined_settlement_data <- separate(data = combined_settlement_data, into = c("Max_Latitude", "Min_Latitude"), col = Settlement_Lat, sep=",")
combined_settlement_data$Max_Latitude <- as.numeric( str_sub(combined_settlement_data$Max_Latitude, start=2))
combined_settlement_data$Min_Latitude <- as.numeric( str_sub(combined_settlement_data$Min_Latitude, start=1, end =3))
head(combined_settlement_data)

combined_settlement_data <- combined_settlement_data %>% group_by(Spawning_Event, Max_Latitude) %>%
  mutate(Latitude = mean(c(Max_Latitude, Min_Latitude)))
head(combined_settlement_data)

table(combined_settlement_data$Spawning_Event)
#combined_settlement_data <- combined_settlement_data[order(-combined_settlement_data$Max_Latitude),]

#write.csv(combined_settlement_data, "Output/Test2.csv", row.names = FALSE)


# Make plot
p1 <- ggplot(combined_settlement_data, aes(x = Latitude, y = Settlement, col = Spawning_Event)) + geom_point() +
  geom_line() + geom_errorbar(aes(ymin = Settlement - Settlement_se, ymax = Settlement + Settlement_se)) +
  theme_classic() + xlab("Latitude (°)") + ylab("Settlement (% ± SE)") + coord_flip() +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 12, face = "bold"),
        legend.position = "none",
        axis.text=element_text(size=12, face = "bold", colour = "black")) +
  scale_colour_manual(values = c("grey80", "grey50", "grey20"),
                      name="Spawning Event",
                      breaks=c("Northern Spring", "Mid-latitude Spring", "Mid-latitude Summer"))
  
p1


#### Now try settlement as a % of total combined spawning event settlement (equivalent to Table 2?)
mydata <- readRDS("katana output/Combined Mortality_endpoints_shelf_with_dists.rds")



table(mydata$Release_Location, mydata$Release_Month)
table(mydata$Spawning_Event, mydata$Release_Month)

### Now try to bin into settlement latitudes
mydata$Settlement_Lat <- cut(mydata$Latitude, breaks = seq(-43,-23, 1))
#plot(total_data2$Settlement_Lat)
#binned <- hist(total_data2$Latitude)
#data.frame(binned$counts, binned$mids)

# Financial Year
library(lubridate)
library(data.table)

fiscal_start_month = 7


mydata[, Fiscal_Year := ifelse(month(Time) >= fiscal_start_month, year(Time) + 1, year(Time))]

# gives percentage settlement of overall annual settlement
settlement_summary2 <- mydata %>% group_by(Fiscal_Year, Spawning_Event, Settlement_Lat) %>% summarise(n = n()) %>%
  group_by(Fiscal_Year) %>% mutate(freq = n / sum(n) *100)

# Check it worked
sum(settlement_summary2$freq[1:30])

settlement_summary_average2 <- settlement_summary2 %>% group_by(Spawning_Event, Settlement_Lat) %>% 
  summarise(Settlement = mean(freq), Settlement_sd = sd(freq), n = n(), Settlement_se = Settlement_sd/(sqrt(n)))

head(settlement_summary_average2)

combined_settlement_data2 <- settlement_summary_average2

# This now makes better lat column boundaries and orders the rows by latitude
library(tidyr)
library(stringr)
combined_settlement_data2 <- separate(data = combined_settlement_data2, into = c("Max_Latitude", "Min_Latitude"), col = Settlement_Lat, sep=",")
combined_settlement_data2$Max_Latitude <- as.numeric( str_sub(combined_settlement_data2$Max_Latitude, start=2))
combined_settlement_data2$Min_Latitude <- as.numeric( str_sub(combined_settlement_data2$Min_Latitude, start=1, end =3))
head(combined_settlement_data)

combined_settlement_data2 <- combined_settlement_data2 %>% group_by(Spawning_Event, Max_Latitude) %>%
  mutate(Latitude = mean(c(Max_Latitude, Min_Latitude)))
head(combined_settlement_data2)

table(combined_settlement_data2$Spawning_Event)
# Make plot
p2 <- ggplot(combined_settlement_data2, aes(x = Latitude, y = Settlement, col = Spawning_Event), alpha = 0.5) + geom_point() +
  geom_line() + geom_errorbar(aes(ymin = Settlement - Settlement_se, ymax = Settlement + Settlement_se)) +
  theme_classic() + xlab("Latitude (°)") + ylab("Settlement (% ± SE)") + coord_flip() +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        legend.position = c(0.6, 0.2),
        axis.text=element_text(size=12, face = "bold", colour = "black")) +
  scale_colour_manual(values = c("grey60", "grey40", "grey20"),
                      name="Spawning Event",
                      breaks=c("Northern Spring", "Mid-latitude Spring", "Mid-latitude Summer"))


p2


# Combine plots 
#install.packages("ggpubr")
library(ggpubr)

ggarrange(p1, p2, 
          labels = c("a) Within Spawning Events ", "b) Combined Settlement"),
          ncol = 2, label.y = 1, label.x = 0)


ggsave("Output/Settlement Density Plots.pdf", width = 21, height = 14.8, units = "cm")
ggsave("Output/Settlement Density Plots.png", width = 21, height = 14.8, units = "cm", dpi = 600)


### Now do plots by distance
mydata <- readRDS("katana output/Combined Mortality_endpoints_shelf_with_dists.rds")



table(mydata$Release_Location, mydata$Release_Month)
table(mydata$Spawning_Event, mydata$Release_Month)

summary(mydata$Dist_straight_line)

### Now try to bin into settlement latitudes
mydata$Distance <- cut(mydata$Dist_straight_line, breaks = seq(0,1500, 50))
#plot(total_data2$Settlement_Lat)
#binned <- hist(total_data2$Latitude)
#data.frame(binned$counts, binned$mids)

# Financial Year
library(lubridate)
library(data.table)
library(tidyr)
library(stringr)

fiscal_start_month = 7


mydata[, Fiscal_Year := ifelse(month(Time) >= fiscal_start_month, year(Time) + 1, year(Time))]

settlement_summary <- mydata %>% group_by(Fiscal_Year, Spawning_Event, Distance) %>% summarise(n = n()) %>%
  mutate(freq = n / sum(n) *100)
head(settlement_summary)

# check sum of one group
sum(settlement_summary$freq[1:34])


settlement_summary_average <- settlement_summary %>% group_by(Spawning_Event, Distance) %>% 
  summarise(Settlement = mean(freq), Settlement_sd = sd(freq), n = n(), Settlement_se = Settlement_sd/(sqrt(n)))

combined_settlement_data <- settlement_summary_average

# This now makes better lat column boundaries and orders the rows by latitude
#library(tidyr)
#library(stringr)
combined_settlement_data <- separate(data = combined_settlement_data, into = c("Max_Distance", "Min_Distance"), col = Distance, sep=",")
combined_settlement_data$Max_Distance <- as.numeric( str_sub(combined_settlement_data$Max_Distance, start=2))
combined_settlement_data$Min_Distance <- as.numeric( str_sub(combined_settlement_data$Min_Distance, start=1, end =-2))
head(combined_settlement_data)

combined_settlement_data <- combined_settlement_data %>% group_by(Spawning_Event, Max_Distance) %>%
  mutate(Latitude = mean(c(Max_Distance, Min_Distance)))
head(combined_settlement_data)

table(combined_settlement_data$Spawning_Event)
#combined_settlement_data <- combined_settlement_data[order(-combined_settlement_data$Max_Latitude),]

#write.csv(combined_settlement_data, "Output/Test2.csv", row.names = FALSE)


# Make plot
p1 <- ggplot(combined_settlement_data, aes(x = Latitude, y = Settlement, col = Spawning_Event)) + geom_point() +
  geom_line() + geom_errorbar(aes(ymin = Settlement - Settlement_sd, ymax = Settlement + Settlement_sd)) +
  theme_classic() + xlab("Distance (km)") + ylab("Settlement within each Spawning Event (% ± SD)") +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 12, face = "bold"),
        legend.position = c(0.7,0.8),
        axis.text=element_text(size=12, face = "bold", colour = "black")) +
  scale_colour_manual(values = c("grey80", "grey50", "grey20"),
                      name="Spawning Event",
                      breaks=c("Northern Spring", "Mid-latitude Spring", "Mid-latitude Summer"))

p1

head(settlement_summary_average)

ggsave("Output/Settlement Distance Plot.pdf", width = 21, height = 14.8, units = "cm")
ggsave("Output/Settlement Distance Plot.png", width = 21, height = 14.8, units = "cm", dpi = 600)

## Test differences in distance
str(mydata)
fit <- lm(log10(Dist_straight_line) ~ Year+Spawning_Event, data = mydata)
plot(fit)
summary(fit)
anova(fit)

dat_dist <- mydata %>% group_by(Spawning_Event) %>%
  summarise(dist = mean(Dist_straight_line), n = n(), sd = sd(Dist_straight_line), se = sd/sqrt(n))
dat_dist
