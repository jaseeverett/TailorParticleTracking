# Code to make settlement histograms

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
full_data_endpoints$Shelf <- "All Particles"
full_data_endpoints_shelf <- subset(full_data_endpoints, Bathymetry <= 200)
full_data_endpoints_shelf$Shelf <- "Particles on the Continental Shelf"

table(full_data_endpoints_shelf$Spawning_Event)

full_data_plotting <- rbind(full_data_endpoints, full_data_endpoints_shelf)

# Calculate mean settlement day
dat <- full_data_endpoints %>% group_by(Spawning_Event) %>% 
  summarise(mean_settle = mean(DayofLife), SD = sd(DayofLife), n=n(), SE = SD/sqrt(n))
dat

dat2 <- full_data_endpoints_shelf %>% group_by(Spawning_Event) %>% 
  summarise(mean_settle = mean(DayofLife), SD = sd(DayofLife), n=n(), SE = SD/sqrt(n))
dat2

dat3 <- full_data_plotting %>% group_by(Shelf, Spawning_Event) %>% 
  summarise(mean_settle = mean(DayofLife), SD = sd(DayofLife), n=n(), SE = SD/sqrt(n))
dat3

full_data_endpoints$Spawning_Event <- factor(full_data_endpoints$Spawning_Event, 
                                             levels = c("Northern Spring", "Mid-latitude Spring", "Mid-latitude Summer"))

## Histogram of settlement date
g1 <- ggplot(full_data_endpoints, aes(x = DayofLife)) + geom_histogram(binwidth = 1) +
  facet_wrap(~Spawning_Event, ncol = 1) + theme_classic() +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 16),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 16),
        axis.text.y  = element_text(colour="black", size = 12),
        strip.text = element_text(colour="black", face = "bold")) +
  ylab("Number of Larvae") + xlab("Settlement Day") +
  scale_y_continuous(labels = comma) + scale_x_continuous(breaks = c(16,18,20,22,24,26,28,30)) +
  geom_vline(data = dat, aes(xintercept=mean_settle), colour = "red")
g1



ggsave("Particle Tracking Manuscript/Settlement Day histograms.pdf", height = 21, width = 14, units = "cm")
ggsave("Particle Tracking Manuscript/Settlement Day histograms.png", height = 21, width = 14, units = "cm")

# On shelf particles only
g2 <- ggplot(full_data_endpoints_shelf, aes(x = DayofLife)) + geom_histogram(binwidth = 1) +
  facet_wrap(~Spawning_Event, ncol = 1) + theme_classic() +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 16),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 16),
        axis.text.y  = element_text(colour="black", size = 12),
        strip.text = element_text(colour="black", face = "bold")) +
  ylab("Number of Larvae") + xlab("Settlement Day") +
  scale_y_continuous(labels = comma) + scale_x_continuous(breaks = c(16,18,20,22,24,26,28,30)) +
  geom_vline(data = dat2, aes(xintercept=mean_settle), colour = "red")
g2            

ggsave("Particle Tracking Manuscript/Settlement Day histograms_shelf.pdf", height = 21, width = 14, units = "cm")
ggsave("Particle Tracking Manuscript/Settlement Day histograms_shelf.png", height = 21, width = 14, units = "cm")


# On and off the shelf at the same time
g3 <- ggplot(full_data_plotting, aes(x = DayofLife)) + geom_histogram(binwidth = 1) +
  facet_grid(Spawning_Event~Shelf) + theme_classic() +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 16),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 16),
        axis.text.y  = element_text(colour="black", size = 12),
        strip.text = element_text(colour="black", face = "bold"),
        axis.ticks = element_line(colour="black"),
        panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("Number of Larvae") + xlab("Settlement Day") +
  scale_y_continuous(labels = comma, expand = c(0,0)) + scale_x_continuous(breaks = c(16,18,20,22,24,26,28,30)) +
  geom_vline(data = dat3, aes(xintercept=mean_settle), colour = "red") + expand_limits( y = 510000)
g3            

ggsave("Particle Tracking Manuscript/Settlement Day histograms_combined.pdf", height = 14.8, width = 21, units = "cm")
ggsave("Particle Tracking Manuscript/Settlement Day histograms_combined.png", height = 14.8, width = 21, units = "cm", dpi = 600)
