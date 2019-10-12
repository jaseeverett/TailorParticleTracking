# Script to make Latitude Settlement Graphs (proportions)

library(tidyr)
library(ggplot2)


mydata <- read.csv("Settlement percentages plotting.csv", header = T)

dat <- gather(mydata, Event, Percentage, c(2:4))
head(dat) #check its ok

dat$Event <- factor(dat$Event, levels = c("Summer.NSW", "Spring.NSW", "Spring.QLD" ))

ggplot(dat, aes(x=Latitude,y=Percentage,fill=Event)) + geom_area(position="fill") +
  ylab("Proportion of Settling Larvae") + xlab("Latitude (°)") +theme_classic() + 
  scale_fill_manual(values = c("grey80", "grey50", "grey20"),
                    name="Spawning Event",
                  breaks=c("Spring.QLD", "Spring.NSW", "Summer.NSW"),
                  labels=c("Northern Spring", "Mid-latitude Spring", "Mid-latitude Summer")) +
  theme(axis.text=element_text(size=14, face = "bold", colour = "black")) + coord_flip() +
  scale_x_continuous(expand = c(0,0), breaks = seq(-40,-25, 2)) + scale_y_continuous(expand = c(0,0)) +
  theme(axis.title = element_text(face="bold", colour="black", size = 16),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom") #+


ggsave(filename = "Output/Settlement proportions by latitude.pdf", width = 21, height = 14.8, units = "cm")
ggsave(filename = "Output/Settlement proportions by latitude.png", width = 21, height = 14.8, units = "cm", dpi = 600)


mydata <- read.csv("Settlement percentages plotting no mortality.csv", header = T)

dat <- gather(mydata, Event, Percentage, c(2:4))
head(dat) # check its ok

dat$Event <- factor(dat$Event, levels = c("Summer.NSW", "Spring.NSW", "Spring.QLD" ))

ggplot(dat, aes(x=Latitude,y=Percentage,fill=Event)) + geom_area(position="fill") +
  ylab("Proportion of Settling Larvae") + xlab("Latitude (°)") +theme_classic() + 
  scale_fill_manual(values = c("grey80", "grey50", "grey20"),
                    name="Spawning Event",
                    breaks=c("Spring.QLD", "Spring.NSW", "Summer.NSW"),
                    labels=c("Northern Spring", "Mid-latitude Spring", "Mid-latitude Summer")) +
  coord_flip() +
  scale_x_continuous(expand = c(0,0), breaks = seq(-40,-25, 2)) + scale_y_continuous(expand = c(0,0)) +
  theme(axis.title = element_text(face="bold", colour="black", size = 16),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        axis.text=element_text(size=14, face = "bold", colour = "black")) #+


ggsave(filename = "Output/Settlement proportions by latitude no mortality.pdf", width = 21, height = 14.8, units = "cm")
ggsave(filename = "Output/Settlement proportions by latitude no mortality.png", width = 21, height = 14.8, units = "cm", dpi = 600)
