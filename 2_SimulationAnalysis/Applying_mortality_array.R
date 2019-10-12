# R script for taking R converted PARCELS outputs and applying mortality
# Run using UNSW Katana HPC

library(dplyr)
library(lubridate)
library(ggplot2)
library(tools)

# Applying mortality to the PARCELS output
# Take the combined file made in Katana/R
# Outputs a combined file where some particles die each day.

ID <- as.integer(Sys.getenv('PBS_ARRAYID'))
#years = as.character(seq(1994, 2015, 1))

file_names <- list.files("../../srv/scratch/z3374139/Forward/", pattern=".rds")
file_names2 <- paste("../../srv/scratch/z3374139/Forward/", file_names, sep="")
#file_names2

save_names <- file_path_sans_ext(file_names)
save_names2 <- paste0("../../srv/scratch/z3374139/Forward_with_Mortality/", save_names, sep="")

file_names2[ID]
#for(i in 1:length(file_names2)){
mydata <- readRDS(file_names2[ID])

### Practice code  - used to select only 2 cohorts ###
#mydata2 <- subset(mydata, DayofLife == 0)
#mydata2$Year <- year(mydata2$Time)
#mydata2 <- subset(mydata2, Release_Location == -30)
#mydata2 <- subset(mydata2, time == 1944000| time == 2030400)
#mydata <- mydata %>% filter(Particle %in% mydata2$Particle)



# Using a value of M = 0.25)
M = 0.25
actual_mortality = 1 - exp(-M) # what proportion die each day (finite mortality)
actual_mortality


# So all particles make it to day 15 therefore mortality applies from day 16, 
# one particle appears to have hit 30 days

# so for each day after 15 delete actual_mortality proportion from that day and all after days

# Therefore on each day make list of particles, randomly select x % of them and remove from future

mydata$Dead <- "Alive"
mydata$Cohort <- as.factor(mydata$Cohort)

mydata_beginning <- subset(mydata, DayofLife < 16)
mydata_end <- subset(mydata, DayofLife > 15)

for (j in (levels(mydata_end$Cohort))) {
  cohort <- subset(mydata_end, mydata_end$Cohort == j)
  for (i in (16:max(mydata_end$DayofLife))){
  datX <- subset(cohort, DayofLife== i & Dead == "Alive")
  particle_list = as.data.frame(unique(datX$Particle)) # list all live particles
  die_particles = (sample_frac(particle_list, size = actual_mortality)) # selects a percentage of particles from list
  mydata_end$Longtitude[mydata_end$DayofLife  >= i & mydata_end$Particle %in% die_particles$`unique(datX$Particle)`] <- NA # removes position of dead particles
  mydata_end$Temperature[mydata_end$DayofLife  >= i & mydata_end$Particle %in% die_particles$`unique(datX$Particle)`] <- NA # removes temperature of dead particles
  mydata_end$Latitude[mydata_end$DayofLife  >= i & mydata_end$Particle %in% die_particles$`unique(datX$Particle)`] <- NA #  removes position of dead particles
  mydata_end$Dead[mydata_end$DayofLife  >= i & mydata_end$Particle %in% die_particles$`unique(datX$Particle)`] <- "Dead" # label as dead
  
  }
}

mydata <- bind_rows(mydata_beginning, mydata_end)
# Now sort data by particle and day of life columns
mydata <- mydata[with(mydata, order(Particle, DayofLife)), ]

summary_dat <- mydata_end %>% group_by(DayofLife, Dead, Cohort) %>% summarise(n = n())
write.csv(summary_dat, file = paste(save_names2[ID], "_mortality_summary.csv", sep=""), row.names = F)

saveRDS(mydata, file = paste(save_names2[ID], "_with_mortality.rds", sep=""))

#saveRDS(mydata, "../../srv/scratch/z3374139/1994 forward with mortality.rds")
#write.csv(mydata, "../../srv/scratch/z3374139/1994 forward with mortality.csv", row.names = F)


#p1 <- ggplot(summary_dat, aes(DayofLife, n, col = Dead)) + geom_line() + theme_classic() +facet_wrap(~Cohort)
#p1

paste("This script finished")