# R script to get end positions from forward tracking without mortality applied.
# Run on UNSW Katana HPC

library(dplyr)
#library(lemon)
library(data.table)
library(tools)


ID <- as.integer(Sys.getenv('PBS_ARRAYID'))

#file_names <- list.files("katana output/2019 Run/Katana Output/Mortality_Applied/Daily_Latitude", pattern=".rds")
file_names <- list.files("../../srv/scratch/z3374139/Forward/", pattern=".rds")
file_names2 <- paste("../../srv/scratch/z3374139/Forward/", file_names, sep="")

save_names <- file_path_sans_ext(file_names)
save_names2 <- paste0("../../srv/scratch/z3374139/Forward_endpoints/", save_names, sep="")
save_names3 <- paste0("../../srv/scratch/z3374139/Forward_endpoints/Shelf/", save_names, sep="")


file_names2[ID]
#for(i in 1:length(file_names2)){
mydata <- readRDS(file_names2[ID])
 #Subset total data to set degree days for each particle to get finish location. Note this may miss particles that died early
mydata2 <- subset(mydata, degree_days >= 500)
mydata <- subset(mydata, degree_days <= 526) # gives path up to death
 
#mydata2 <- subset(mydata2, Month == "Aug" | Month == "Sep" | Month == "Oct" | Month == "Nov" | Month == "Dec")
mydata2 <- na.omit(mydata2)
mydata2 <- mydata2 %>% group_by(Particle) %>% top_n(-1, degree_days) # final position all particles
 
mydata3 <- subset(mydata2, Bathymetry <= 200) # final position on shelf only
mydata3 <- na.omit(mydata3)

saveRDS(mydata2, file = paste(save_names2[ID], "_endpoints.rds", sep=""))
saveRDS(mydata3, file = paste(save_names3[ID], "_endpoints_shelf.rds", sep=""))
   

paste("THIS SCRIPT IS FINISHED")

