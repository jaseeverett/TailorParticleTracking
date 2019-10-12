# Combines mortality applied endpoints into a single file
# Run on UNSW Katana HPC

library(dplyr)
#library(lemon)
library(data.table)

#file_names <- list.files("katana output/2019 Run/Katana Output/Mortality_Applied/Daily_Latitude", pattern=".rds")
file_names <- list.files("../../srv/scratch/z3374139/Forward_with_Mortality_endpoints/", pattern=".rds")

# Empty output table generation
combined_data <- list()


for(i in 1:length(file_names)){
  #mydat <- readRDS(paste("katana output/2019 Run/Katana Output/Mortality_Applied/Daily_Latitude/",
   mydat <- readRDS(paste("../../srv/scratch/z3374139/Forward_with_Mortality_endpoints/",
                          file_names[i], sep = ""))
  combined_data[[i]] <- mydat
}

combined_data <- rbindlist(combined_data)

saveRDS(combined_data, "../../srv/scratch/z3374139/Combined Mortality_endpoints_ALL.rds")


paste("THIS SCRIPT IS FINISHED")

