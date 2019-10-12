# Script for combining backwards traking output files into a single file
# Run on UNSW Katana HPC

library(dplyr)
#library(lemon)

file_names <- list.files("../../srv/scratch/z3374139/Back/Output/", pattern=".rds")

# Empty output table generation
combined_data <- data.frame()

for(i in 1:length(file_names)){
  mydat <- readRDS(paste("../../srv/scratch/z3374139/Back/Output/",
                          file_names[i], sep = ""))
  combined_data <- bind_rows(combined_data, mydat)
}


saveRDS(combined_data, "../../srv/scratch/z3374139/Combined_Backwards.rds")

