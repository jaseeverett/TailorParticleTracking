# Script To get endpoints of backwards particles
# Run on UNSW Katana HPC

library(dplyr)

mydata <- readRDS("../../srv/scratch/z3374139/Combined_Backwards.rds")

total_data2 <- subset(mydata, degree_days >= 500)
total_data2 <- na.omit(total_data2)
total_data2 <- total_data2 %>% group_by(Particle) %>% top_n(-1, degree_days)


saveRDS(total_data2, file = "../../srv/scratch/z3374139/Backwards_endpoints.rds")
